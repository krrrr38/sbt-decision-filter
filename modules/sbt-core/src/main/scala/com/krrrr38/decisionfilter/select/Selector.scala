package com.krrrr38.decisionfilter.select

import java.io.File
import java.util.concurrent.{Callable, ExecutorService, Executors, ThreadFactory, TimeUnit}
import java.util.concurrent.atomic.AtomicInteger

import scala.concurrent.duration.FiniteDuration
import scala.util.control.NonFatal
import scala.util.matching.Regex

final case class SelectConfig(
    buildRoot: File,
    modules: Seq[ModuleSources],
    output: File,
    base: Option[String],
    gate: GateConfig,
    runAllPatterns: Seq[String],
    redactPatterns: Seq[String],
    concurrency: Int,
    timeout: FiniteDuration,
    dryRun: Boolean,
    backend: BackendFactory
)

/** Thrown for invalid settings: the only case where selection fails the build. */
final class InvalidConfig(msg: String) extends RuntimeException(msg)

/** The whole selection: diff → triggers → test classes → questions → gate → selection.json.
  *
  * Anything that goes wrong (no API key, API errors, timeouts, a bug here) becomes "run every test class" with an
  * info line saying why, never a build failure.
  */
final class Selector(env: Env, log: Log) {

  private val Prefix = "decision-filter:"

  def run(config: SelectConfig): SelectionDocument = {
    val (trigger, redact) = validate(config)
    val doc =
      try select(config, trigger, redact)
      catch {
        case e: InvalidConfig => throw e
        case NonFatal(e)      => fallback(config, s"selector failed: $e", None, Vector.empty)
      }
    try doc.write(config.output)
    catch { case NonFatal(e) => log.info(s"$Prefix could not write ${config.output}: $e") }
    log.info(summary(doc))
    doc
  }

  private def validate(config: SelectConfig): (RunAllTrigger, Vector[Regex]) = {
    def check(ok: Boolean, msg: => String): Unit = if (!ok) throw new InvalidConfig(msg)
    def finite(d: Double) = !d.isNaN && !d.isInfinite
    check(finite(config.gate.cutoff), s"decisionFilterCutoff must be a number: ${config.gate.cutoff}")
    check(finite(config.gate.unsureBelow), s"decisionFilterUnsureBelow must be a number: ${config.gate.unsureBelow}")
    check(
      finite(config.gate.unsureMargin) && config.gate.unsureMargin >= 0,
      s"decisionFilterUnsureMargin must be a non-negative number: ${config.gate.unsureMargin}"
    )
    check(config.concurrency >= 1, s"decisionFilterConcurrency must be at least 1: ${config.concurrency}")
    check(config.timeout.toMillis > 0, s"decisionFilterTimeout must be positive: ${config.timeout}")
    val trigger = RunAllTrigger(config.runAllPatterns).fold(e => throw new InvalidConfig(e), identity)
    val redact = StateBuilder.compile(config.redactPatterns).fold(e => throw new InvalidConfig(e), identity)
    (trigger, redact)
  }

  private def select(config: SelectConfig, trigger: RunAllTrigger, redact: Vector[Regex]): SelectionDocument = {
    val repoRoot = RepoDiff.topLevel(config.buildRoot) match {
      case Some(r) => r
      case None    => return fallback(config, s"${config.buildRoot} is not inside a git repository", None, Vector.empty)
    }
    val repo = RepoDiff.collect(new Git(repoRoot), config.base, env) match {
      case Right(r) => r
      case Left(e)  => return fallback(config, e, None, Vector.empty)
    }
    val files = DiffParser.parse(repo.diff)
    if (files.isEmpty) return fallback(config, s"no diff against ${repo.base}", Some(repo), Vector.empty)

    val changed = files.flatMap(_.paths).toSet
    trigger.firstMatch(files.flatMap(_.paths)) match {
      case Some((file, _)) => return fallback(config, s"$file changed", Some(repo), Vector.empty)
      case None            => ()
    }

    val extracted = Extractor.extract(config.modules, repoRoot, repo.trackedFiles.contains)
    if (extracted.parseFailures.nonEmpty) {
      log.info(s"$Prefix ${extracted.parseFailures.size} test files could not be parsed; their test classes always run")
      extracted.parseFailures.foreach { case (file, msg) => log.debug(s"$Prefix cannot parse $file: $msg") }
    }
    val suites = extracted.suites
    if (suites.isEmpty) return fallback(config, "no test classes were found", Some(repo), Vector.empty)

    val pre: Vector[(TestSuite, Option[Reason])] = suites.map { s =>
      s -> (
        if (changed.contains(s.file)) Some(Reason.Touched)
        else if (s.baseFiles.exists(changed.contains)) Some(Reason.BaseTouched)
        else None
      )
    }
    val questions = pre.collect { case (s, None) => s }.zipWithIndex.map { case (s, i) => SuiteQuestion(f"q$i%04d", s) }
    val runAll = pre.map { case (s, r) => Verdict(s, selected = true, r.getOrElse(Reason.NotAsked), None) }

    val factory = config.backend
    def doc(mode: String, reason: Option[String], verdicts: Vector[Verdict], spent: Spent) = SelectionDocument(
      mode = mode,
      reason = reason,
      backend = Some(factory.id),
      model = Some(factory.model),
      base = Some(repo.base),
      mergeBase = Some(repo.mergeBase),
      head = Some(repo.head),
      gate = config.gate,
      verdicts = verdicts,
      spent = spent
    )

    if (config.dryRun) return doc(SelectionDocument.ModeAll, Some("dry run"), runAll, Spent.Zero)
    if (questions.isEmpty) return doc(SelectionDocument.ModeFilter, None, runAll, Spent.Zero)

    val backend = factory.create(env) match {
      case Right(b)  => b
      case Left(why) => return doc(SelectionDocument.ModeAll, Some(why), runAll, Spent.Zero)
    }

    val state = StateBuilder.build(files, repo.stat, redact)
    ask(backend, state, questions, config) match {
      case Left(err) => doc(SelectionDocument.ModeAll, Some(s"${backend.id} failed: ${err.message}"), runAll, Spent.Zero)
      case Right((answers, spent)) =>
        val byId = questions.map(q => q.suite -> q.id).toMap
        val verdicts = pre.map {
          case (s, Some(reason)) => Verdict(s, selected = true, reason, None)
          case (s, None)         => Gate.decide(s, byId.get(s).flatMap(answers.get), config.gate)
        }
        val selected = verdicts.count(_.selected)
        if (state.truncated && selected * 2 < verdicts.size)
          doc(
            SelectionDocument.ModeAll,
            Some(
              s"the diff was cut to fit (${state.omittedFiles.size} files left out) and only $selected of " +
                s"${verdicts.size} test classes were selected"
            ),
            verdicts,
            spent
          )
        else doc(SelectionDocument.ModeFilter, None, verdicts, spent)
    }
  }

  /** Batches of `maxQuestionsPerCall`, `concurrency` at a time, within `timeout`. Any batch failing fails all. */
  private def ask(
      backend: DecisionBackend,
      state: DiffState,
      questions: Vector[SuiteQuestion],
      config: SelectConfig
  ): Either[BackendError, (Map[String, Answer], Spent)] = {
    val calls = new AtomicInteger(0)
    val splits = new AtomicInteger(0)

    def askSplitting(qs: Vector[SuiteQuestion]): Either[BackendError, BackendResult] = {
      calls.incrementAndGet()
      backend.judge(state, qs) match {
        case Left(_: BackendError.TooLarge) if qs.size >= 2 =>
          splits.incrementAndGet()
          val (a, b) = qs.splitAt((qs.size + 1) / 2)
          for {
            ra <- askSplitting(a)
            rb <- askSplitting(b)
          } yield BackendResult(ra.answers ++ rb.answers, ra.usage + rb.usage)
        case other => other
      }
    }

    val batches = questions.grouped(math.max(1, backend.maxQuestionsPerCall)).toVector
    val pool = newPool(math.min(config.concurrency, batches.size))
    try {
      val tasks = new java.util.ArrayList[Callable[Either[BackendError, BackendResult]]]()
      batches.foreach { b =>
        tasks.add(new Callable[Either[BackendError, BackendResult]] {
          def call(): Either[BackendError, BackendResult] = askSplitting(b)
        })
      }
      val futures = pool.invokeAll(tasks, config.timeout.toMillis, TimeUnit.MILLISECONDS)
      val results = (0 until futures.size).toVector.map { i =>
        val f = futures.get(i)
        if (f.isCancelled) Left(BackendError.Failed(s"timed out after ${config.timeout}"))
        else
          try f.get()
          catch { case NonFatal(e) => Left(BackendError.Failed(String.valueOf(e.getCause))) }
      }
      results.collectFirst { case Left(e) => e } match {
        case Some(e) => Left(e)
        case None =>
          val ok = results.collect { case Right(r) => r }
          val usage = ok.foldLeft(Usage.Zero)(_ + _.usage)
          Right((ok.flatMap(_.answers).toMap, Spent(calls.get(), usage.inputTokens, usage.outputTokens, splits.get())))
      }
    } finally pool.shutdownNow()
  }

  private def newPool(n: Int): ExecutorService = {
    val counter = new AtomicInteger(0)
    Executors.newFixedThreadPool(
      math.max(1, n),
      new ThreadFactory {
        def newThread(r: Runnable): Thread = {
          val t = new Thread(r, s"decision-filter-${counter.incrementAndGet()}")
          t.setDaemon(true)
          t
        }
      }
    )
  }

  private def fallback(config: SelectConfig, reason: String, repo: Option[RepoDiff], verdicts: Vector[Verdict]) =
    SelectionDocument(
      mode = SelectionDocument.ModeAll,
      reason = Some(reason),
      backend = Some(config.backend.id),
      model = Some(config.backend.model),
      base = repo.map(_.base),
      mergeBase = repo.map(_.mergeBase),
      head = repo.map(_.head),
      gate = config.gate,
      verdicts = verdicts,
      spent = Spent.Zero
    )

  private def summary(doc: SelectionDocument): String =
    if (!doc.isFilter) s"$Prefix running every test class (${doc.reason.getOrElse("unknown reason")})"
    else {
      val total = doc.totalCount
      val running = doc.selectedCount
      val modules = doc.verdicts.filter(_.selected).map(_.suite.module).distinct.size
      val tokens = doc.spent.inputTokens + doc.spent.outputTokens
      val tokenText = if (tokens >= 1000) f"${tokens / 1000.0}%.1fk" else tokens.toString
      val calls = if (doc.spent.calls == 1) "1 call" else s"${doc.spent.calls} calls"
      s"$Prefix running $running of $total test classes (${total - running} skipped) in $modules modules " +
        s"($calls, $tokenText tokens)"
    }
}
