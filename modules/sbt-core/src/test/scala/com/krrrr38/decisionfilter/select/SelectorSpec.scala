package com.krrrr38.decisionfilter.select

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Files

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration._

import org.scalatest.funsuite.AnyFunSuite

class SelectorSpec extends AnyFunSuite {

  private final class Repo {
    val dir: File = Files.createTempDirectory("decision-filter-repo").toFile.getCanonicalFile
    private val git = new Git(dir)
    def apply(args: String*): Unit = {
      val r = git.run(args: _*)
      assert(r.ok, s"git ${args.mkString(" ")}: ${r.stderr}")
    }
    def write(path: String, text: String): Unit = {
      val f = new File(dir, path)
      f.getParentFile.mkdirs()
      Files.write(f.toPath, text.getBytes(StandardCharsets.UTF_8))
    }
    apply("init", "-q")
    apply("config", "user.email", "t@example.com")
    apply("config", "user.name", "t")
    apply("config", "commit.gpgsign", "false")
  }

  private def spec(name: String, base: String = "org.scalatest.funsuite.AnyFunSuite") =
    s"""package p
       |class $name extends $base {
       |  test("works") { assert(true) }
       |}
       |""".stripMargin

  /** Classes whose name contains "Affected" are at risk; everything else is unrelated. */
  private final class FakeBackend(
      result: Vector[SuiteQuestion] => Either[BackendError, BackendResult] = qs =>
        Right(
          BackendResult(
            qs.map(q => q.id -> Answer(Some(if (q.suite.suiteId.contains("Affected")) 2.6 else 0.1), Some(0.9))).toMap,
            Usage(100, 5)
          )
        )
  ) extends DecisionBackend {
    val id = "fake"
    val model = "fake-1"
    val maxQuestionsPerCall = 400
    var asked: Vector[SuiteQuestion] = Vector.empty
    var lastState: Option[DiffState] = None
    def judge(state: DiffState, questions: Vector[SuiteQuestion]): Either[BackendError, BackendResult] = {
      asked ++= questions
      lastState = Some(state)
      result(questions)
    }
  }

  private def factory(b: => Either[String, DecisionBackend]): BackendFactory = new BackendFactory {
    val id = "fake"
    val model = "fake-1"
    def create(env: Env): Either[String, DecisionBackend] = b
  }

  private def setup(): Repo = {
    val repo = new Repo
    repo.write("core/src/main/scala/p/Calc.scala", "package p\nobject Calc { def add(a: Int, b: Int) = a + b }\n")
    repo.write("core/src/test/scala/p/UnitSpec.scala", "package p\nabstract class UnitSpec extends org.scalatest.funsuite.AnyFunSuite\n")
    repo.write("core/src/test/scala/p/AffectedSpec.scala", spec("AffectedSpec"))
    repo.write("core/src/test/scala/p/UnrelatedSpec.scala", spec("UnrelatedSpec"))
    repo.write("core/src/test/scala/p/BasedSpec.scala", spec("BasedSpec", "UnitSpec"))
    repo.write("build.sbt", "")
    repo("add", "-A")
    repo("commit", "-q", "-m", "init")
    repo("branch", "base")
    repo
  }

  private def config(repo: Repo, backend: BackendFactory) = SelectConfig(
    buildRoot = repo.dir,
    modules = Seq(ModuleSources("core", Seq(new File(repo.dir, "core/src/test/scala")), "2.13.18")),
    output = new File(repo.dir, "target/decision-filter/selection.json"),
    base = Some("base"),
    gate = GateConfig.Default,
    runAllPatterns = RunAllTrigger.DefaultPatterns,
    redactPatterns = StateBuilder.DefaultRedactPatterns,
    concurrency = 2,
    timeout = 30.seconds,
    dryRun = false,
    backend = backend
  )

  private final class Recording extends Log {
    val infos = ArrayBuffer.empty[String]
    def info(msg: String): Unit = infos += msg
    def debug(msg: String): Unit = ()
  }

  private def changeCalc(repo: Repo): Unit =
    repo.write("core/src/main/scala/p/Calc.scala", "package p\nobject Calc { def add(a: Int, b: Int) = a + b + 0 }\n")

  test("filters by class, from unstaged changes; untracked files are ignored; logs the reduction") {
    val repo = setup()
    changeCalc(repo)
    repo.write("core/src/test/scala/p/UntrackedSpec.scala", spec("UntrackedSpec"))
    val backend = new FakeBackend()
    val log = new Recording
    val doc = new Selector(Env.of(Map.empty), log).run(config(repo, factory(Right(backend))))

    assert(doc.mode == "filter")
    assert(doc.deselected == Set("p.UnrelatedSpec", "p.BasedSpec"))
    assert(backend.asked.map(_.suite.suiteId).sorted == Vector("p.AffectedSpec", "p.BasedSpec", "p.UnrelatedSpec"))
    assert(backend.asked.find(_.suite.suiteId == "p.AffectedSpec").map(_.suite.testNames) == Some(Vector("works")))
    assert(backend.lastState.map(_.changedFiles) == Some(Vector("core/src/main/scala/p/Calc.scala")))
    assert(log.infos == ArrayBuffer(
      "decision-filter: running 1 of 3 test classes (2 skipped) in 1 modules (1 call, 105 tokens)"
    ))
    assert(new File(repo.dir, "target/decision-filter/selection.json").isFile)
  }

  test("a changed test file or base class selects without asking") {
    val repo = setup()
    repo.write("core/src/test/scala/p/UnrelatedSpec.scala", spec("UnrelatedSpec").replace("works", "still works"))
    repo.write("core/src/test/scala/p/UnitSpec.scala", "package p\nabstract class UnitSpec extends org.scalatest.funsuite.AnyFunSuite { }\n")
    val backend = new FakeBackend()
    val doc = new Selector(Env.of(Map.empty), Log.Silent).run(config(repo, factory(Right(backend))))
    assert(doc.verdicts.map(v => v.suite.suiteId -> v.reason).toMap == Map(
      "p.AffectedSpec" -> Reason.Scored,
      "p.UnrelatedSpec" -> Reason.Touched,
      "p.BasedSpec" -> Reason.BaseTouched
    ))
    assert(backend.asked.map(_.suite.suiteId) == Vector("p.AffectedSpec"))
  }

  test("falls back to every test class with an info line, never failing") {
    def run(repo: Repo, b: BackendFactory, f: SelectConfig => SelectConfig = identity): String = {
      val log = new Recording
      val doc = new Selector(Env.of(Map.empty), log).run(f(config(repo, b)))
      assert(doc.mode == "all")
      assert(doc.deselected.isEmpty)
      assert(log.infos.size == 1)
      log.infos.head
    }
    val ok = factory(Right(new FakeBackend()))

    assert(run(setup(), ok) == "decision-filter: running every test class (no diff against base)")

    val build = setup()
    build.write("build.sbt", "// changed")
    assert(run(build, ok) == "decision-filter: running every test class (build.sbt changed)")

    val repo = setup()
    changeCalc(repo)
    assert(run(repo, factory(Left("no API key; set FAKE_KEY"))) ==
      "decision-filter: running every test class (no API key; set FAKE_KEY)")
    assert(run(repo, factory(Right(new FakeBackend(_ => Left(BackendError.Failed("HTTP 529 overloaded")))))) ==
      "decision-filter: running every test class (fake failed: HTTP 529 overloaded)")
    assert(run(repo, factory(Right(new FakeBackend(_ => throw new RuntimeException("boom"))))).contains("boom"))
    assert(run(repo, ok, _.copy(base = Some("no-such-ref"))).contains("cannot resolve merge-base with no-such-ref"))
    assert(run(repo, ok, _.copy(modules = Nil)).contains("no test classes were found"))
    assert(run(repo, ok, _.copy(dryRun = true)).contains("dry run"))
  }

  test("invalid settings fail") {
    val repo = setup()
    val sel = new Selector(Env.of(Map.empty), Log.Silent)
    intercept[InvalidConfig](sel.run(config(repo, factory(Left("x"))).copy(runAllPatterns = Seq("["))))
    intercept[InvalidConfig](sel.run(config(repo, factory(Left("x"))).copy(gate = GateConfig(Double.NaN, 0.5, 1.0))))
  }

  test("a request that is too large is split in halves") {
    val repo = setup()
    changeCalc(repo)
    val backend = new FakeBackend(qs =>
      if (qs.size > 1) Left(BackendError.TooLarge("too big"))
      else Right(BackendResult(qs.map(q => q.id -> Answer(Some(0.0), Some(1.0))).toMap, Usage(10, 1)))
    )
    val doc = new Selector(Env.of(Map.empty), Log.Silent).run(config(repo, factory(Right(backend))))
    assert(doc.mode == "filter")
    assert(doc.spent.splits == 2)
    assert(doc.spent.calls == 5)
    assert(doc.spent.inputTokens == 30)
  }

  test("a truncated diff that deselects most classes runs everything") {
    val repo = setup()
    changeCalc(repo)
    repo.write("core/src/main/scala/p/Big.scala", "x" * 200000)
    repo("add", "-A") // staged, not committed: still part of the diff
    val backend = new FakeBackend(qs => Right(BackendResult(qs.map(q => q.id -> Answer(Some(0.0), Some(1.0))).toMap, Usage.Zero)))
    val doc = new Selector(Env.of(Map.empty), Log.Silent).run(config(repo, factory(Right(backend))))
    assert(doc.mode == "all")
    assert(doc.reason.exists(_.contains("cut to fit")))
  }
}
