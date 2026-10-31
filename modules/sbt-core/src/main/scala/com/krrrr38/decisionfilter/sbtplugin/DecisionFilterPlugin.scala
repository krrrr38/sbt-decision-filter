package com.krrrr38.decisionfilter.sbtplugin

import java.nio.charset.StandardCharsets
import java.nio.file.Files

import scala.concurrent.duration._
import scala.util.control.NonFatal

import com.krrrr38.decisionfilter.internal.json.Json
import com.krrrr38.decisionfilter.select._
import sbt._
import sbt.Keys._

/** Backend-independent part of DecisionFilter. It only reaches a build through a backend plugin
  * (e.g. sbt-decision-filter-jev), which adds itself to `decisionFilterBackends`.
  *
  *   - `decisionFilterSelect` (task): writes selection.json and returns its path
  *   - `decisionFilterTest` (command): selects, then runs the root's aggregated tests leaving out the deselected
  *     test classes through sbt's `Tests.Filter`
  */
object DecisionFilterPlugin extends AutoPlugin {
  override def trigger = allRequirements
  override def requires = plugins.JvmPlugin

  object autoImport extends DecisionFilterKeys
  import autoImport._

  override def buildSettings: Seq[Setting[_]] = Seq(
    // A fixed place under the root (sbt 2 moves `target` elsewhere), so CI can pick it up as an artifact.
    decisionFilterSelectionFile := (LocalRootProject / baseDirectory).value / "target" / "decision-filter" / "selection.json",
    decisionFilterBase := None,
    decisionFilterCutoff := GateConfig.Default.cutoff,
    decisionFilterUnsureBelow := GateConfig.Default.unsureBelow,
    decisionFilterUnsureMargin := GateConfig.Default.unsureMargin,
    decisionFilterRunAllPatterns := RunAllTrigger.DefaultPatterns,
    decisionFilterRedactPatterns := StateBuilder.DefaultRedactPatterns,
    decisionFilterConcurrency := 8,
    decisionFilterTimeout := 5.minutes,
    decisionFilterDryRun := false,
    decisionFilterBackends := Nil,
    decisionFilterSelect := SbtCompat.selectTask.value,
    decisionFilterSelect / aggregate := false
  )

  override def projectSettings: Seq[Setting[_]] = Seq(
    // Empty unless decisionFilterTest is running, so a plain `test` is never filtered.
    Test / testOptions ++= SbtCompat.filterOptions.value,
    commands += testCommand
  )

  // -----------------------------------------------------------------------------------------------------------------

  /** Test classes to leave out, present on the State only while decisionFilterTest runs the tests. */
  private[sbtplugin] val activeDeselection = AttributeKey[Set[String]]("decisionFilterDeselection")

  private[sbtplugin] def filterOptionsFor(state: State): Seq[TestOption] =
    state.get(activeDeselection).toSeq.map(deselected => Tests.Filter(name => !deselected.contains(name)))

  private[sbtplugin] final case class Inputs(
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
      backends: Seq[BackendFactory]
  )

  /** Called from the version-specific task definition in [[SbtCompat]]. */
  private[sbtplugin] def runSelect(inputs: Inputs, log: Logger): File = {
    val backend = inputs.backends match {
      case Seq(one) => one
      case Seq() =>
        throw new MessageOnlyException("decision-filter: no backend; add a plugin such as sbt-decision-filter-jev")
      case many =>
        throw new MessageOnlyException(
          s"decision-filter: more than one backend plugin is enabled (${many.map(_.id).mkString(", ")}); keep one"
        )
    }
    val config = SelectConfig(
      buildRoot = inputs.buildRoot,
      modules = inputs.modules,
      output = inputs.output,
      base = inputs.base,
      gate = inputs.gate,
      runAllPatterns = inputs.runAllPatterns,
      redactPatterns = inputs.redactPatterns,
      concurrency = inputs.concurrency,
      timeout = inputs.timeout,
      dryRun = inputs.dryRun,
      backend = backend
    )
    try new Selector(Env.System, new SbtLog(log)).run(config)
    catch { case e: InvalidConfig => throw new MessageOnlyException(s"decision-filter: ${e.getMessage}") }
    inputs.output
  }

  /** Module id, test source directories and Scala version of every project in the build. */
  private[sbtplugin] def collectModules(state: State): Seq[ModuleSources] = {
    val extracted = Project.extract(state)
    extracted.structure.allProjectRefs.flatMap { ref =>
      val dirs = extracted.getOpt(ref / Test / unmanagedSourceDirectories).getOrElse(Nil)
      val scalaV = extracted.getOpt(ref / scalaVersion).getOrElse("2.13")
      if (dirs.isEmpty) None else Some(ModuleSources(ref.project, dirs, scalaV))
    }
  }

  /** The test classes to leave out, as written to selection.json. Unreadable means none. */
  private def readDeselected(file: File): Set[String] =
    try
      Json.parse(new String(Files.readAllBytes(file.toPath), StandardCharsets.UTF_8)).toOption match {
        case Some(json) if json.field("mode").flatMap(_.asString).contains(SelectionDocument.ModeFilter) =>
          json.field("deselected").flatMap(_.asArray).getOrElse(Vector.empty).flatMap(_.asString).toSet
        case _ => Set.empty
      }
    catch { case NonFatal(_) => Set.empty }

  private final class SbtLog(log: Logger) extends com.krrrr38.decisionfilter.select.Log {
    def info(msg: String): Unit = log.info(msg)
    def debug(msg: String): Unit = log.debug(msg)
  }

  // -----------------------------------------------------------------------------------------------------------------
  // decisionFilterTest

  /** 1. decisionFilterSelect 2. the root's aggregated tests, with the deselected classes filtered out.
    *
    * The deselection lives on the State only while the tests run, so a plain `test` is never filtered. The tests run
    * inside this command, so its success or failure is the tests' (which is what CI and sbt server clients see).
    */
  private lazy val testCommand: Command = Command.command("decisionFilterTest") { s0 =>
    val (s1, file) = Project.extract(s0).runTask(ThisBuild / decisionFilterSelect, s0)
    val deselected = readDeselected(file)
    val s2 = if (deselected.isEmpty) s1 else s1.put(activeDeselection, deselected)
    try SbtCompat.runTests(Project.extract(s2), s2).remove(activeDeselection)
    catch {
      case NonFatal(e) =>
        val s = s2.remove(activeDeselection)
        s.log.error(s"decision-filter: tests failed (${e.getMessage})")
        s.fail
    }
  }
}
