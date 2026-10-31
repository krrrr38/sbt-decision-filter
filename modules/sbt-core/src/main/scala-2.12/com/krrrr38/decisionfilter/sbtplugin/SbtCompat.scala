package com.krrrr38.decisionfilter.sbtplugin

import com.krrrr38.decisionfilter.select.GateConfig
import sbt._
import sbt.Keys._

/** sbt 1.x */
private[sbtplugin] object SbtCompat {
  import DecisionFilterKeys._

  def selectTask: Def.Initialize[Task[File]] = Def.task {
    DecisionFilterPlugin.runSelect(
      DecisionFilterPlugin.Inputs(
        buildRoot = (ThisBuild / baseDirectory).value,
        modules = DecisionFilterPlugin.collectModules(state.value),
        output = decisionFilterSelectionFile.value,
        base = decisionFilterBase.value,
        gate = GateConfig(decisionFilterCutoff.value, decisionFilterUnsureBelow.value, decisionFilterUnsureMargin.value),
        runAllPatterns = decisionFilterRunAllPatterns.value,
        redactPatterns = decisionFilterRedactPatterns.value,
        concurrency = decisionFilterConcurrency.value,
        timeout = decisionFilterTimeout.value,
        dryRun = decisionFilterDryRun.value,
        backends = decisionFilterBackends.value
      ),
      streams.value.log
    )
  }

  def filterOptions: Def.Initialize[Task[Seq[TestOption]]] = Def.task {
    DecisionFilterPlugin.filterOptionsFor(state.value)
  }

  /** The root project's aggregated tests, as a plain `test` at the root would run them. Throws on failure. */
  def runTests(extracted: Extracted, state: State): State =
    extracted.runAggregated(LocalRootProject / Test / test, state)
}
