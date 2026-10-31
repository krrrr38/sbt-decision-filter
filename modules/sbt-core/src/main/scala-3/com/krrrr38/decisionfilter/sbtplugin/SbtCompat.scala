package com.krrrr38.decisionfilter.sbtplugin

import com.krrrr38.decisionfilter.select.GateConfig
import sbt.*
import sbt.Keys.*

/** sbt 2.x */
private[sbtplugin] object SbtCompat {
  import DecisionFilterKeys.*

  // Selection depends on the git working tree, so its result must never come from sbt 2's task cache.
  def selectTask: Def.Initialize[Task[File]] = Def.task {
    Def.uncached {
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
  }

  // Depends on the State of the running command, so it must not be cached either.
  def filterOptions: Def.Initialize[Task[Seq[TestOption]]] = Def.task {
    Def.uncached(DecisionFilterPlugin.filterOptionsFor(state.value))
  }

  /** The root project's aggregated tests. sbt 2's `test` is incremental (testQuick); `testFull` runs them all.
    * Throws on failure.
    */
  def runTests(extracted: Extracted, state: State): State =
    extracted.runAggregated(LocalRootProject / Test / testFull, state)
}
