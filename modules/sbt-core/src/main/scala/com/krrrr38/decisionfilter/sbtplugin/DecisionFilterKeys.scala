package com.krrrr38.decisionfilter.sbtplugin

import java.io.File

import scala.concurrent.duration.FiniteDuration

import com.krrrr38.decisionfilter.select.BackendFactory
import sbt._

trait DecisionFilterKeys {
  // @transient: the result depends on the git working tree, so sbt 2 must never serve it from its task cache.
  @transient
  val decisionFilterSelect =
    taskKey[File]("Selects the tests the current diff can affect and writes selection.json. Returns its path.")
  val decisionFilterSelectionFile = settingKey[File]("Where selection.json is written.")
  val decisionFilterBase =
    settingKey[Option[String]]("Git ref to compare against. Default: $DECISION_FILTER_BASE, " +
      "$CI_MERGE_REQUEST_DIFF_BASE_SHA, origin/HEAD, origin/main.")
  val decisionFilterCutoff = settingKey[Double]("Select a test whose score is at least this. Default 2.0.")
  val decisionFilterUnsureBelow =
    settingKey[Double]("Confidence below which a near-cutoff score still selects the test. Default 0.5.")
  val decisionFilterUnsureMargin = settingKey[Double]("How far under the cutoff 'unsure' applies. Default 1.0.")
  val decisionFilterRunAllPatterns = settingKey[Seq[String]]("Globs; if a changed file matches, every test runs.")
  val decisionFilterRedactPatterns =
    settingKey[Seq[String]]("Regexes; matching text in the diff is replaced before it is sent.")
  val decisionFilterConcurrency = settingKey[Int]("Concurrent requests to the backend. Default 8.")
  val decisionFilterTimeout = settingKey[FiniteDuration]("Overall timeout for asking the backend. Default 5 minutes.")
  val decisionFilterDryRun = settingKey[Boolean]("Extract tests but do not ask the backend; every test runs.")
  val decisionFilterBackends =
    settingKey[Seq[BackendFactory]]("Decision backends. Set by a backend plugin; exactly one is allowed.")
}

object DecisionFilterKeys extends DecisionFilterKeys
