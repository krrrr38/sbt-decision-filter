package com.krrrr38.decisionfilter.jev

import com.krrrr38.decisionfilter.sbtplugin.DecisionFilterPlugin
import com.krrrr38.decisionfilter.sbtplugin.DecisionFilterPlugin.autoImport._
import sbt._

/** DecisionFilter with TypeSafe Jev as the backend.
  *
  * The API key is read from `$TYPESAFE_API_KEY` (or the variables in `decisionFilterJevApiKeyEnv`) when selection
  * runs. Without one, every test runs.
  */
object DecisionFilterJevPlugin extends AutoPlugin {
  override def trigger = allRequirements
  override def requires = DecisionFilterPlugin

  object autoImport {
    val decisionFilterJevModel = settingKey[String](s"Jev model, pinned. Default ${JevBackend.DefaultModel}.")
    val decisionFilterJevApiKeyEnv =
      settingKey[Seq[String]]("Environment variables to read the API key from, first set wins.")
    val decisionFilterJevBaseUrlEnv =
      settingKey[Seq[String]]("Environment variables to read the API base URL from, first set wins.")
  }
  import autoImport._

  override def buildSettings: Seq[Setting[_]] = Seq(
    decisionFilterJevModel := JevBackend.DefaultModel,
    decisionFilterJevApiKeyEnv := JevBackend.DefaultApiKeyEnv,
    decisionFilterJevBaseUrlEnv := JevBackend.DefaultBaseUrlEnv,
    decisionFilterBackends += JevBackend.factory(
      decisionFilterJevModel.value,
      decisionFilterJevApiKeyEnv.value,
      decisionFilterJevBaseUrlEnv.value
    )
  )
}
