package com.krrrr38.decisionfilter.openai

import com.krrrr38.decisionfilter.sbtplugin.DecisionFilterPlugin
import com.krrrr38.decisionfilter.sbtplugin.DecisionFilterPlugin.autoImport._
import sbt._

/** DecisionFilter with the OpenAI Decisions API as the backend.
  *
  * The API key is read from `$OPENAI_API_KEY` (or the variables in `decisionFilterOpenAiApiKeyEnv`) when selection
  * runs; `$OPENAI_ORG_ID` and `$OPENAI_PROJECT_ID` are sent when set. Without a key, every test runs.
  */
object DecisionFilterOpenAiDecisionPlugin extends AutoPlugin {
  override def trigger = allRequirements
  override def requires = DecisionFilterPlugin

  object autoImport {
    val decisionFilterOpenAiModel =
      settingKey[String](s"Decisions API model. Default ${OpenAiDecisionBackend.DefaultModel}.")
    val decisionFilterOpenAiApiKeyEnv =
      settingKey[Seq[String]]("Environment variables to read the API key from, first set wins.")
    val decisionFilterOpenAiBaseUrlEnv =
      settingKey[Seq[String]]("Environment variables to read the API base URL from, first set wins.")
  }
  import autoImport._

  override def buildSettings: Seq[Setting[_]] = Seq(
    decisionFilterOpenAiModel := OpenAiDecisionBackend.DefaultModel,
    decisionFilterOpenAiApiKeyEnv := OpenAiDecisionBackend.DefaultApiKeyEnv,
    decisionFilterOpenAiBaseUrlEnv := OpenAiDecisionBackend.DefaultBaseUrlEnv,
    decisionFilterBackends += OpenAiDecisionBackend.factory(
      decisionFilterOpenAiModel.value,
      decisionFilterOpenAiApiKeyEnv.value,
      decisionFilterOpenAiBaseUrlEnv.value
    )
  )
}
