package com.krrrr38.decisionfilter.openai

import com.krrrr38.decisionfilter.internal.json.Json
import com.krrrr38.decisionfilter.select._
import com.krrrr38.decisionfilter.select.http.{Http, HttpResult, Outcome}

/** OpenAI Decisions API (`POST /v1/decisions`): shared `input`, up to 400 `score` questions per request.
  *
  * https://developers.openai.com/api/docs/guides/decisions
  */
final class OpenAiDecisionBackend(
    apiKey: String,
    baseUrl: String,
    val model: String,
    extraHeaders: Seq[(String, String)],
    http: Http
) extends DecisionBackend {
  val id: String = OpenAiDecisionBackend.Id
  val maxQuestionsPerCall: Int = 400

  def judge(state: DiffState, questions: Vector[SuiteQuestion]): Either[BackendError, BackendResult] =
    if (questions.isEmpty) Right(BackendResult(Map.empty, Usage.Zero))
    else
      http
        .postJson(
          s"${baseUrl.stripSuffix("/")}/v1/decisions",
          ("authorization" -> s"Bearer $apiKey") +: extraHeaders,
          OpenAiDecisionBackend.requestBody(model, state, questions).render,
          OpenAiDecisionBackend.classify
        )
        .flatMap(res => OpenAiDecisionBackend.parseResponse(res.body))
}

object OpenAiDecisionBackend {
  val Id = "openai-decision"
  val DefaultModel = "gpt-6-luna"
  val DefaultBaseUrl = "https://api.openai.com"
  val DefaultApiKeyEnv: Seq[String] = Seq("OPENAI_API_KEY")
  val DefaultBaseUrlEnv: Seq[String] = Seq("OPENAI_BASE_URL")
  val OrganizationEnv = "OPENAI_ORG_ID"
  val ProjectEnv = "OPENAI_PROJECT_ID"

  /** Credentials, organization, project and endpoint are read from the environment when selection runs. */
  def factory(
      model: String,
      apiKeyEnv: Seq[String] = DefaultApiKeyEnv,
      baseUrlEnv: Seq[String] = DefaultBaseUrlEnv,
      http: () => Http = () => new Http()
  ): BackendFactory = {
    val m = model
    new BackendFactory {
      val id: String = Id
      val model: String = m
      def create(env: Env): Either[String, DecisionBackend] =
        env.first(apiKeyEnv) match {
          case None => Left(s"no API key; set ${apiKeyEnv.headOption.getOrElse("OPENAI_API_KEY")}")
          case Some((_, key)) =>
            val url = env.first(baseUrlEnv).map(_._2).getOrElse(DefaultBaseUrl).stripSuffix("/").stripSuffix("/v1")
            val headers =
              env.first(Seq(OrganizationEnv)).map(v => "openai-organization" -> v._2).toSeq ++
                env.first(Seq(ProjectEnv)).map(v => "openai-project" -> v._2).toSeq
            Right(new OpenAiDecisionBackend(key, url, m, headers, http()))
        }
    }
  }

  def requestBody(model: String, state: DiffState, questions: Vector[SuiteQuestion]): Json = {
    import Json._
    val evidence = obj(
      "changed_files" -> strs(state.changedFiles),
      "stat" -> str(state.stat),
      "diff" -> str(state.diff),
      "truncated" -> bool(state.truncated),
      "omitted_files" -> strs(state.omittedFiles)
    )
    val input = arr(
      obj(
        "role" -> str("user"),
        "content" -> arr(
          obj(
            "type" -> str("input_text"),
            "text" -> str(
              s"The JSON below is ${ScoreRubric.reviewing}. `diff` is the unified diff, `stat` its summary. " +
                "When `truncated` is true, the diffs of `omitted_files` were left out to fit; they did change."
            )
          ),
          obj("type" -> str("input_json"), "data" -> evidence)
        )
      )
    )
    val qs = JArr(questions.map { q =>
      obj(
        "type" -> str("score"),
        "name" -> str(q.id),
        "instructions" -> str(instructions(q)),
        "levels" -> JArr(ScoreRubric.levels.map(l => obj("label" -> str(l.label), "description" -> str(l.description))))
      )
    })
    obj(
      "model" -> str(model),
      "input" -> input,
      "questions" -> qs,
      // Every batch shares the same diff; let the API cache it across them.
      "prompt_cache_key" -> str(s"decision-filter:${Integer.toHexString(state.diff.hashCode)}")
    )
  }

  private def instructions(q: SuiteQuestion): String =
    (ScoreRubric.task +: ScoreRubric.subject(q).filterNot(_._1 == "subject").map {
      case (k, Left(v))   => s"$k: $v"
      case (k, Right(vs)) => s"$k:" + vs.map("\n- " + _).mkString
    }).mkString("\n")

  private val TooLargePattern =
    """(?i)(context_length_exceeded|max_tokens|too many tokens|too large|token limit|maximum context|string_above_max_length)""".r

  def classify(res: HttpResult): Outcome =
    if (res.status >= 200 && res.status < 300) Outcome.Success
    else if (res.status == 413 || (res.status == 400 && TooLargePattern.findFirstIn(res.body).isDefined))
      Outcome.Fatal(BackendError.TooLarge(s"HTTP ${res.status}: request too large"))
    else if (res.status == 401 || res.status == 402 || res.status == 403)
      Outcome.Fatal(BackendError.Auth(s"HTTP ${res.status}: ${res.body.take(200)}"))
    else if (res.status == 429) {
      // OpenAI uses 429 for both rate limits and an exhausted quota; the latter will not clear by waiting.
      if (res.body.contains("insufficient_quota")) Outcome.Fatal(BackendError.Auth("HTTP 429: insufficient_quota"))
      else Outcome.RateLimited
    } else if (res.status >= 500) Outcome.Transient
    else Outcome.Fatal(BackendError.Failed(s"HTTP ${res.status}: ${res.body.take(300)}"))

  /** `answers` is an array keyed by `name`. A `refusal` or anything without a numeric score is no answer. */
  def parseResponse(body: String): Either[BackendError, BackendResult] =
    Json.parse(body) match {
      case Left(e) => Left(BackendError.Failed(s"unreadable response: ${e.message}"))
      case Right(json) =>
        val answers = json.field("answers").flatMap(_.asArray).getOrElse(Vector.empty).flatMap { a =>
          a.field("name").flatMap(_.asString).map { name =>
            val isScore = a.field("type").flatMap(_.asString).forall(_ == "score")
            name -> (if (isScore) Answer(a.field("score").flatMap(_.asDouble), a.field("confidence").flatMap(_.asDouble))
                     else Answer(None, None))
          }
        }
        val usage = json.field("usage")
        Right(
          BackendResult(
            answers.toMap,
            Usage(
              usage.flatMap(_.field("input_tokens")).flatMap(_.asNumber).map(_.toLong).getOrElse(0L),
              usage.flatMap(_.field("output_tokens")).flatMap(_.asNumber).map(_.toLong).getOrElse(0L)
            )
          )
        )
    }
}
