package com.krrrr38.decisionfilter.jev

import com.krrrr38.decisionfilter.internal.json.Json
import com.krrrr38.decisionfilter.select._
import com.krrrr38.decisionfilter.select.http.{Http, HttpResult, Outcome}

/** TypeSafe Jev (System One): one shared `state`, many `score` questions, one round trip. */
final class JevBackend(apiKey: String, baseUrl: String, val model: String, http: Http) extends DecisionBackend {
  val id: String = JevBackend.Id
  val maxQuestionsPerCall: Int = 400

  def judge(state: DiffState, questions: Vector[SuiteQuestion]): Either[BackendError, BackendResult] =
    if (questions.isEmpty) Right(BackendResult(Map.empty, Usage.Zero))
    else
      http
        .postJson(
          s"${baseUrl.stripSuffix("/")}/v1/systemone",
          Seq("authorization" -> s"Bearer $apiKey"),
          JevBackend.requestBody(model, state, questions).render,
          JevBackend.classify
        )
        .flatMap(res => JevBackend.parseResponse(res.body))
}

object JevBackend {
  val Id = "jev"
  val DefaultModel = "jev-1.13.0"
  val DefaultBaseUrl = "https://api.typesafe.ai"
  val DefaultApiKeyEnv: Seq[String] = Seq("TYPESAFE_API_KEY", "TYPESAFEAI_API_KEY")
  val DefaultBaseUrlEnv: Seq[String] = Seq("TYPESAFE_BASE_URL", "TYPESAFEAI_BASE_URL")

  /** Credentials and the endpoint are read from the environment when selection runs, never from settings. */
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
          case None => Left(s"no API key; set ${apiKeyEnv.headOption.getOrElse("TYPESAFE_API_KEY")}")
          case Some((_, key)) =>
            val url = env.first(baseUrlEnv).map(_._2).getOrElse(DefaultBaseUrl)
            Right(new JevBackend(key, url, m, http()))
        }
    }
  }

  def requestBody(model: String, state: DiffState, questions: Vector[SuiteQuestion]): Json = {
    import Json._
    val stateJson = obj(
      "reviewing" -> str(ScoreRubric.reviewing),
      "changed_files" -> strs(state.changedFiles),
      "stat" -> str(state.stat),
      "diff" -> str(state.diff),
      "truncated" -> bool(state.truncated),
      "omitted_files" -> strs(state.omittedFiles)
    )
    val qs = JObj(questions.map { q =>
      q.id -> obj(
        "type" -> str("score"),
        "instructions" -> JObj(("task" -> str(ScoreRubric.task)) +: ScoreRubric.subject(q).map {
          case (k, Left(v))   => k -> str(v)
          case (k, Right(vs)) => k -> strs(vs)
        }),
        "criteria" -> strs(ScoreRubric.levels.map(_.description))
      )
    })
    obj("model" -> str(model), "state" -> stateJson, "questions" -> qs)
  }

  def classify(res: HttpResult): Outcome =
    if (res.status >= 200 && res.status < 300) Outcome.Success
    else if (res.status == 400 && res.body.contains("max_tokens_exceeded"))
      Outcome.Fatal(BackendError.TooLarge(s"HTTP 400: max_tokens_exceeded"))
    else if (res.status == 401 || res.status == 402 || res.status == 403)
      Outcome.Fatal(BackendError.Auth(s"HTTP ${res.status}: ${res.body.take(200)}"))
    else if (res.status == 429) Outcome.RateLimited
    else if (res.status >= 500) Outcome.Transient
    else Outcome.Fatal(BackendError.Failed(s"HTTP ${res.status}: ${res.body.take(300)}"))

  /** Answers are read as `{score, confidence}`; `value` stands in for a missing `score`; a bare number is a score.
    * Anything that is not a number counts as no answer, which selects the test.
    */
  def parseResponse(body: String): Either[BackendError, BackendResult] =
    Json.parse(body) match {
      case Left(e) => Left(BackendError.Failed(s"unreadable response: ${e.message}"))
      case Right(json) =>
        val answers = json.field("answers").flatMap(_.asObject).getOrElse(Vector.empty).map { case (id, a) =>
          id -> (a match {
            case Json.JNum(v) => Answer(Some(v.toDouble), None)
            case other =>
              Answer(
                other.field("score").flatMap(_.asDouble).orElse(other.field("value").flatMap(_.asDouble)),
                other.field("confidence").flatMap(_.asDouble)
              )
          })
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
