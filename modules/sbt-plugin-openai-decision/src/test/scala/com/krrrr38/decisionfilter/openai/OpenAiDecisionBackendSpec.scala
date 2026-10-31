package com.krrrr38.decisionfilter.openai

import com.krrrr38.decisionfilter.internal.json.Json
import com.krrrr38.decisionfilter.select._
import com.krrrr38.decisionfilter.select.http.{FakeServer, Http}
import org.scalatest.funsuite.AnyFunSuite

class OpenAiDecisionBackendSpec extends AnyFunSuite {

  private val state = DiffState(Vector("a.scala"), "1 file changed", "diff --git ...", truncated = false, Vector.empty)
  private def q(i: Int) =
    SuiteQuestion(f"q$i%04d", TestSuite("core", s"p.S$i", "core/S.scala", Vector.empty, Vector("A should x", "A should y")))

  private def backend(server: FakeServer, env: Map[String, String] = Map.empty): DecisionBackend =
    OpenAiDecisionBackend
      .factory("gpt-6-luna", http = () => new Http(sleep = _ => ()))
      .create(Env.of(Map("OPENAI_API_KEY" -> "sk-test", "OPENAI_BASE_URL" -> s"${server.url}/v1") ++ env))
      .fold(e => fail(e), identity)

  test("sends shared input and named score questions with levels; reads score answers") {
    val answer =
      """{"answers":[
        |  {"type":"score","name":"q0000","score":2.4,"probabilities":[],"confidence":0.7},
        |  {"type":"refusal","name":"q0001"},
        |  {"type":"score","name":"q0002","score":0.1}
        |],"usage":{"input_tokens":900,"output_tokens":0,"input_tokens_details":{"cached_tokens":800}}}""".stripMargin
    val server = new FakeServer(Seq((200, Nil, answer)))
    try {
      val Right(res) =
        backend(server, Map("OPENAI_ORG_ID" -> "org-1", "OPENAI_PROJECT_ID" -> "proj-1")).judge(state, Vector(q(0), q(1), q(2)))
      assert(res.answers == Map(
        "q0000" -> Answer(Some(2.4), Some(0.7)),
        "q0001" -> Answer(None, None),
        "q0002" -> Answer(Some(0.1), None)
      ))
      assert(res.usage == Usage(900, 0))

      val req = server.requests.head
      assert(req.path == "/v1/decisions")
      assert(req.headers("authorization") == "Bearer sk-test")
      assert(req.headers("openai-organization") == "org-1")
      assert(req.headers("openai-project") == "proj-1")
      val Right(body) = Json.parse(req.body)
      assert(body.field("model").flatMap(_.asString).contains("gpt-6-luna"))
      val content = body.field("input").flatMap(_.asArray).flatMap(_.headOption).flatMap(_.field("content")).flatMap(_.asArray).get
      assert(content(1).field("type").flatMap(_.asString).contains("input_json"))
      assert(content(1).field("data").flatMap(_.field("diff")).flatMap(_.asString).contains("diff --git ..."))
      val q0 = body.field("questions").flatMap(_.asArray).map(_.head).get
      assert(q0.field("name").flatMap(_.asString).contains("q0000"))
      assert(q0.field("type").flatMap(_.asString).contains("score"))
      assert(q0.field("levels").flatMap(_.asArray).map(_.size).contains(4))
      assert(q0.field("instructions").flatMap(_.asString).exists(_.contains("test_class: p.S0")))
      assert(q0.field("instructions").flatMap(_.asString).exists(_.contains("- A should y")))
      assert(body.field("prompt_cache_key").isDefined)
    } finally server.close()
  }

  test("errors: context length, quota, rate limit and 5xx") {
    val server = new FakeServer(
      Seq(
        (400, Nil, """{"error":{"code":"context_length_exceeded"}}"""),
        (429, Nil, """{"error":{"code":"insufficient_quota"}}"""),
        (429, Seq("retry-after-ms" -> "1"), """{"error":{"code":"rate_limit_exceeded"}}"""),
        (503, Nil, "unavailable"),
        (200, Nil, """{"answers":[]}""")
      )
    )
    try {
      val b = backend(server)
      assert(b.judge(state, Vector(q(0))).left.map(_.getClass) == Left(classOf[BackendError.TooLarge]))
      assert(b.judge(state, Vector(q(0))).left.map(_.getClass) == Left(classOf[BackendError.Auth]))
      assert(b.judge(state, Vector(q(0))).isRight)
    } finally server.close()
  }

  test("no API key is a reason to run everything") {
    assert(OpenAiDecisionBackend.factory("gpt-6-luna").create(Env.of(Map.empty)) == Left("no API key; set OPENAI_API_KEY"))
  }
}
