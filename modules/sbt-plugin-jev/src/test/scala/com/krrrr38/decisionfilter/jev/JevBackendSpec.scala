package com.krrrr38.decisionfilter.jev

import com.krrrr38.decisionfilter.internal.json.Json
import com.krrrr38.decisionfilter.select._
import com.krrrr38.decisionfilter.select.http.{FakeServer, Http}
import org.scalatest.funsuite.AnyFunSuite

class JevBackendSpec extends AnyFunSuite {

  private val state = DiffState(Vector("a.scala"), "1 file changed", "diff --git ...", truncated = false, Vector.empty)
  private def q(i: Int) =
    SuiteQuestion(f"q$i%04d", TestSuite("core", s"p.S$i", "core/S.scala", Vector.empty, Vector("A should x", "A should y")))

  private val noSleep = () => new Http(sleep = _ => ())

  private def backend(server: FakeServer, env: Map[String, String] = Map.empty): DecisionBackend =
    JevBackend
      .factory("jev-1.13.0", http = noSleep)
      .create(Env.of(Map("TYPESAFE_API_KEY" -> "secret", "TYPESAFE_BASE_URL" -> server.url) ++ env))
      .fold(e => fail(e), identity)

  test("sends one state and keyed score questions; reads score, value and confidence") {
    val answer =
      """{"model":"jev-1.13.0","answers":{"q0000":{"score":2.5,"confidence":0.8},"q0001":{"value":0.2},"q0002":"n/a"},
        |"usage":{"input_tokens":1234,"output_tokens":56}}""".stripMargin
    val server = new FakeServer(Seq((200, Nil, answer)))
    try {
      val Right(res) = backend(server).judge(state, Vector(q(0), q(1), q(2)))
      assert(res.answers("q0000") == Answer(Some(2.5), Some(0.8)))
      assert(res.answers("q0001") == Answer(Some(0.2), None))
      assert(res.answers("q0002") == Answer(None, None))
      assert(res.usage == Usage(1234, 56))

      val req = server.requests.head
      assert(req.path == "/v1/systemone")
      assert(req.headers("authorization") == "Bearer secret")
      val Right(body) = Json.parse(req.body)
      assert(body.field("model").flatMap(_.asString).contains("jev-1.13.0"))
      assert(body.field("state").flatMap(_.field("diff")).flatMap(_.asString).contains("diff --git ..."))
      val q0 = body.field("questions").flatMap(_.field("q0000")).get
      assert(q0.field("type").flatMap(_.asString).contains("score"))
      assert(q0.field("instructions").flatMap(_.field("test_class")).flatMap(_.asString).contains("p.S0"))
      assert(q0.field("instructions").flatMap(_.field("tests")).flatMap(_.asArray).map(_.size).contains(2))
      assert(q0.field("criteria").flatMap(_.asArray).map(_.size).contains(4))
      assert(!req.body.contains("secret"), "the key goes in the header only")
    } finally server.close()
  }

  test("errors: max_tokens_exceeded, auth, retried 5xx and 429") {
    val server = new FakeServer(
      Seq(
        (400, Nil, """{"error":{"code":"max_tokens_exceeded"}}"""),
        (401, Nil, "bad key"),
        (529, Nil, "overloaded"),
        (429, Seq("Retry-After" -> "0"), "slow down"),
        (200, Nil, """{"answers":{}}""")
      )
    )
    try {
      val b = backend(server)
      assert(b.judge(state, Vector(q(0))).left.map(_.getClass) == Left(classOf[BackendError.TooLarge]))
      assert(b.judge(state, Vector(q(0))).left.map(_.getClass) == Left(classOf[BackendError.Auth]))
      assert(b.judge(state, Vector(q(0))).isRight)
      assert(server.requests.size == 5)
    } finally server.close()
  }

  test("no API key is a reason to run everything; the variable names are configurable") {
    val f = JevBackend.factory("jev-1.13.0", apiKeyEnv = Seq("MY_JEV_KEY"))
    assert(f.create(Env.of(Map("TYPESAFE_API_KEY" -> "x"))) == Left("no API key; set MY_JEV_KEY"))
    assert(f.create(Env.of(Map("MY_JEV_KEY" -> "x"))).isRight)
    assert(JevBackend.factory("m").create(Env.of(Map("TYPESAFE_API_KEY" -> "  "))).isLeft)
  }
}
