package com.krrrr38.decisionfilter.internal.json

import org.scalatest.funsuite.AnyFunSuite

class JsonSpec extends AnyFunSuite {
  import Json._

  test("round-trips strings, numbers, booleans, null, arrays and objects") {
    val j = obj(
      "s" -> str("quote \" backslash \\ newline \n tab \t ünïcode 日本語"),
      "int" -> num(21400L),
      "double" -> num(2.5),
      "t" -> bool(true),
      "f" -> bool(false),
      "n" -> JNull,
      "a" -> arr(str("x"), num(1L)),
      "empty" -> obj()
    )
    assert(parse(j.render) == Right(j))
    assert(parse(j.pretty) == Right(j))
  }

  test("whole numbers render without a fraction") {
    assert(obj("n" -> num(21400L), "d" -> num(2.0)).render == """{"n":21400,"d":2}""")
  }

  test("reads what the APIs send") {
    val Right(v) = parse("""{"answers":{"q0000":{"score":2.6,"confidence":0.81}},"usage":{"input_tokens":1e3}}""")
    assert(v.field("answers").flatMap(_.field("q0000")).flatMap(_.field("score")).flatMap(_.asDouble) == Some(2.6))
    assert(v.field("usage").flatMap(_.field("input_tokens")).flatMap(_.asNumber).map(_.toLong) == Some(1000L))
  }

  test("invalid input is an error, not an exception") {
    assert(parse("{not json").isLeft)
    assert(parse("").isLeft)
  }
}
