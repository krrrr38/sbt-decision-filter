package com.krrrr38.decisionfilter.select

import org.scalatest.funsuite.AnyFunSuite

class ExtractorSpec extends AnyFunSuite {

  private def extract(sources: (String, String)*): ExtractResult =
    Extractor.extractSources(sources.map { case (path, text) => ("m", path, text, "2.13.18") })

  private def suite(r: ExtractResult, id: String): TestSuite =
    r.suites.find(_.suiteId == id).getOrElse(fail(s"$id not found in ${r.suites.map(_.suiteId)}"))

  test("finds concrete ScalaTest classes with their fully qualified names") {
    val r = extract(
      "a/FooSpec.scala" ->
        """package com.example
          |package foo
          |
          |import org.scalatest.funsuite.AnyFunSuite
          |import org.scalatest.flatspec
          |
          |class FooSpec extends AnyFunSuite { test("x") {} }
          |class BarSpec extends flatspec.AnyFlatSpec
          |class AsyncSpec extends org.scalatest.wordspec.AsyncWordSpecLike
          |abstract class AbstractSpec extends AnyFunSuite
          |trait SomeTrait extends AnyFunSuite
          |object SomeObject extends AnyFunSuite
          |class Helper { def test(s: String) = s }
          |object Holder { class Nested extends AnyFunSuite }
          |""".stripMargin
    )
    assert(r.suites.map(_.suiteId).sorted == Vector("com.example.foo.AsyncSpec", "com.example.foo.BarSpec", "com.example.foo.FooSpec"))
    assert(suite(r, "com.example.foo.FooSpec").file == "a/FooSpec.scala")
  }

  test("follows in-project base classes and records their files") {
    val r = extract(
      "base/UnitSpec.scala" ->
        """package base
          |abstract class UnitSpec extends org.scalatest.wordspec.AnyWordSpec with Helpers
          |trait Helpers
          |""".stripMargin,
      "a/ASpec.scala" ->
        """package a
          |class ASpec extends base.UnitSpec { "A" should { "work" in {} } }
          |""".stripMargin,
      "a/NotATest.scala" -> "package a\nclass NotATest extends base.Helpers\n"
    )
    assert(r.suites.map(_.suiteId) == Vector("a.ASpec"))
    assert(suite(r, "a.ASpec").baseFiles == Vector("base/UnitSpec.scala"))
  }

  test("collects test names per style as hints") {
    val r = extract(
      "S.scala" ->
        """import org.scalatest.funspec.AnyFunSpec
          |import org.scalatest.wordspec.AnyWordSpec
          |import org.scalatest.flatspec.AnyFlatSpec
          |import org.scalatest.freespec.AnyFreeSpec
          |import org.scalatest.featurespec.AnyFeatureSpec
          |import org.scalatest.propspec.AnyPropSpec
          |
          |class Fun extends AnyFunSpec { describe("A") { it("x") {}; ignore("y") {} } }
          |class Word extends AnyWordSpec { "A" when { "empty" should { "x" in {}; "y" taggedAs(Slow) in {} } } }
          |class Flat extends AnyFlatSpec { "A" should "x" in {}; it must "y" in {}; behavior of "B"; it can "z" in {} }
          |class Free extends AnyFreeSpec { "A" - { "x" in {} } }
          |class Feature extends AnyFeatureSpec { Feature("A") { Scenario("x") {} } }
          |class Prop extends AnyPropSpec { property("x") {}; property(s"dyn $n") {} }
          |""".stripMargin
    )
    assert(suite(r, "Fun").testNames == Vector("A x", "A y"))
    assert(suite(r, "Word").testNames == Vector("A when empty should x", "A when empty should y"))
    assert(suite(r, "Flat").testNames == Vector("A should x", "A must y", "B can z"))
    assert(suite(r, "Free").testNames == Vector("A x"))
    assert(suite(r, "Feature").testNames == Vector("Feature: A Scenario: x"))
    assert(suite(r, "Prop").testNames == Vector("x", "<dynamic>"))
  }

  test("Scala 3 syntax") {
    val r = Extractor.extractSources(
      Seq(
        (
          "m",
          "S.scala",
          """package a.b
            |
            |import org.scalatest.funspec.AnyFunSpec
            |
            |class S extends AnyFunSpec:
            |  describe("A"):
            |    it("x"):
            |      assert(true)
            |""".stripMargin,
          "3.3.7"
        )
      )
    )
    assert(r.suites.map(s => s.suiteId -> s.testNames) == Vector("a.b.S" -> Vector("A x")))
  }

  test("a file that does not parse is reported, not fatal") {
    val r = extract("S.scala" -> "class S extends {")
    assert(r.suites.isEmpty)
    assert(r.parseFailures.map(_._1) == Vector("S.scala"))
  }
}
