package com.krrrr38.decisionfilter.select

import com.krrrr38.decisionfilter.internal.json.Json
import org.scalatest.funsuite.AnyFunSuite

class GateAndStateSpec extends AnyFunSuite {

  private def suite(id: String, module: String = "m") = TestSuite(module, id, s"$id.scala", Vector.empty, Vector("x"))
  private val s = suite("p.S")
  private def gate(score: Option[Double], confidence: Option[Double]) =
    Gate.decide(s, Some(Answer(score, confidence)), GateConfig.Default)

  test("gate boundaries") {
    assert(gate(Some(2.0), Some(0.9)).reason == Reason.Scored)
    assert(gate(Some(1.99), Some(0.49)).reason == Reason.Unsure)
    assert(gate(Some(1.99), Some(0.5)).reason == Reason.Below)
    assert(gate(Some(1.0), Some(0.1)).reason == Reason.Unsure)
    assert(gate(Some(0.99), Some(0.1)).reason == Reason.Below)
    assert(!gate(Some(0.99), Some(0.1)).selected)
  }

  test("no confidence counts as low confidence; no score is no answer") {
    assert(gate(Some(1.5), None).reason == Reason.Unsure)
    assert(gate(Some(0.5), None).reason == Reason.Below)
    assert(gate(None, Some(0.9)).reason == Reason.Missing)
    assert(gate(Some(Double.NaN), Some(0.9)).reason == Reason.Missing)
    assert(Gate.decide(s, None, GateConfig.Default).reason == Reason.Missing)
  }

  private def file(path: String, size: Int) =
    FileDiff(path, None, "x" * size, Set.empty, binary = false, deleted = false)

  test("state keeps as many whole files as fit, in their original order") {
    val files = Vector(file("big", 60), file("a", 30), file("b", 30), file("c", 50))
    val st = StateBuilder.build(files, "stat", Nil, limit = 100)
    assert(st.truncated)
    assert(st.diff.length == 60)
    assert(st.omittedFiles == Vector("big", "c"))
    assert(st.changedFiles == Vector("big", "a", "b", "c"))
    assert(!StateBuilder.build(files, "", Nil, limit = 1000).truncated)
  }

  test("redaction") {
    val Right(patterns) = StateBuilder.compile(StateBuilder.DefaultRedactPatterns)
    val text = "+val key = \"AKIAABCDEFGHIJKLMNOP\"\n+password = hunter2hunter2\n+url = jdbc://user:pa55@db\n+fine = 1"
    val out = StateBuilder.redact(text, patterns)
    assert(!out.contains("AKIAABCDEFGHIJKLMNOP"))
    assert(!out.contains("hunter2hunter2"))
    assert(!out.contains("pa55"))
    assert(out.contains("fine = 1"))
    assert(StateBuilder.compile(Seq("(")).isLeft)
  }

  test("run-all globs") {
    val Right(trigger) = RunAllTrigger(RunAllTrigger.DefaultPatterns)
    assert(trigger.firstMatch(Seq("src/A.scala", "build.sbt")).map(_._1).contains("build.sbt"))
    assert(trigger.firstMatch(Seq("sub/version.sbt")).isDefined)
    assert(trigger.firstMatch(Seq("project/plugins.sbt")).isDefined)
    assert(trigger.firstMatch(Seq("sub/project/x.scala")).isEmpty)
    assert(RunAllTrigger(Seq("[")).isLeft)
  }

  private def doc(mode: String, verdicts: Verdict*) =
    SelectionDocument(mode, None, Some("jev"), Some("m"), None, None, None, GateConfig.Default, verdicts.toVector, Spent.Zero)

  test("a class is deselected only when every module deselected it, and only when filtering") {
    val d = doc(
      "filter",
      Verdict(suite("p.A", "a"), selected = false, Reason.Below, None),
      Verdict(suite("p.A", "b"), selected = true, Reason.Scored, None),
      Verdict(suite("p.B"), selected = false, Reason.Below, None),
      Verdict(suite("p.C"), selected = true, Reason.Touched, None)
    )
    assert(d.deselected == Set("p.B"))
    assert((d.selectedCount, d.totalCount) == ((2, 3)))
    assert(doc("all", Verdict(suite("p.B"), selected = false, Reason.Below, None)).deselected.isEmpty)
  }

  test("selection.json lists what was left out") {
    val d = doc("filter", Verdict(suite("p.B"), selected = false, Reason.Below, Some(Answer(Some(0.1), Some(0.9)))))
    val Right(json) = Json.parse(d.toJson.pretty)
    assert(json.field("deselected").flatMap(_.asArray).map(_.flatMap(_.asString)) == Some(Vector("p.B")))
    assert(json.field("suites").flatMap(_.asArray).map(_.size) == Some(1))
  }
}
