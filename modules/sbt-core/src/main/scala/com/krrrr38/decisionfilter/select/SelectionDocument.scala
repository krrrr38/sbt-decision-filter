package com.krrrr38.decisionfilter.select

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, StandardCopyOption}

import com.krrrr38.decisionfilter.internal.json.Json

final case class Spent(calls: Int, inputTokens: Long, outputTokens: Long, splits: Int)
object Spent {
  val Zero: Spent = Spent(0, 0L, 0L, 0)
}

/** The result of a selection, written to `selection.json` for people and CI artifacts to read.
  *
  * `mode` is "filter" (leave out `deselected`) or "all" (run everything, `reason` says why).
  */
final case class SelectionDocument(
    mode: String,
    reason: Option[String],
    backend: Option[String],
    model: Option[String],
    base: Option[String],
    mergeBase: Option[String],
    head: Option[String],
    gate: GateConfig,
    verdicts: Vector[Verdict],
    spent: Spent
) {
  def isFilter: Boolean = mode == SelectionDocument.ModeFilter

  /** Test classes to leave out. Empty unless filtering. */
  def deselected: Set[String] =
    if (!isFilter) Set.empty
    else {
      // A class id found in several modules (shared sources) is only left out when every module deselected it.
      val byId = verdicts.groupBy(_.suite.suiteId)
      byId.collect { case (id, vs) if vs.forall(!_.selected) => id }.toSet
    }

  def selectedCount: Int = verdicts.map(_.suite.suiteId).distinct.count(id => !deselected.contains(id))
  def totalCount: Int = verdicts.map(_.suite.suiteId).distinct.size

  def toJson: Json = {
    import Json._
    obj(
      "version" -> num(SelectionDocument.Version.toLong),
      "mode" -> str(mode),
      "reason" -> opt(reason)(str),
      "backend" -> opt(backend)(str),
      "model" -> opt(model)(str),
      "base" -> opt(base)(str),
      "merge_base" -> opt(mergeBase)(str),
      "head" -> opt(head)(str),
      "gate" -> obj(
        "cutoff" -> num(gate.cutoff),
        "unsure_below" -> num(gate.unsureBelow),
        "unsure_margin" -> num(gate.unsureMargin)
      ),
      "selected" -> num(selectedCount.toLong),
      "total" -> num(totalCount.toLong),
      "deselected" -> strs(deselected.toVector.sorted),
      "suites" -> JArr(verdicts.map { v =>
        obj(
          "module" -> str(v.suite.module),
          "suite" -> str(v.suite.suiteId),
          "file" -> str(v.suite.file),
          "selected" -> bool(v.selected),
          "reason" -> str(v.reason.name),
          "score" -> opt(v.answer.flatMap(_.score))(num),
          "confidence" -> opt(v.answer.flatMap(_.confidence))(num)
        )
      }),
      "spent" -> obj(
        "calls" -> num(spent.calls.toLong),
        "input_tokens" -> num(spent.inputTokens),
        "output_tokens" -> num(spent.outputTokens),
        "splits" -> num(spent.splits.toLong)
      )
    )
  }

  def write(file: File): Unit = {
    val parent = file.getAbsoluteFile.getParentFile
    Files.createDirectories(parent.toPath)
    val tmp = new File(parent, file.getName + ".tmp")
    Files.write(tmp.toPath, (toJson.pretty + "\n").getBytes(StandardCharsets.UTF_8))
    Files.move(tmp.toPath, file.toPath, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE)
  }
}

object SelectionDocument {
  val Version: Int = 2
  val ModeFilter: String = "filter"
  val ModeAll: String = "all"
}
