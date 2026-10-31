package com.krrrr38.decisionfilter.select

final case class GateConfig(cutoff: Double, unsureBelow: Double, unsureMargin: Double)

object GateConfig {
  val Default: GateConfig = GateConfig(cutoff = 2.0, unsureBelow = 0.5, unsureMargin = 1.0)
}

sealed abstract class Reason(val name: String) extends Product with Serializable
object Reason {

  /** The class's own file changed. */
  case object Touched extends Reason("touched")

  /** A file of one of its in-project parents (e.g. a shared base spec) changed. */
  case object BaseTouched extends Reason("base_touched")
  case object Missing extends Reason("missing")
  case object Scored extends Reason("scored")
  case object Unsure extends Reason("unsure")
  case object Below extends Reason("below")

  /** Not asked: everything runs anyway (no API key, an API error, a dry run). */
  case object NotAsked extends Reason("not_asked")
}

final case class Verdict(suite: TestSuite, selected: Boolean, reason: Reason, answer: Option[Answer])

/** Every rule fails towards running the test class. */
object Gate {

  def decide(suite: TestSuite, answer: Option[Answer], config: GateConfig): Verdict =
    answer.flatMap(a => a.score.filter(isNumber).map(s => (s, a.confidence.filter(isNumber)))) match {
      case None => Verdict(suite, selected = true, Reason.Missing, answer)
      case Some((score, _)) if score >= config.cutoff =>
        Verdict(suite, selected = true, Reason.Scored, answer)
      // No confidence is no confidence: treat it as low.
      case Some((score, confidence))
          if score >= config.cutoff - config.unsureMargin && confidence.forall(_ < config.unsureBelow) =>
        Verdict(suite, selected = true, Reason.Unsure, answer)
      case Some(_) => Verdict(suite, selected = false, Reason.Below, answer)
    }

  private def isNumber(d: Double): Boolean = !d.isNaN && !d.isInfinite
}
