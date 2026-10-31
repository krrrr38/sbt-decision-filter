package com.krrrr38.decisionfilter.select

/** A test class found by the extractor.
  *
  * @param suiteId fully qualified class name; what sbt's `Tests.Filter` sees
  * @param baseFiles files of in-project parent classes and traits (e.g. a shared `UnitSpec`)
  * @param testNames best-effort test names, as hints for the model
  */
final case class TestSuite(
    module: String,
    suiteId: String,
    file: String,
    baseFiles: Vector[String],
    testNames: Vector[String]
)

/** The shared evidence every question is judged against. */
final case class DiffState(
    changedFiles: Vector[String],
    stat: String,
    diff: String,
    truncated: Boolean,
    omittedFiles: Vector[String]
)

final case class SuiteQuestion(id: String, suite: TestSuite)

final case class Answer(score: Option[Double], confidence: Option[Double])

final case class Usage(inputTokens: Long, outputTokens: Long) {
  def +(o: Usage): Usage = Usage(inputTokens + o.inputTokens, outputTokens + o.outputTokens)
}
object Usage {
  val Zero: Usage = Usage(0L, 0L)
}

final case class BackendResult(answers: Map[String, Answer], usage: Usage)

sealed abstract class BackendError extends Product with Serializable {
  def message: String
}
object BackendError {

  /** The request was too large; the selector halves the questions and asks again. */
  final case class TooLarge(message: String) extends BackendError

  /** Missing or rejected credentials. Retrying does not help. */
  final case class Auth(message: String) extends BackendError

  /** Anything else that is left after the backend's own retries. */
  final case class Failed(message: String) extends BackendError
}

/** A decision engine that scores how likely a change is to alter each test class's outcome.
  *
  * Every backend answers on the same scale so one gate works for all of them: `score` is 0 to 3 along
  * [[ScoreRubric.levels]] (continuous), `confidence` is 0 to 1.
  */
trait DecisionBackend {
  def id: String
  def model: String
  def maxQuestionsPerCall: Int

  /** One request. HTTP-level retries (5xx, network, 429) happen inside. */
  def judge(state: DiffState, questions: Vector[SuiteQuestion]): Either[BackendError, BackendResult]
}

/** Builds a backend at selection time, so credentials are read from the environment then and never stored in
  * settings. A Left is the reason to run every test, e.g. "no API key; set TYPESAFE_API_KEY".
  */
trait BackendFactory {
  def id: String
  def model: String
  def create(env: Env): Either[String, DecisionBackend]
}

/** Environment access, injectable for tests. */
trait Env {
  def get(name: String): Option[String]

  /** The first of `names` that is set and non-blank. */
  def first(names: Seq[String]): Option[(String, String)] =
    names.iterator.flatMap(n => get(n).map(_.trim).filter(_.nonEmpty).map(n -> _)).toSeq.headOption
}
object Env {
  object System extends Env {
    def get(name: String): Option[String] = Option(java.lang.System.getenv(name))
  }
  def of(values: Map[String, String]): Env = new Env {
    def get(name: String): Option[String] = values.get(name)
  }
}

trait Log {
  def info(msg: String): Unit
  def debug(msg: String): Unit
}
object Log {
  object Silent extends Log {
    def info(msg: String): Unit = ()
    def debug(msg: String): Unit = ()
  }
}
