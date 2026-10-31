package com.krrrr38.decisionfilter.select

/** The question every backend asks about each test class, independent of how the backend encodes it.
  *
  * The selection threshold is deliberately not part of the question, so changing the gate never changes what is
  * asked.
  */
object ScoreRubric {

  final case class Level(label: String, description: String)

  val reviewing: String = "a git diff of a multi-module sbt project"

  val task: String =
    "The state is a git diff. Judge only the one test class identified below, and only for whether this change " +
      "can alter the outcome of any of its tests -- other problems with the tests or with the change are not your " +
      "concern here. The listed test names are a best-effort hint and may be incomplete."

  val levels: Vector[Level] = Vector(
    Level("unrelated", "Unrelated to this change: running these tests cannot produce a different result than before."),
    Level(
      "unaffected",
      "These tests exercise code the change touched, but nothing in the change can alter their outcome."
    ),
    Level("at-risk", "These tests could be affected by the change: some might fail."),
    Level(
      "likely-failing",
      "These tests directly exercise behaviour the change altered or broke: some are likely to fail."
    )
  )

  /** The fields that identify the test class. */
  def subject(q: SuiteQuestion): Vector[(String, Either[String, Vector[String]])] = Vector(
    "subject" -> Left(q.id),
    "module" -> Left(q.suite.module),
    "test_class" -> Left(q.suite.suiteId),
    "test_file" -> Left(q.suite.file),
    "tests" -> Right(q.suite.testNames)
  )
}
