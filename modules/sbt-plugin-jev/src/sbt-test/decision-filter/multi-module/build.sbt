import com.krrrr38.decisionfilter.select._

ThisBuild / scalaVersion := "2.13.18"
ThisBuild / decisionFilterBase := Some("base")

// Stand-in for a real API: classes whose name contains "Affected" are at risk, everything else is unrelated.
ThisBuild / decisionFilterBackends := Seq(new BackendFactory {
  val id = "fake"
  val model = "fake-1"
  def create(env: Env): Either[String, DecisionBackend] = Right(new DecisionBackend {
    val id = "fake"
    val model = "fake-1"
    val maxQuestionsPerCall = 400
    def judge(state: DiffState, qs: Vector[SuiteQuestion]): Either[BackendError, BackendResult] =
      Right(BackendResult(
        qs.map(q => q.id -> Answer(Some(if (q.suite.suiteId.contains("Affected")) 3.0 else 0.0), Some(1.0))).toMap,
        Usage(10, 1)
      ))
  })
})

val testDeps = Seq("org.scalatest" %% "scalatest" % "3.2.20" % Test)

lazy val a = project.settings(libraryDependencies ++= testDeps, Test / fork := true)
lazy val b = project.dependsOn(a).settings(libraryDependencies ++= testDeps, Test / fork := false)
lazy val root = (project in file(".")).aggregate(a, b)
