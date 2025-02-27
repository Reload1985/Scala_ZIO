ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.1"

ThisBuild / libraryDependencies ++= Seq(
  "dev.zio" %% "zio" % "2.1.15",
  "dev.zio" %% "zio-test" % "2.1.15" % Test,
  "dev.zio" %% "zio-test-sbt" % "2.1.15" % Test,
  "dev.zio" %% "zio-streams" % "2.1.15",
  "dev.zio" %% "zio-test-junit" % "2.1.15"
)

testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")

lazy val root = (project in file("."))
  .settings(
    name := "NemProject"
  )
