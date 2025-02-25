ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.1"

ThisBuild / libraryDependencies ++= Seq(
  "dev.zio" %% "zio" % "2.1.15",
  "dev.zio" %% "zio-streams" % "2.1.15"
)

lazy val root = (project in file("."))
  .settings(
    name := "NemProject"
  )
