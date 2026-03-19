val scala3Version = "3.7.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "Mohs",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "org.typelevel" %% "cats-core" % "2.13.0",

    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0"
   
  )
