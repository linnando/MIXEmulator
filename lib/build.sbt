name := "MIX Emulator Library"

ThisBuild / scalaVersion := "2.13.10"

lazy val root = project.in(file(".")).
  aggregate(lib.js, lib.jvm).
  settings()

lazy val lib = crossProject(JSPlatform, JVMPlatform).in(file(".")).
  enablePlugins(ScalaJSPlugin).
  settings(
    name := "lib",
    organization := "org.linnando",
    version := "0.1-SNAPSHOT"
  ).
  jvmSettings(
    Test / scalacOptions ++= Seq("-Yrangepos"),
    libraryDependencies ++= Seq(
      "org.specs2" %% "specs2-core" % "4.19.0" % "test",
      "org.specs2" %% "specs2-matcher-extra" % "4.19.0" % "test"
    )
  ).
  jsSettings(
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) }
  )
