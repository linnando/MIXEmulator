name := "MIX Emulator"

scalaVersion in ThisBuild := "2.11.8"

lazy val asm = crossProject
  .dependsOn(vm)
  .settings(
    name := "asm",
    organization := "org.linnando",
    version := "0.1-SNAPSHOT"
  )
  .jvmSettings(
    scalacOptions in Test ++= Seq("-Yrangepos"),
    libraryDependencies ++= Seq(
      "org.specs2" %% "specs2-core" % "3.9.2" % "test",
      "org.specs2" %% "specs2-matcher-extra" % "3.9.2" % "test"
    )
  )
  .jsSettings()

lazy val asmJVM = asm.jvm

lazy val asmJS = asm.js

lazy val vm = crossProject
  .settings(
    name := "vm",
    organization := "org.linnando",
    version := "0.1-SNAPSHOT"
  )
  .jvmSettings(
    scalacOptions in Test ++= Seq("-Yrangepos"),
    libraryDependencies ++= Seq(
      "org.specs2" %% "specs2-core" % "3.9.2" % "test",
      "org.specs2" %% "specs2-matcher-extra" % "3.9.2" % "test"
    )
  )
  .jsSettings()

lazy val vmJVM = vm.jvm

lazy val vmJS = vm.js

lazy val webapp = project
  .enablePlugins(ScalaJSPlugin, Angulate2Plugin)
  .dependsOn(asmJS, vmJS)
  .settings(
    name := "webapp",
    organization := "org.linnando",
    version := "0.1-SNAPSHOT",
    ngBootstrap := Some("org.linnando.mixemulator.webapp.AppModule")
  )

val stage = taskKey[Unit]("Stage task")

stage := (fullOptJS in (webapp, Compile)).value