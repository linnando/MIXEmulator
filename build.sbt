name := "MIX Emulator"

scalaVersion in ThisBuild := "2.11.11"

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
      "org.specs2" %% "specs2-core" % "4.3.4" % "test",
      "org.specs2" %% "specs2-matcher-extra" % "4.3.4" % "test"
    )
  )
  .jsSettings()

lazy val asmJVM = asm.jvm

lazy val asmJS = asm.js

lazy val vm = crossProject
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name := "vm",
    organization := "org.linnando",
    version := "0.1-SNAPSHOT"
  )
  .jvmSettings(
    scalacOptions in Test ++= Seq("-Yrangepos"),
    libraryDependencies ++= Seq(
      "org.specs2" %% "specs2-core" % "4.3.4" % "test",
      "org.specs2" %% "specs2-matcher-extra" % "4.3.4" % "test"
    )
  )
  .jsSettings(
    resolvers += Resolver.sonatypeRepo("releases"),
    libraryDependencies ++= Seq(
      "io.scalajs" %%% "nodejs-lts" % "0.4.2"
    )
  )

lazy val vmJVM = vm.jvm

lazy val vmJS = vm.js

lazy val webapp = project
  .enablePlugins(ScalaJSPlugin, Angulate2Plugin)
  .dependsOn(asmJS, vmJS)
  .settings(
    name := "webapp",
    organization := "org.linnando",
    version := "0.1-SNAPSHOT",
    resolvers += "amateras-repo" at "http://amateras.sourceforge.jp/mvn/",
    libraryDependencies ++= Seq(
      "com.github.karasiq" %%% "scalajs-bootstrap-v4" % "2.3.4",
      "com.scalawarrior" %%% "scalajs-ace" % "0.0.4"
    ),
    ngBootstrap := Some("org.linnando.mixemulator.webapp.AppModule")
  )

val stage = taskKey[Unit]("Stage task")

stage := (fullOptJS in (webapp, Compile)).value
