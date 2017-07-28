lazy val sharedSettings = Seq(
  organization := "org.linnando",
  scalaVersion := "2.11.8",
  javacOptions in Compile ++= "-source" :: "1.7" :: "-target" :: "1.7" :: Nil
)

lazy val asm = project
  .dependsOn(vm)
  .settings(
    sharedSettings,
    exportJars := true,
    libraryDependencies ++= Seq(
      "org.specs2" %% "specs2-core" % "3.9.2" % "test",
      "org.specs2" %% "specs2-matcher-extra" % "3.9.2" % "test"
    ),
    scalacOptions in Test ++= Seq("-Yrangepos")
  )

lazy val vm = project
  .settings(
    sharedSettings,
    exportJars := true,
    libraryDependencies ++= Seq(
      "org.specs2" %% "specs2-core" % "3.9.2" % "test",
      "org.specs2" %% "specs2-matcher-extra" % "3.9.2" % "test"
    ),
    scalacOptions in Test ++= Seq("-Yrangepos")
  )

lazy val webapp = project
  .dependsOn(asm, vm)
  .enablePlugins(ScalaJSPlugin)
  .enablePlugins(Angulate2Plugin)
  .settings(
    sharedSettings,
    name := "MIX Emulator Web",
    ngBootstrap := Some("org.linnando.mixemulator.webapp.AppModule"),
    scalacOptions in Test ++= Seq("-Yrangepos")
  )

val stage = taskKey[Unit]("Stage task")

stage := {
  (systemJS in (webapp, Compile, fullOptJS)).value
  (fullOptJS in (webapp, Compile)).value
}
