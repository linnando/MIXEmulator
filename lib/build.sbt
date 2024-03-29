name := "MIX Emulator Library"

ThisBuild / scalaVersion := "2.13.10"

lazy val buildNpm = taskKey[Unit]("Build NPM package")

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
    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.ESModule)
    },
    Compile / buildNpm := {
      val srcDirectory = (Compile / sourceDirectory).value / "typescript"
      val targetDirectory = (Compile / fastLinkJS / scalaJSLinkerOutputDirectory).value
      val modules = (Compile / fastLinkJS).value.data.publicModules
      val srcFiles = modules.map(module => {
        val jsFileName = module.jsFileName
        val dtsFileName = jsFileName.replaceFirst("\\.js$", ".d.ts")
        val srcFile = srcDirectory / dtsFileName
        if (!srcFile.exists()) {
          throw new java.io.FileNotFoundException(srcFile.getPath)
        }
        srcFile
      })
      val pairs = srcFiles pair Path.rebase(srcDirectory, targetDirectory)
      IO.copy(pairs, CopyOptions(overwrite = true, preserveLastModified = false, preserveExecutable = false))
    }
  )
