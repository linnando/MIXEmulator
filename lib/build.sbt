name := "MIX Emulator Library"

ThisBuild / scalaVersion := "2.13.15"

lazy val buildNpm = taskKey[Unit]("Build NPM package")

lazy val lib = crossProject(JSPlatform, JVMPlatform).in(file(".")).
  settings(
    name := "lib",
    organization := "org.linnando",
    version := "0.1-SNAPSHOT",
    libraryDependencies ++= Seq(
      "org.scalatest" %%% "scalatest" % "3.2.14" % "test",
      "org.scalatest" %%% "scalatest-wordspec" % "3.2.14" % "test"
    )
  ).
  jvmSettings(
    Test / scalacOptions ++= Seq("-Yrangepos"),
    libraryDependencies ++= Seq(
      "org.scala-js" %% "scalajs-library" % "1.17.0",
    )
  ).
  jsSettings(
    scalaJSLinkerConfig ~= {
      import org.scalajs.linker.interface.OutputPatterns
      _.withModuleKind(ModuleKind.ESModule).withOutputPatterns(OutputPatterns.fromJSFile("%s.mjs"))
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
