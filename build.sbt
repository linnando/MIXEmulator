lazy val sharedSettings = Seq(
  organization := "org.linnando",
  scalaVersion := "2.11.8",
  javacOptions in Compile ++= "-source" :: "1.7" :: "-target" :: "1.7" :: Nil
)

lazy val androidapp = project
  .dependsOn(vm)
  .enablePlugins(AndroidApp)
  .settings(
    sharedSettings,
    android.useSupportVectors,
    platformTarget in Android := "android-25",
    buildToolsVersion in Android := Some("25.0.2"),
    minSdkVersion in Android := "15",
    versionCode := Some(1),
    version := "0.1-SNAPSHOT",
    instrumentTestRunner := "android.support.test.runner.AndroidJUnitRunner",
    libraryDependencies ++= Seq(
      "com.android.support" % "appcompat-v7" % "25.2.0",
      "com.android.support.test" % "runner" % "0.5" % "androidTest",
      "com.android.support.test.espresso" % "espresso-core" % "2.2.2" % "androidTest"
    )
  )

lazy val vm = project
  .settings(
    sharedSettings,
    exportJars := true,
    libraryDependencies ++= Seq(
      "org.specs2" %% "specs2-core" % "3.8.8" % "test"
    ),
    scalacOptions in Test ++= Seq("-Yrangepos")
  )
