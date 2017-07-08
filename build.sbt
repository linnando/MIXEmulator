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
    platformTarget in Android := "android-26",
    buildToolsVersion in Android := Some("26.0.0"),
    minSdkVersion in Android := "15",
    versionCode := Some(1),
    version := "0.1-SNAPSHOT",
    instrumentTestRunner := "android.support.test.runner.AndroidJUnitRunner",
    libraryDependencies ++= Seq(
      "com.android.support" % "appcompat-v7" % "25.3.1",
      "com.android.support.test" % "runner" % "0.5" % "androidTest",
      "com.android.support.test.espresso" % "espresso-core" % "2.2.2" % "androidTest"
    )
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
