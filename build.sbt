import ReleaseTransformations._
scalaVersion := "3.1.2"
ThisBuild / organization := "com.jeekrs"

releaseUseGlobalVersion := false
Global / onChangedBuildSource := ReloadOnSourceChanges

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
resolvers += "jitpack" at "https://jitpack.io"

releaseProcess := Seq[ReleaseStep](
  inquireVersions,
  runClean,
  setReleaseVersion,
  commitReleaseVersion,
  tagRelease,
  setNextVersion,
  commitNextVersion
)

enablePlugins(NativeImagePlugin)

nativeImageOptions ++= List(
  s"-H:ConfigurationFileDirectories=${target.value / "native-image-configs"}",
  s"-H:ReflectionConfigurationFiles=${target.value / "native-image-configs" / "reflect-config.json"}",
  s"-H:ResourceConfigurationFiles=${target.value / "native-image-configs" / "resource-config.json"}",
  "-H:+JNI",
  "--no-fallback",
  "--allow-incomplete-classpath",
  "--no-server"
)

libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5"
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.11"

// https://mvnrepository.com/artifact/org.apache.commons/commons-lang3
libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.12.0"

// https://mvnrepository.com/artifact/org.apache.commons/commons-text
libraryDependencies += "org.apache.commons" % "commons-text" % "1.9"

// https://mvnrepository.com/artifact/org.junit.jupiter/junit-jupiter-api
libraryDependencies += "org.junit.jupiter" % "junit-jupiter-api" % "5.9.0" % Test

// https://mvnrepository.com/artifact/org.antlr/antlr4-runtime
libraryDependencies += "org.antlr" % "antlr4-runtime" % "4.11.1"
