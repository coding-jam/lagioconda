name := """play-scala"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file("."))
  .settings(
    javaOptions += "-Xmx3g"
  )
  .enablePlugins(PlayScala)

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  jdbc,
  cache,
  ws,
  "org.scalatestplus.play" %% "scalatestplus-play" % "1.5.1" % Test
)

libraryDependencies ++= Seq(
  "org.bytedeco" % "javacv-platform" % "1.3.1",
  "ch.qos.logback" % "logback-classic" % "1.1.3"
)

//compile in Compile <<= (compile in Compile).dependsOn(scalafmt)

javaOptions ++= Seq("-Xmx4g")
