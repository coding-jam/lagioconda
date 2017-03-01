name := """play-scala"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  jdbc,
  cache,
  ws,
  "org.scalatestplus.play" %% "scalatestplus-play" % "1.5.1" % Test
)

libraryDependencies += "org.bytedeco" % "javacv-platform" % "1.3.1"

//compile in Compile <<= (compile in Compile).dependsOn(scalafmt)

// fork a new JVM for 'test:run', but not 'run'
fork in run := true



javaOptions in run += "-XX:+HeapDumpOnOutOfMemoryError"