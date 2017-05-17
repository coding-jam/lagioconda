import NativePackagerHelper._


val commonSettings = Seq(
  organization := "it.codingjam",
  version := "0.0.1",
  scalaVersion := "2.11.8")

resolvers ++= Seq(
  "openImaJ repo" at "http://maven.openimaj.org",
  "Semantic Desktop" at "http://aperture.sourceforge.net/maven"
)


lazy val root = (project in file("."))
  .settings(
    name := """la-gioconda"""
  )
  .aggregate(common, frontend)


lazy val common = (project in file("common"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "common",
    resolvers ++= Seq(
      "openImaJ repo" at "http://maven.openimaj.org",
      "Semantic Desktop" at "http://aperture.sourceforge.net/maven"
    ),
    libraryDependencies ++= Dependencies.openimaj ++ Dependencies.common,
    commonSettings
  )


lazy val backend = (project in file("backend"))
  .settings(
  name := "backend",
    fork in run := true,
    javaOptions ++= Seq(
      "-Djava.library.path=" + (baseDirectory.value.getParentFile / "backend" / "sigar" ).getAbsolutePath,
      "-Xms128m", "-Xmx128m"),
    libraryDependencies ++= (Dependencies.common),
  commonSettings
  ).dependsOn(common)


lazy val frontend = (project in file("frontend"))
  .enablePlugins(PlayScala, BuildInfoPlugin, JavaAppPackaging)
  .settings(
    name := "frontend",
    routesGenerator := InjectedRoutesGenerator,
    libraryDependencies ++= (Dependencies.frontend  ++ Seq(filters, cache)),
    javaOptions ++= Seq(
      "-Djava.library.path=" + (baseDirectory.value.getParentFile / "backend" / "sigar" ).getAbsolutePath,
      "-Xms128m", "-Xmx1024m"),
    fork in run := true,
    mappings in Universal ++= directory(baseDirectory.value.getParentFile / "backend" / "sigar"),
    bashScriptExtraDefines ++= Seq(
      """declare -r sigar_dir="$(realpath "${app_home}/../sigar")"""",
      """addJava "-Djava.library.path=${sigar_dir}""""
    ),
    commonSettings
  ).dependsOn(common)


scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  jdbc,
  cache,
  ws,
  "org.scalatestplus.play" %% "scalatestplus-play" % "1.5.1" % Test
)

libraryDependencies ++= Seq(
  "ch.qos.logback" % "logback-classic" % "1.1.3"
)

//compile in Compile <<= (compile in Compile).dependsOn(scalafmt)

javaOptions ++= Seq("-Xmx4g")

PlayKeys.externalizeResources := false

routesGenerator := InjectedRoutesGenerator

scalacOptions in ThisBuild ++= Seq(
  "-target:jvm-1.8",
  "-encoding", "UTF-8",
  "-deprecation", // warning and location for usages of deprecated APIs
  "-feature", // warning and location for usages of features that should be imported explicitly
  "-unchecked", // additional warnings where generated code depends on assumptions
  "-Xlint", // recommended additional warnings
  "-Ywarn-adapted-args", // Warn if an argument list is modified to match the receiver
  "-Ywarn-value-discard", // Warn when non-Unit expression results are unused
  "-Ywarn-inaccessible",
  "-Ywarn-dead-code"
)



fork in run := true