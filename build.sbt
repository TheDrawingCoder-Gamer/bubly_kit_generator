val scala3Version = "3.1.3"

lazy val core = crossProject(JVMPlatform, JSPlatform)
  .in(file("core"))
  .settings(
    name := "splooge_core",
    version := "0.1.0-SNAPSHOT",
    scalacOptions += "-old-syntax",
    scalacOptions += "-no-indent",
    scalaVersion := scala3Version,
  ).jsSettings(
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.4.0"
  )
lazy val swing = project
  .in(file("swing"))
  .dependsOn(core.jvm)
  .settings(
    name := "splooge3",
    version := "0.1.0-SNAPSHOT",
    scalacOptions += "-old-syntax",
    scalacOptions += "-no-indent",
    scalaVersion := scala3Version,
    Compile / run / fork := true,
    Compile / resourceDirectory := file("resources"),
    libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "3.0.0",
    libraryDependencies += "org.apache.xmlgraphics" % "batik" % "1.15",
    libraryDependencies += "org.apache.xmlgraphics" % "batik-swing" % "1.15",
    libraryDependencies += "org.apache.xmlgraphics" % "batik-transcoder" % "1.15",
    libraryDependencies += "com.google.jimfs" % "jimfs" % "1.2",

  )

lazy val web = project
  .enablePlugins(ScalaJSPlugin)
  .in(file("web"))
  .settings(
    name := "splooge_web",
    version := "0.1.0-SNAPSHOT",
    scalacOptions += "-old-syntax",
    scalacOptions += "-no-indent",
    libraryDependencies += "com.armanbilge" %%% "calico" % "0.2.0-RC1",
    scalaJSUseMainModuleInitializer := true,
  )
lazy val root = project
  .aggregate(core.jvm, swing)
