val scala3Version = "3.1.3"

lazy val root = project
  .in(file("."))
  .settings(
    name := "splooge3",
    version := "0.1.0-SNAPSHOT",
    scalacOptions += "-old-syntax",
    scalacOptions += "-no-indent",
    scalaVersion := scala3Version,
    Compile / run / fork := true,
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,
    libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "3.0.0",
    libraryDependencies += "org.apache.xmlgraphics" % "batik" % "1.15",
    libraryDependencies += "org.apache.xmlgraphics" % "batik-swing" % "1.15",
    libraryDependencies += "org.apache.xmlgraphics" % "batik-transcoder" % "1.15",
    libraryDependencies += "com.google.jimfs" % "jimfs" % "1.2",
  )
