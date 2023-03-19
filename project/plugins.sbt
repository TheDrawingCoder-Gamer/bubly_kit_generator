addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.13.0")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.0.0")
addSbtPlugin("org.scalablytyped.converter" % "sbt-converter" % "1.0.0-beta41")

val http4sVersion = "0.23.18"
libraryDependencies += "org.apache.xmlgraphics" % "batik" % "1.15"
libraryDependencies += "org.apache.xmlgraphics" % "batik-transcoder" % "1.15"
libraryDependencies += "com.google.jimfs" % "jimfs" % "1.2"
libraryDependencies += "org.http4s" %% "http4s-ember-server" % http4sVersion
libraryDependencies += "org.http4s" %% "http4s-dsl"          % http4sVersion
