addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.13.0")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.0.0")
addSbtPlugin("org.scalablytyped.converter" % "sbt-converter" % "1.0.0-beta41")

val http4sVersion = "0.23.18"
val batikVersion = "1.17"
libraryDependencies += "org.apache.xmlgraphics" % "batik" % batikVersion
libraryDependencies += "org.apache.xmlgraphics" % "batik-transcoder" % batikVersion
libraryDependencies += "org.apache.xmlgraphics" % "batik-codec" % batikVersion
libraryDependencies += "com.google.jimfs" % "jimfs" % "1.2"
libraryDependencies += "org.http4s" %% "http4s-ember-server" % http4sVersion
libraryDependencies += "org.http4s" %% "http4s-dsl"          % http4sVersion
