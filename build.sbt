import java.nio.file.Files

val scala3Version = "3.2.0"

ThisBuild / scalaVersion := scala3Version
ThisBuild / scalacOptions += "-old-syntax"
ThisBuild / scalacOptions += "-no-indent"
lazy val core = crossProject(JVMPlatform, JSPlatform)
  .in(file("core"))
  .enablePlugins(ScalablyTypedConverterPlugin)
  .settings(
    name := "splooge_core",
    version := "0.1.0-SNAPSHOT",
    libraryDependencies += "org.typelevel" %%% "cats-effect" % "3.4.8",
  ).jsSettings(
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.4.0",
    Compile / npmDependencies += "@types/css-font-loading-module" -> "0.0.8",
  )
lazy val swing = project
  .in(file("swing"))
  .dependsOn(core.jvm)
  .settings(
    name := "splooge3",
    version := "0.1.0-SNAPSHOT",
    Compile / run / fork := true,
    Compile / resourceDirectory := file("resources"),
    libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "3.0.0",
    libraryDependencies += "org.apache.xmlgraphics" % "batik" % "1.15",
    libraryDependencies += "org.apache.xmlgraphics" % "batik-swing" % "1.15",
    libraryDependencies += "org.apache.xmlgraphics" % "batik-transcoder" % "1.15",
    libraryDependencies += "com.google.jimfs" % "jimfs" % "1.2",
    libraryDependencies += "org.typelevel" %% "cats-effect" % "3.4.8",

  )
val copyToTarget = TaskKey[Unit]("copyToTarget")

val fastBuild = TaskKey[Unit]("fastBuild")
val fullBuild = TaskKey[Unit]("fullBuild")
val fastRemove = TaskKey[Unit]("fastRemove")
ThisBuild / fastRemove := IO.delete(new File((web / Compile / fastLinkJSOutput).value, "resources"))
lazy val web = project
  .enablePlugins(ScalaJSPlugin)
  .enablePlugins(ScalablyTypedConverterPlugin)
  .in(file("web"))
  .dependsOn(core.js)
  .settings(
    name := "splooge_web",
    version := "0.1.0-SNAPSHOT",
    libraryDependencies += "com.armanbilge" %%% "calico" % "0.2.0-RC1",
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.4.0",
    scalaJSUseMainModuleInitializer := true,
    copyToTarget := {
      (Compile / fastLinkJS).value
      IO.copyDirectory(file("resources"), new File((Compile / fastLinkJSOutput).value, "resources"))
      IO.copyFile(new File(baseDirectory.value, "index.html"), new File((Compile / fastLinkJSOutput).value, "index.html"))
    }
  )
lazy val root = project
  .aggregate(core.jvm, swing)

ThisBuild / fastBuild := {
  Def.sequential(
  ThisBuild / fastRemove,
  web / Compile / copyToTarget
  ).value
}
