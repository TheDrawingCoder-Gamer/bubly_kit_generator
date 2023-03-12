import java.nio.file.Files

val scala3Version = "3.2.2"

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
lazy val swingio = project
  .in(file("swingio"))
  .dependsOn(core.jvm)
  .settings(
    name := "splooge_io",
    version := "0.1.0-SNAPSHOT",
    scalacOptions += "-source:future",
    libraryDependencies += "io.github.thedrawingcoder-gamer" %% "swing-io" % "0.1-066ba2a-SNAPSHOT",
    libraryDependencies += "org.typelevel" %% "cats-effect" % "3.4.8",
    Compile / resourceDirectory := file("resources")
  )
val webTarget = settingKey[File]("webTarget")
val tearDownWeb = TaskKey[Unit]("tearDownWeb")
val linkJSDir = TaskKey[File]("linkJSDir")
val fastCopyWeb = TaskKey[Unit]("fastCopyWeb")
val fullCopyWeb = TaskKey[Unit]("fullCopyWeb")

val fastBuild = TaskKey[Unit]("fastBuild")
val fullBuild = TaskKey[Unit]("fullBuild")

def tearDownImpl(targetDir: File) = {
    if (Files.exists(targetDir.toPath))
      IO.delete(targetDir)
}
def copyImpl(linkDir: File, targetDir: File, baseDir: File) = {
    if (!Files.exists(targetDir.toPath))
      IO.createDirectory(targetDir)
    IO.copyDirectory(linkDir, targetDir)
    IO.copyDirectory(file("resources"), new File(targetDir, "resources"))
    IO.copyDirectory(new File(baseDir, "web"), targetDir)
  }
ThisBuild / resolvers +=
  "Sonatype OSS Snapshots" at "https://s01.oss.sonatype.org/content/repositories/snapshots"
lazy val web = project
  .enablePlugins(ScalaJSPlugin)
  .enablePlugins(ScalablyTypedConverterPlugin)
  .in(file("web"))
  .dependsOn(core.js)
  .settings(
    name := "splooge_web",
    version := "0.1.0-SNAPSHOT",
    libraryDependencies += "com.armanbilge" %%% "calico" % "0.2-1f61455-SNAPSHOT",
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.4.0",
    scalaJSUseMainModuleInitializer := true,
    webTarget := new File((Compile / target).value, "webpage"),
    Compile / fullLinkJS / scalaJSLinkerConfig ~= { _.withSourceMap(false) },
    fastCopyWeb := copyImpl((Compile / fastLinkJSOutput).value, webTarget.value, baseDirectory.value),
    fullCopyWeb := copyImpl((Compile / fullLinkJSOutput).value, webTarget.value, baseDirectory.value),
    tearDownWeb := tearDownImpl(webTarget.value),
    fastBuild := {
      Def.sequential(
        tearDownWeb,
        Compile / fastLinkJS,
        fastCopyWeb
      ).value 
    },
    fullBuild := {
      Def.sequential(
        tearDownWeb,
        Compile / fullLinkJS,
        fullCopyWeb,

      ).value
    }
  )
lazy val root = project
  .aggregate(core.jvm, swing, swingio)

