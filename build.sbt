import java.awt.Color
import sbt.io.ExtensionFilter
import java.awt.image.BufferedImage
import java.nio.file.Files
import scala.collection.JavaConverters._

val scala3Version = "3.2.2"


import org.http4s.ember.server._
import cats.syntax.all._
import cats.effect.{IO => CIO}
import com.comcast.ip4s._
import org.http4s.server
import server.staticcontent._
def routes(path: String) = fileService[CIO](FileService.Config(path))
def servePage(dir: File): CIO[Nothing] = {
  val port = Port.fromInt(8000).get
  val bind = IpAddress.fromString("127.0.0.1").get
  EmberServerBuilder
    .default[CIO]
    .withHost(bind)
    .withPort(port)
    .withHttpApp(routes(dir.toPath.toString).orNotFound)
    .build
    .evalTap(s =>
      CIO.println(
        s"Serving HTTP on ${s.addressIp4s.host} port ${s.addressIp4s.port} (http://${s.addressIp4s}/)"
      )
    )
    .useForever
}
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
    libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "3.0.0",
    libraryDependencies += "org.apache.xmlgraphics" % "batik" % "1.15",
    libraryDependencies += "org.apache.xmlgraphics" % "batik-swing" % "1.15",
    libraryDependencies += "org.apache.xmlgraphics" % "batik-transcoder" % "1.15",
    libraryDependencies += "com.google.jimfs" % "jimfs" % "1.2",
    libraryDependencies += "org.typelevel" %% "cats-effect" % "3.4.8",
    Compile / rasterizeSvgs := 
     { rasterizeSvgFiles((Compile / resourceManaged).value) },
    Compile / resourceGenerators += (Compile / rasterizeSvgs),

  )
lazy val swingio = project
  .in(file("swingio"))
  .dependsOn(core.jvm)
  .settings(
    name := "splooge_io",
    version := "0.1.0-SNAPSHOT",
    scalacOptions += "-source:future",
    libraryDependencies += "io.github.thedrawingcoder-gamer" %% "swing-io" % "0.1-c82eab4-SNAPSHOT",
    libraryDependencies += "org.typelevel" %% "cats-effect" % "3.4.8",
    Compile / rasterizeSvgs := 
     { rasterizeSvgFiles(( Compile / resourceManaged).value) },
    Compile / resourceGenerators += (Compile / rasterizeSvgs),
    
  )
val webTarget = settingKey[File]("webTarget")
val tearDownWeb = TaskKey[Unit]("tearDownWeb")
val linkJSDir = TaskKey[File]("linkJSDir")
val fastCopyWeb = TaskKey[Unit]("fastCopyWeb")
val fullCopyWeb = TaskKey[Unit]("fullCopyWeb")
val serve = TaskKey[Unit]("serve")

val fastBuild = TaskKey[Unit]("fastBuild")
val fullBuild = TaskKey[Unit]("fullBuild")

val rasterizeSvgs = TaskKey[Seq[File]]("rasterizeSvgs")
def tearDownImpl(targetDir: File) = {
    if (Files.exists(targetDir.toPath))
      IO.delete(targetDir)
}

def rasterizeSvgFiles(targetDir: File): Seq[File] = {
    rasterizeSvgImpl(targetDir)
    Files.walk(targetDir.asPath).iterator().asScala.map(_.toFile()).toSeq
}

val inkColor = new Color(58, 13, 205)
def rasterizeSvgImpl(targetDir: File) = {
    if (!Files.exists(targetDir.toPath))
      IO.createDirectory(targetDir)
    IO.copyDirectory(file("assets/resources"), targetDir)
    rasterize(file("assets/svgweapons"), targetDir, 256, 256)
    rasterize(file("assets/coloredweapons"), targetDir, 256, 256, getCss(inkColor))
    IO.copyDirectory(file("assets/coloredweapons"), targetDir)
}
def copyImpl(linkDir: File, targetDir: File, baseDir: File) = {
    if (!Files.exists(targetDir.toPath))
      IO.createDirectory(targetDir)
    IO.copyDirectory(linkDir, targetDir)
    rasterizeSvgImpl(new File(targetDir, "resources"))
    IO.copyDirectory(new File(baseDir, "web"), targetDir)
  }
ThisBuild / resolvers +=
  "Sonatype OSS Snapshots" at "https://s01.oss.sonatype.org/content/repositories/snapshots"
import cats.effect.unsafe.implicits.global
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
    },
    serve := {
      fastBuild.value
      servePage(webTarget.value).unsafeRunSync()
    }
  )
lazy val root = project
  .aggregate(core.jvm, swing, swingio)

def loadDocument(url : URL) = {
  import org.apache.batik.util.XMLResourceDescriptor 
  import org.apache.batik.anim.dom.SAXSVGDocumentFactory
  val parser = XMLResourceDescriptor.getXMLParserClassName()
  val f = new SAXSVGDocumentFactory(parser)
  val stream = url.openStream()
  val document = f.createSVGDocument("", stream) 
  document
}

def getCss(color: Color): String = {
  val magicColor = "#" + (color.getRGB() & 0xFFFFFF).toHexString
                        s"""
                          |.ink {
                          |  fill: $magicColor !important;
                          |}
                          |.inkstroke {
                          |  stroke: $magicColor !important;
                          |}
                          |#ink stop {
                          |  stop-color: $magicColor;
                          |}
                         """.stripMargin
}
def rasterize(source: File, file: File, w: Float, h: Float, extraCss: String = ""): Seq[File] = {
  import org.apache.batik.transcoder.image.{ImageTranscoder, PNGTranscoder}
  import org.apache.batik.transcoder.{TranscoderInput, TranscoderOutput, TranscodingHints, XMLAbstractTranscoder, SVGAbstractTranscoder}
  import org.apache.batik.anim.dom.SVGDOMImplementation
  import org.apache.batik.util.SVGConstants
  import java.nio.file.{Files, Paths}
  import scala.util.{Try, Success, Failure, Using}
  import scala.util.chaining._
  import com.google.common.jimfs.{Jimfs, Configuration}
  import java.util.UUID
  import javax.imageio.ImageIO
  if (source.isDirectory()) {
    val css = 
      """
      svg {
      shape-rendering: geometricPrecision;
      text-rendering:  geometricPrecision;
      color-rendering: optimizeQuality;
      image-rendering: optimizeQuality;
      }
      """ + extraCss
    IO.createDirectory(file)

    val cssFile = new File(file, "egg.css")
    Files.writeString(cssFile.asPath, css)
    val out = Path.selectSubpaths(source, new ExtensionFilter("svg")).unzip._1.map { svg =>
      val daSvg = loadDocument(svg.asURL)
      var daImage: Option[BufferedImage] = None
      val hints = new TranscodingHints()
      hints.put(XMLAbstractTranscoder.KEY_XML_PARSER_VALIDATING, java.lang.Boolean.FALSE)
      hints.put(XMLAbstractTranscoder.KEY_DOM_IMPLEMENTATION, SVGDOMImplementation.getDOMImplementation())
      hints.put(XMLAbstractTranscoder.KEY_DOCUMENT_ELEMENT_NAMESPACE_URI, SVGConstants.SVG_NAMESPACE_URI)
      hints.put(SVGAbstractTranscoder.KEY_USER_STYLESHEET_URI, cssFile.toURI().toString())
      hints.put(SVGAbstractTranscoder.KEY_WIDTH, w)
      hints.put(SVGAbstractTranscoder.KEY_HEIGHT, h)

      val input = new TranscoderInput(daSvg)
     
      val t = new PNGTranscoder()

      val base = svg.base
      val res = Path.rebase(source, file)(new File(svg.getParentFile, base + ".png")).get
      IO.createDirectory(res.getParentFile())

      val bytestream = new java.io.FileOutputStream(res)
      val output = new TranscoderOutput(bytestream)

      t.setTranscodingHints(hints)
      t.transcode(input, output)
      
      
       
      res
      
    }.toSeq

    cssFile.delete()
    out 

  } else {
    Seq()
  }
}

