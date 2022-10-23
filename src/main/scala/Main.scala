

import collection.immutable.ListMap
import javax.imageio.ImageIO;
import java.io.File;
case class ItemLocation(val path : String, val internal : Boolean) {
  def asBufferedImage = 
    if (internal) 
      ImageIO.read(this.getClass.getResourceAsStream(path))
    else 
      ImageIO.read(File(path))
}
enum Game(val name : String, val longName : String) {
  case Splatoon1 extends Game("s", "Splatoon")
  case Splatoon2 extends Game("s2", "Splatoon 2")
  case Splatoon3 extends Game("s3", "Splatoon 3")
  
}


case class WeaponStyle(val game : Game, val name : String)
case class Weapon(val rootPath : String,val name : String, val styles : Seq[WeaponStyle]) { 
  def path(style : WeaponStyle, twodim : Boolean) : ItemLocation = {
    val daName = name.replace(' ', '_').toLowerCase()
    val gStyle = style.name.trim().replace(' ', '_').toLowerCase()
    val gamePrefix = style.game.name
    val (noDimPath, goodPath) = {
      val firstPath = 
        if (gStyle == "") {
          gamePrefix + "_" + daName 
        } else {
          gamePrefix + "_" + gStyle + "_" + daName 
        }
      (s"$rootPath/$firstPath.png", s"$rootPath/${firstPath}_2d.png")
    }
    val daPath =
      if (twodim) {
        if (this.getClass.getResource(goodPath) == null) {
          noDimPath 
        } else {
          goodPath
        }
      } else {
        if (this.getClass.getResource(noDimPath) == null) {
          goodPath 
        } else {
          noDimPath
        }
      }
    Resource(daPath) 
  }
}


type AWeapon = Seq[WeaponStyle]
def MapToWeapon(daMap : Map[String, AWeapon], root : String) : Seq[Weapon] = 
  Seq.from(for ((k, styles) <- daMap) yield {
    Weapon(root, k, styles)
  })
// def ComplexWeapon(defGame : Game, styles : (String, Game)*) = AWeapon(Seq(styles:_*).map((a, b) => WeaponStyle(b, a)).prepended(WeaponStyle(defGame, "")))
def ComplexWeapon(styles : Seq[(String, Game)]*) = 
  Seq.concat(styles:_*).map( (a, b) => WeaponStyle(b, a))
def SimpleWeapon(game : Game) = Seq(WeaponStyle(game, ""))
def Simple3Weapon = SimpleWeapon(Game.Splatoon3)
def WeaponList(root : String, contents : (String, AWeapon)*) = 
  MapToWeapon(ListMap(contents:_*), root)
def Resource(path : String) = ItemLocation(path, true)

case class OtherWeapon(root : String, name : String, games : Seq[Game])
def OtherWeaponList(root : String, contents : (String, Seq[Game])*) : NonMainWeaponList = 
  contents.map((k, v) => OtherWeapon(root, k, v))
def DefStyles(games : Game*) = 
  games.map(game => ("", game))
def Style(name : String, games : Game*) = 
  games.map(game => (name, game))
type NonMainWeaponList = Seq[OtherWeapon]
object Mains { 
  import Game.*
  def allGamesDefStyle = DefStyles(Splatoon3, Splatoon2, Splatoon1)
  def allGames(style : String) = Style(style, Splatoon3, Splatoon2, Splatoon1)
  def oldGames(style : String) = Style(style, Splatoon2, Splatoon1)
  def newGames(style : String) = Style(style, Splatoon3, Splatoon2)
  val heroWeapon = Style("Hero", Splatoon2)
  val splatterscope = ComplexWeapon(
      allGames(""),
      Style("Kelp", Splatoon1),
      Style("Firefin", Splatoon2),
      Style("Kensa", Splatoon2),
      Style("Sheldon's Picks", Splatoon1)
    )
  val eliter = ComplexWeapon(
      allGames(""),
      oldGames("Custom")
    )
  val weapons = WeaponList("weapons",
    ("52 Gal", ComplexWeapon(
      allGamesDefStyle,
      oldGames("Deco")
    )),
    ("96 Gal", ComplexWeapon(
      allGamesDefStyle,
      oldGames("Deco")
    )),
    ("Aerospray", ComplexWeapon(
      allGamesDefStyle, 
      oldGames("Gold"),
      oldGames("Sheldon's Picks")
    )),
    ("Ballpoint Splatling", ComplexWeapon(
      newGames(""), 
      Style("Nouveau", Splatoon2)
    )),
    ("Bamboozler 14", ComplexWeapon(
      allGames(""),
      newGames("Grizzco"),
      oldGames("Cuttlegear"),
      oldGames("Sheldon's Picks")
      )),
    ("Blaster", ComplexWeapon(
      allGames(""),
      newGames("Grizzco"),
      heroWeapon
    )),
    ("Bloblobber", ComplexWeapon(
      newGames(""),
      Style("Deco", Splatoon2)
    )),
    ("Carbon Roller", ComplexWeapon(
      allGames(""),
      oldGames("Deco")
    )),
    ("Clash Blaster", ComplexWeapon(
      newGames(""),
      Style("Neo", Splatoon2)
    )),
    ("Dapple Dualies", ComplexWeapon(
      newGames(""),
      Style("Nouveau", Splatoon2),
      Style("Sheldon's Picks", Splatoon2)
    )),
    ("Dualie Squelchers", ComplexWeapon(
      newGames(""),
      Style("Custom", Splatoon2)
    )),
    ("Dual Squelcher", ComplexWeapon(
      DefStyles(Splatoon1),
      Style("Custom", Splatoon1)
      )),
    ("Dynamo Roller", ComplexWeapon(
      allGames(""),
      oldGames("Gold"),
      Style("Kensa", Splatoon2),
      Style("Sheldon's Picks", Splatoon1)
    )),
    ("E-Liter", eliter),
    ("E-Liter Scope", eliter),
    ("Explosher", ComplexWeapon(
      newGames(""),
      Style("Custom", Splatoon2)
    )),
    ("Flingza Roller", ComplexWeapon(
      newGames(""),
      Style("Foil", Splatoon2)
    )),
    ("Glooga Dualies", ComplexWeapon(
      DefStyles(Splatoon3, Splatoon2),
      Style("Deco",Splatoon3, Splatoon2),
      Style("Kensa", Splatoon2)
    )),
    ("Goo Tuber", ComplexWeapon(
      newGames(""),
      Style("Custom", Splatoon2)
    )),
    ("H-3 Nozzlenose", ComplexWeapon(
      allGames(""),
      oldGames("D")
    )),
    ("Heavy Splatling", ComplexWeapon(
      allGames(""),
      oldGames("Deco"),
      oldGames("Sheldon's Picks"),
      heroWeapon
    )),
    ("Hydra Splatling", ComplexWeapon(
      allGames(""),
      oldGames("Custom"),
    )),
    ("Inkbrush", ComplexWeapon(
      allGames(""),
      oldGames("Nouveau"),
      oldGames("Sheldon's Picks")
    )),
    ("Jet Squelcher", ComplexWeapon(
      allGames(""),
      oldGames("Custom")
    )),
    ("L-3 Nozzlenose", ComplexWeapon(
      allGames(""),
      oldGames("D")
    )),
    ("Luna Blaster", ComplexWeapon(
      allGames(""),
      oldGames("Neo")
    )),
    ("Mini Splatling", ComplexWeapon(
      allGames(""),
      oldGames("Zink"),
      Style("Kensa", Splatoon2),
      Style("Sheldon's Picks", Splatoon1)
    )),
    ("N-Zap", ComplexWeapon(
      DefStyles(Splatoon3, Splatoon2, Splatoon1),
      Style("89", Splatoon3, Splatoon2, Splatoon1),
      Style("Sheldon's Picks", Splatoon3, Splatoon2, Splatoon1)
    )
    ),
    ("Nautilus", ComplexWeapon(
      newGames(""),
      Style("Gold", Splatoon2)
    )),
    ("Octobrush", ComplexWeapon(
      allGames(""),
      oldGames("Nouveau"),
      heroWeapon
    )),
    ("Range Blaster", ComplexWeapon(
      DefStyles(Splatoon3, Splatoon2, Splatoon1),
      allGames("Sheldon's Picks")
    )),
    ("Rapid Blaster", ComplexWeapon(
      allGames(""),
      oldGames("Deco")
    )),
    ("Rapid Blaster Pro", ComplexWeapon(
      allGames(""),
      oldGames("Deco")
    )),
    ("REEF-LUX 450", Simple3Weapon),
    ("Slosher", ComplexWeapon(
      allGames(""),
      oldGames("Deco"),
      heroWeapon
    )),
    ("Sloshing Machine", ComplexWeapon(
      DefStyles(Splatoon3, Splatoon2, Splatoon1),
      Style("Grizzco", Splatoon3, Splatoon2),
      oldGames("Neo"),
      Style("Kensa", Splatoon2)
    )),
    ("Splash-o-matic", ComplexWeapon(
      allGames(""),
      allGames("Neo")
    )),
    ("Splat Brella", ComplexWeapon(
      DefStyles(Splatoon3, Splatoon2),
      Style("Grizzco", Splatoon3, Splatoon2),
      Style("Sorella", Splatoon2),
      heroWeapon
    )),
    ("Splat Charger", splatterscope ++ ComplexWeapon(oldGames("Hero"))),
    ("Splat Dualies", ComplexWeapon(
      DefStyles(Splatoon3, Splatoon2),
      Style("Kensa", Splatoon2),
      heroWeapon
      )),
    ("Splat Roller", ComplexWeapon(
      allGames(""),
      oldGames("Hero"),
      oldGames("Krak-on"),
      Style("Sheldon's Picks", Splatoon1),
      Style("Kensa", Splatoon2)
    )),
    ("Splatana Stamper", Simple3Weapon),
    ("Splatana Wiper", Simple3Weapon),
    ("Splatterscope", splatterscope),
    ("Splattershot", ComplexWeapon(
      DefStyles(Splatoon3, Splatoon2, Splatoon1),
      Style("Kensa", Splatoon2),
      allGames("Hero"),
      Style("Sheldon's Picks", Splatoon1),
      oldGames("Tentatek"),
      // BEST SPLATTERSHOT :heart:
      oldGames("Octo")
    )),
    ("Splattershot Jr", ComplexWeapon(
      allGames(""),
      oldGames("Custom"),
      Style("Kensa", Splatoon2)
    )),
    ("Splattershot Pro", ComplexWeapon(
      allGames(""),
      oldGames("Forge"),
      Style("Kensa", Splatoon2),
      Style("Sheldon's Picks", Splatoon1)
    )),
    ("Sploosh-o-matic", ComplexWeapon(
      allGames(""),
      oldGames("Neo"),

    )),
    ("Squeezer", ComplexWeapon(
      newGames(""),
      Style("Foil", Splatoon2)
    )),
    ("Squiffer", ComplexWeapon(
      allGames(""),
      oldGames("New"),
      oldGames("Sheldon's Picks")
    )),
    ("Tenta Brella", ComplexWeapon(
      newGames(""),
      Style("Sorella", Splatoon2),
      Style("Sheldon's Picks", Splatoon2)
    )),
    ("Tetra Dualies", ComplexWeapon(
      newGames(""),
      Style("Tentatek", Splatoon2)
    )),
    ("Tri-Slosher", ComplexWeapon(
      allGames(""),
      oldGames("Nouveau"),

    )),
    ("Tri-Stringer", ComplexWeapon(
      DefStyles(Splatoon3),
      Style("Grizzco", Splatoon3)
      )),
    ("Undercover Brella", ComplexWeapon(
      newGames(""),
      Style("Sorella", Splatoon2),
      Style("Kensa", Splatoon2)
    ))
  )
}


object Subs {
  import Game.*
  val weapons = OtherWeaponList("sub",  
    ("Angle Shooter", Seq(Splatoon3)),
    ("Autobomb", Seq(Splatoon3, Splatoon2)),
    ("Burst Bomb", Seq(Splatoon3, Splatoon2, Splatoon1)),
    ("Curling Bomb", Seq(Splatoon3, Splatoon2)),
    ("Fizzy Bomb", Seq(Splatoon3, Splatoon2)),
    ("Ink Mine", Seq(Splatoon3, Splatoon2, Splatoon1)),
    ("Point Sensor", Seq(Splatoon3, Splatoon2, Splatoon1)),
    ("Smallfry", Seq(Splatoon3)),
    ("Splash Wall", Seq(Splatoon3, Splatoon2, Splatoon1)),
    ("Splat Bomb", Seq(Splatoon3, Splatoon2, Splatoon1)), 
    ("Sprinkler", Seq(Splatoon3, Splatoon2, Splatoon1)),
    ("Squid Beakon", Seq(Splatoon3, Splatoon2, Splatoon1)),
    ("Suction Bomb", Seq(Splatoon3, Splatoon2, Splatoon1)),
    ("Torpedo", Seq(Splatoon3, Splatoon2)),
    ("Toxic Mist", Seq(Splatoon3, Splatoon2)),
    ("Disruptor", Seq(Splatoon1)),
    ("Seeker", Seq(Splatoon3, Splatoon1))
    )
}
object Specials {
  import Game.* 

  val weapons = OtherWeaponList("specials",
      ("Big Bubbler", Seq(Splatoon3)),
      ("Booyah Bomb", Seq(Splatoon3, Splatoon2)),
      ("Crab Tank", Seq(Splatoon3, Splatoon1)),
      ("Ink Storm", Seq(Splatoon3, Splatoon2, Splatoon1)),
      ("Ink Vac", Seq(Splatoon3, Splatoon1)),
      ("Inkjet", Seq(Splatoon3, Splatoon2, Splatoon1)),
      ("Killer Wail 5.1", Seq(Splatoon3, Splatoon1)),
      ("Reefslider", Seq(Splatoon3, Splatoon1)),
      ("Splashdown", Seq(Splatoon3, Splatoon2)),
      ("Tacticooler",  Seq(Splatoon3, Splatoon1)),
      ("Tenta Missiles", Seq(Splatoon3, Splatoon2, Splatoon1)),
      ("Triple Inkstrike", Seq(Splatoon3, Splatoon1)),
      ("Trizooka", Seq(
        Splatoon3,
        Splatoon2,
        Splatoon1
      )),
      ("Ultra Stamp", Seq(Splatoon3,Splatoon2,Splatoon1)),
      ("Wave Breaker", Seq(Splatoon3, Splatoon1)),
      ("Zipcaster", Seq(
        Splatoon3,
        Splatoon2,
        Splatoon1
      )),
      ("Autobomb Rush", Seq(Splatoon3)),
      ("Autobomb Launcher", Seq(Splatoon2)),
      ("Baller", Seq(
        Splatoon3, 
        Splatoon2
      )),
      ("Bubbler", Seq(
        Splatoon3, 
        Splatoon1
      )),
      ("Burst Bomb Rush", Seq(
        Splatoon3,
        Splatoon1
      )),
      ("Burst Bomb Launcher", Seq(
        Splatoon2
      )),
      ("Curling Bomb Rush", Seq(Splatoon3)),
      ("Curling Bomb Launcher", Seq(
        Splatoon2 
      )),
      ("Echolocator", Seq(
        Splatoon3, 
        Splatoon1
      )),
      ("Ink Armor", Seq(
        Splatoon3, 
        Splatoon2
      )),
      ("Inkstrike", Seq(
        Splatoon3,
        Splatoon1
      )),
      ("Killer Wail", Seq(
        Splatoon3,
        Splatoon1
      )),
      ("Kraken", Seq(
        Splatoon3,
        Splatoon1
      )),
      ("Seeker Bomb Rush", Seq(
        Splatoon3,
        Splatoon1
      )),
      ("Splat Bomb Rush", Seq(
        Splatoon3,
        Splatoon1
      )),
      ("Splat Bomb Launcher", Seq(
        Splatoon2
      )),
      ("Suction Bomb Rush", Seq(
        Splatoon3,
        Splatoon1
      )),
      ("Suction Bomb Launcher", Seq(
        Splatoon2
      )),
      ("Inkzooka", Seq(Splatoon3, Splatoon1)),
      ("Bubble Blower", Seq(Splatoon3, Splatoon2)),
      ("Sting Ray", Seq(Splatoon3, Splatoon2))

    )
}
object Brands {
  val brands = Seq(
      "89",
      "amiibo",
      "Annaki",
      "Barazushi",
      "Custom Star",
      "Custom White",
      "Custom Yellow",
      "Cuttlegear",
      "Cuttlegear Rectangle",
      "D",
      "Deco Black",
      "Deco Blue",
      "Deco Rhinestones",
      "Deco Yellow",
      "Emberz",
      "Enperry",
      "Enperry Crown",
      "Fishfry Red",
      "Fishfry Yellow",
      "Foil",
      "Foil Triangle",
      "Forge",
      "Gold",
      "Grizzco",
      "Inkline",
      "Inkline Blue",
      "Kelp",
      "Kensa",
      "Krak-on",
      "Krak-on Blue",
      "Neo",
      "Neo Gray",
      "New!",
      "Nouveau Blue",
      "Nouveau White",
      "Rockenberg",
      "Scalop",
      "Sheldon's Picks",
      "Sorella Fish",
      "Sorella Tartan",
      "Splashmob",
      "SquidForce",
      "Takoroka",
      "Tentatek",
      "Tentatek Light",
      "Tentatek Splatoon 1",
      "Zekko",
      "Zink Blue",
      "Zink White",
      "Zink Yellow"
    )
}
import java.awt.image.BufferedImage;
import java.awt.image.FilteredImageSource;
import java.awt.Graphics2D
import java.awt.Font
import java.awt.Color
import java.awt.RenderingHints
import java.awt.AlphaComposite 
import java.awt.geom.AffineTransform
import javax.swing.GrayFilter 
import scala.swing.* 
def gtkFileSelector(save : Boolean) : Option[File] = {
  import java.io.BufferedReader 
  import java.io.InputStreamReader
  import java.io.IOException
  val os = System.getProperty("os.name")
  var inputFile : Option[File] = None
  val zenityBase = "zenity --file-selection"
  if (os.indexOf("nux") != -1 || os.indexOf("nix") != -1) {
    try {
      val zenity = 
        if (save) {
          zenityBase + " --title=Save --save"
        } else {
          zenityBase + " --title=Open"
        }
      val p = Runtime.getRuntime().exec(zenity)
      val br = BufferedReader(InputStreamReader(p.getInputStream()))
      val sb = StringBuffer()
      var line = null
      sb.append(br.readLine())
      val filestring = sb.toString()
      if (filestring.equals("null")) {
        return None
      } else {
        return Some(File(filestring))
      }
    } catch {
      case e : IOException => return None 
    }

  } else {
      val fc = FileChooser()
      val returnVal = 
        if (save) {
          fc.showSaveDialog(fc)
        } else {
          fc.showOpenDialog(fc)
        }
      if (returnVal == FileChooser.Result.Approve) {
        return Some(fc.selectedFile)

      }
  }
  None

}
import java.io.{InputStream}
import java.net.URL
import org.w3c.dom.svg.SVGDocument

def loadDocument(url : URL) = {
  import org.apache.batik.util.XMLResourceDescriptor 
  import org.apache.batik.anim.dom.SAXSVGDocumentFactory
  val parser = XMLResourceDescriptor.getXMLParserClassName()
  val f = SAXSVGDocumentFactory(parser)
  val stream = url.openStream()
  val document = f.createSVGDocument("", stream) 
  document
}
def rasterize(svg : SVGDocument, width : Option[Float] = None, height : Option[Float] = None, extraCss : String = "") = {
  import org.apache.batik.transcoder.image.ImageTranscoder   
  import org.apache.batik.transcoder.{TranscoderInput, TranscoderOutput, TranscodingHints, XMLAbstractTranscoder, SVGAbstractTranscoder}
  import org.apache.batik.anim.dom.SVGDOMImplementation
  import org.apache.batik.util.SVGConstants
  import java.nio.file.{Files, Paths}
  import util.{Try, Success, Failure}
  // like from minecraft!
  import java.util.UUID
  import com.google.common.jimfs.{Jimfs, Configuration}
  var daimage : Option[BufferedImage] = None
  val css =
    """
    svg {
    shape-rendering: geometricPrecision;
    text-rendering:  geometricPrecision;
    color-rendering: optimizeQuality;
    image-rendering: optimizeQuality;
    }
    """ + extraCss
  // Jimfs SNATCHED MY WEAVE :heart_eyes:
  val fs = Try(Jimfs.newFileSystem(Configuration.unix()))
  fs.map {f => 
      val path = f.getPath(UUID.randomUUID().toString())
      Files.writeString(path, css) 
      val hints = TranscodingHints()
      // how sinful
      hints.put(XMLAbstractTranscoder.KEY_XML_PARSER_VALIDATING, java.lang.Boolean.FALSE)
      hints.put(XMLAbstractTranscoder.KEY_DOM_IMPLEMENTATION, SVGDOMImplementation.getDOMImplementation())
      hints.put(XMLAbstractTranscoder.KEY_DOCUMENT_ELEMENT_NAMESPACE_URI, SVGConstants.SVG_NAMESPACE_URI)
      hints.put(SVGAbstractTranscoder.KEY_USER_STYLESHEET_URI, path.toUri().toString())

      width match {
        case None => ()
        case Some(w) => hints.put(SVGAbstractTranscoder.KEY_WIDTH, w)
      }
      height match {
        case None => () 
        case Some(h) => hints.put(SVGAbstractTranscoder.KEY_HEIGHT, h)
      } 
      val input = TranscoderInput(svg)

      val t = new ImageTranscoder() {
        override def createImage(w : Int, h : Int) : BufferedImage = {
          BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB)
        }
        override def writeImage(image : BufferedImage, out : TranscoderOutput) : Unit = {
          println("hi")
          daimage = Some(image)
        }
      }
      t.setTranscodingHints(hints)
      t.transcode(input, TranscoderOutput())

      f.close()
      daimage
  }.get
  
  

}
trait KitFactory {
  protected def kit : BufferedImage
  protected def canvasSize : (Int, Int)
  protected def subSize : Int 
  protected def specialSize : Int 
  protected def kitWidth : Int 
  protected def kitPos : (Int, Int)
  protected def mainSize : Int 
  protected def mainPos : (Int, Int)
  protected def subPos : (Int, Int) 
  protected def specialPos : (Int, Int)
  protected def weaponFont : Font 
  protected def subFont : Font 
  protected def specialFont : Font 
  protected def spPointsFont : Font 
  protected def weaponTextPos : (Int, Int)
  protected def subTextPos : (Int, Int)
  protected def specialTextPos : (Int, Int)
  protected def spPointsTextPos : (Int, Int)
  protected def renderSubShadow : Boolean = false
  protected def renderSpecialShadow : Boolean = false 
  private def makeShadow(loadImg : BufferedImage) = {
    val img = new BufferedImage(loadImg.getWidth(), loadImg.getHeight(),
        BufferedImage.TYPE_INT_ARGB)
    val graphics = img.createGraphics()
    graphics.drawImage(loadImg, null, 0, 0)
    graphics.setComposite(AlphaComposite.SrcIn)
    graphics.setColor(Color.BLACK)
    graphics.fillRect(0, 0, loadImg.getWidth(), loadImg.getHeight())
    graphics.setColor(Color(0, 0, 0, 100))
    graphics.fillRect(0, 0, loadImg.getWidth(), loadImg.getHeight())
    graphics.dispose()
    img
  }
  def renderKit(mainName : String, mainImage : BufferedImage, subName : String, subImage : Either[SVGDocument, BufferedImage], 
    specialName : String, specialImage : Either[SVGDocument, BufferedImage], specialPoints : Option[String], color : Color, brand : Option[BufferedImage]) : BufferedImage = {
      import org.w3c.dom.Document 
      import org.w3c.dom.svg.SVGLength
      import org.apache.batik.anim.dom.SVGDOMImplementation
      import org.apache.batik.util.SVGConstants
      val (canvasW, canvasH) = canvasSize 
      val canvas = BufferedImage(canvasW,canvasH, BufferedImage.TYPE_INT_ARGB); 
      val g = canvas.createGraphics()
      val kit = this.kit
      val (mainX, mainY) = mainPos
      val (subX, subY) = subPos
      val (specialX, specialY) = specialPos
      val shadow = makeShadow(mainImage)
      val widthPng = mainSize
      val brandSize = widthPng / 2d
      val brandOffset = brandSize 
      val brandX = mainX + brandOffset 
      val brandY = mainY + brandOffset
     
      val widthSubPng = subSize 
      val widthSpecialPng = specialSize
      // use width on height to prevent STREEETCh
      val mainTransform = AffineTransform.getTranslateInstance(mainX, mainY)
      mainTransform.scale(widthPng.toDouble / mainImage.getWidth(), widthPng.toDouble / mainImage.getWidth())
      
      val transform = AffineTransform.getScaleInstance(widthPng.toDouble / shadow.getWidth(), (widthPng * 0.75) / shadow.getWidth())
      transform.preConcatenate(AffineTransform.getTranslateInstance(mainX, mainY + (widthPng.toDouble / 4)))
      val kitScaling = kitWidth.toDouble / kit.getWidth()
      val (kitX, kitY) = kitPos 
      val kitTransform = AffineTransform.getTranslateInstance(kitX, kitY) 
      kitTransform.scale(kitScaling, kitScaling)
      lazy val magicColor = {
          val r = color.getRed()
          val g = color.getGreen()
          val b = color.getBlue()
          s"rgb($r, $g, $b)" 
      }
      lazy val basedCss = 
        s"""
        .ink {

          fill: $magicColor !important;
        }
        .inkstroke {
          stroke: $magicColor !important;
        }
        #ink stop {
          stop-color: $magicColor;
        }
        """
  
      val sub = 
        subImage match 
        { case Left(doc) => rasterize(doc, Some(widthSubPng), Some(widthSubPng), basedCss).get
          case Right(img) => img
        }  
      val special = 
        specialImage match 
        { case Left(doc) => rasterize(doc, Some(widthSpecialPng), Some(widthSpecialPng), basedCss).get
          case Right(img) => img
        }
      val subTransform = AffineTransform.getTranslateInstance(subX, subY)
      val subScaling = widthSubPng.toDouble / sub.getWidth()
      subTransform.scale(subScaling, subScaling)
      val specialTransform = AffineTransform.getTranslateInstance(specialX, specialY)
      val specialScaling = widthSpecialPng.toDouble / special.getWidth()
      specialTransform.scale(specialScaling, specialScaling)
      g.setRenderingHint(
        RenderingHints.KEY_INTERPOLATION,
        RenderingHints.VALUE_INTERPOLATION_BILINEAR)
      g.drawImage(kit, kitTransform, null)
      g.drawImage(shadow, transform, null)
      g.drawImage(mainImage, mainTransform, null)
      brand match {
        case None => ()
        case Some(b) => { 
          val brandTransform = AffineTransform.getTranslateInstance(brandX, brandY)
          // let's stretch our legs :bangbang:
          brandTransform.scale(brandSize.toDouble / b.getWidth(), brandSize.toDouble / b.getWidth())
          g.drawImage(b, brandTransform, null)
        }
      }
      if (renderSubShadow) {
        val subShadow = makeShadow(sub) 
        val shadowTransform = AffineTransform(subTransform)
        shadowTransform.preConcatenate(AffineTransform.getTranslateInstance(0, widthSubPng.toDouble / 4))
        shadowTransform.scale(1, 0.75)
        g.drawImage(subShadow, shadowTransform, null)
      }
      if (renderSpecialShadow) {
        val spShadow = makeShadow(special) 
        val shadowTransform = AffineTransform(specialTransform)
        shadowTransform.preConcatenate(AffineTransform.getTranslateInstance(0, widthSubPng.toDouble / 4))
        shadowTransform.scale(1, 0.75)
        g.drawImage(spShadow, shadowTransform, null)
      }
      g.drawImage(sub, subTransform, null) 
      g.drawImage(special, specialTransform, null)
      g.setRenderingHint(
        RenderingHints.KEY_TEXT_ANTIALIASING,
        RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
      g.setFont(weaponFont)
      val fm = g.getFontMetrics()
      g.setColor(Color.WHITE)
      val (mainTxtX, mainTxtY) = weaponTextPos
      g.drawString(mainName, mainTxtX, mainTxtY + fm.getAscent())
      g.setFont(subFont)
      val fm2 = g.getFontMetrics()
      val (subFontX, subFontY) = subTextPos
      val (spFontX, spFontY) = specialTextPos
      g.drawString(subName, subFontX, subFontY + fm2.getAscent())
      g.setFont(specialFont)
      val fm3 = g.getFontMetrics().getAscent()
      g.drawString(specialName, spFontX, spFontY + fm3)
      specialPoints match { 
        case Some(p) =>
          g.setFont(spPointsFont)
          val pointsAscent = g.getFontMetrics().getAscent()
          val (spX, spY) = spPointsTextPos
          g.drawString(p + "p", spX, spY + pointsAscent)
        case None => ()
      }
      g.dispose()
      canvas
  }
}

object Splooge3KitGen extends KitFactory {
  // TODO: is holding an image in memory really worth it?
  override lazy val kit = ImageIO.read(this.getClass.getResourceAsStream("/ui/s3_kit_backdrop.png"))
  override val kitWidth = 676
  override val canvasSize = (686, 507)
  private val subSpSize = 62 
  override val mainSize = 170
  override val mainPos = (50, 135)
  override val subSize = subSpSize 
  override val specialSize = subSpSize
  private val subSpX = 268
  private val subY = 148 
  private val specialY = 235
  override val subPos = (subSpX, subY)
  override val specialPos = (subSpX, specialY)
  override val kitPos = (0, 0)
  private lazy val splooge1Font = Font.createFont(Font.TRUETYPE_FONT, this.getClass.getResourceAsStream("/font/splatoon1.otf"))
  private lazy val splooge2Font = Font.createFont(Font.TRUETYPE_FONT, this.getClass.getResourceAsStream("/font/splatoon2.otf"))
  private lazy val subSpFont = splooge2Font.deriveFont(Font.PLAIN, 25)
  override lazy val weaponFont = splooge1Font.deriveFont(Font.PLAIN, 32)
  override lazy val subFont = subSpFont 
  override lazy val specialFont = subSpFont 
  override lazy val spPointsFont = splooge2Font.deriveFont(Font.PLAIN, 32)
  override val weaponTextPos = (52, 25)
  private val subSpFontX = subSpX + 75
  override val subTextPos = (subSpFontX, subY)
  override val specialTextPos = (subSpFontX, specialY)
  override val spPointsTextPos = (475, 320)
}
object Splooge2KitGen extends KitFactory {
  // TODO: is holding an image in memory really worth it?
  override lazy val kit = ImageIO.read(this.getClass.getResourceAsStream("/ui/s2_kit_backdrop.png"))
  override val kitWidth = 676
  override val canvasSize = (686, 600)
  private val subSpSize = 100 
  override val mainSize = 150
  override val mainPos = (80, 260)
  override val subSize = subSpSize 
  override val specialSize = subSpSize
  private val subSpX = 280
  private val subY = 215
  private val specialY = 355
  override val subPos = (subSpX, subY)
  override val specialPos = (subSpX, specialY)
  override val kitPos = (0, 0)
  private lazy val splooge2Font = Font.createFont(Font.TRUETYPE_FONT, this.getClass.getResourceAsStream("/font/splatoon2.otf"))
  private lazy val subSpFont = splooge2Font.deriveFont(Font.PLAIN, 28)
  override lazy val weaponFont = splooge2Font.deriveFont(Font.PLAIN, 35)
  override lazy val subFont = subSpFont 
  override lazy val specialFont = subSpFont 
  override lazy val spPointsFont = splooge2Font.deriveFont(Font.PLAIN, 32)
  override val weaponTextPos = (70, 120)
  private val subSpFontX = subSpX + 150 
  private val textOffset = 50
  override val subTextPos = (subSpFontX, subY + textOffset)
  override val specialTextPos = (subSpFontX, specialY + textOffset)
  override val spPointsTextPos = (485, 485)
}
object Splooge1KitGen extends KitFactory {
  // TODO: is holding an image in memory really worth it?
  override lazy val kit = ImageIO.read(this.getClass.getResourceAsStream("/ui/s_kit_backdrop.png"))
  override val kitWidth = 676
  override val canvasSize = (686, 507)
  private val subSpSize = 64 
  override val mainSize = 125
  override val mainPos = (80, 120)
  override val subSize = subSpSize 
  override val specialSize = subSpSize
  private val subX = 240 
  private val specialX = 105
  private val subY = 185
  private val specialY = 275
  override val subPos = (subX, subY)
  override val specialPos = (specialX, specialY)
  override val kitPos = (0, 0)
  private lazy val splooge1Font = Font.createFont(Font.TRUETYPE_FONT, this.getClass.getResourceAsStream("/font/splatoon1.otf"))
  private lazy val michromaFont = Font.createFont(Font.TRUETYPE_FONT, this.getClass.getResourceAsStream("/font/michroma-regular.ttf"))
  private lazy val subSpFont = michromaFont.deriveFont(Font.PLAIN, 20)
  private val fontTransform = AffineTransform.getRotateInstance(Math.toRadians(-5), 0, 0)
  // me omw to derive ur mom
  override lazy val weaponFont = splooge1Font.deriveFont(Font.PLAIN, 32).deriveFont(fontTransform)
  override lazy val subFont = subSpFont 
  override lazy val specialFont = subSpFont 
  override lazy val spPointsFont = michromaFont.deriveFont(Font.PLAIN, 14)
  override val weaponTextPos = (52, 35)
  private val subFontX = subX + 90 
  private val specialFontX = specialX + 120 
  private val textYOff = 25 
  override val subTextPos = (subFontX, subY + textYOff + 5)
  override val specialTextPos = (specialFontX, specialY + textYOff)
  override val spPointsTextPos = (410, 367)
  override val renderSubShadow = true 
  override val renderSpecialShadow = true
}

import java.awt.Dimension
import javax.swing.ImageIcon
import javax.{swing => jswing}
abstract class GenericCellRenderer[T] extends jswing.JLabel with jswing.ListCellRenderer[T] {
  setOpaque(true)
  private val defaults = jswing.UIManager.getDefaults()
  private val bg = defaults.get("ComboBox.background").asInstanceOf[Color]
  private val fg = defaults.get("ComboBox.foreground").asInstanceOf[Color]
  private val selBg = defaults.get("ComboBox.selectionBackground").asInstanceOf[Color]
  private val selFg = defaults.get("ComboBox.selectionForeground").asInstanceOf[Color]
  def getTextOfValue(value : T) : String
  def getListCellRendererComponent(list: jswing.JList[_ <: T], value : T, index : Int, isSelected : Boolean, isFocused : Boolean) = {
    setText(getTextOfValue(value))
    if (isSelected) {
      setBackground(selBg)
      setForeground(selFg)
    } else {
      setBackground(bg)
      setForeground(fg)
    }
    this
  }

}
class OtherWeaponCellRenderer extends GenericCellRenderer[OtherWeapon] {
  override def getTextOfValue(other : OtherWeapon) = 
    Option(other).map(_.name).getOrElse("")
}
class GameCellRenderer extends GenericCellRenderer[Game] {
  override def getTextOfValue(value : Game) = 
    Option(value).map(_.longName).getOrElse("")
}
object SwingApp { 
  import net.bulbyvr.swing.svg.SVGComponent
  import org.apache.batik.swing.svg.JSVGComponent
  import org.apache.batik.swing.svg.{SVGDocumentLoaderListener, SVGDocumentLoaderEvent}
  def top = new MainFrame {
    private var safeDoc = false
    title = "Bubly Kit Generator"
    contents = {
      new BoxPanel(Orientation.Vertical) {
        
        val mainGroup = MainWeaponGroup(BufferedImage(1, 1, BufferedImage.TYPE_INT_ARGB), "Main", Mains.weapons) 
        val subGroup = OtherWeaponGroup(BufferedImage(1, 1, BufferedImage.TYPE_INT_ARGB),"Sub", Subs.weapons) 
        val specialGroup = OtherWeaponGroup(BufferedImage(1, 1, BufferedImage.TYPE_INT_ARGB),"Special", Specials.weapons)

        contents += mainGroup
        contents += subGroup 
        contents += specialGroup

        val doSvgMagic = CheckBox("Do SVG Ink")
        val colorPicker = ColorChooser(Color(0.1019608f,  0.1019608f, 0.6862745f))
        colorPicker.peer.setPreviewPanel(jswing.JPanel())
        contents += FlowPanel(doSvgMagic, colorPicker)
        val brandGroup = BrandGroup()
        contents += brandGroup
        val gameModel = ListView.Renderer.Wrapped(GameCellRenderer())
        val kitStyle = new ComboBox(Seq(Game.Splatoon3, Game.Splatoon2, Game.Splatoon1))
        kitStyle.renderer = gameModel
        contents += FlowPanel(kitStyle)
        contents += new Button("Generate!") {
          reactions += {
            case event.ButtonClicked(_) => 
              val mainImage = mainGroup.image
              val mainName = mainGroup.textField.text match { 
                case "" => mainGroup.selectedName.getOrElse("")
                case text => text
              }
              val subName = subGroup.textField.text match { 
                case "" => subGroup.selectedName.getOrElse("") 
                case text => text
              }
              val specialName = specialGroup.textField.text match { 
                case "" => specialGroup.selectedName.getOrElse("") 
                case text => text 
              }
              val (subImage, specialImage) = 
                if (doSvgMagic.selected) {
                  val subImage = subGroup.fetchImage.flatMap { p => 
                    val svgPath = p.path.replace(".png", ".svg")
                    Option(this.getClass.getResource(svgPath)).map(p => loadDocument(p))
                  }.toLeft(subGroup.image)
                  val specialImage = specialGroup.fetchImage.flatMap { sppath => 
                    val spsvgPath = sppath.path.replace(".png", ".svg")
                    Option(this.getClass.getResource(spsvgPath)).map(p => loadDocument(p))
                  }.toLeft(specialGroup.image)
                  (subImage, specialImage)
                } else {
                  (Right(subGroup.image), Right(specialGroup.image))
                }
              val brand = brandGroup.image 
              val renderer = kitStyle.selection.item match {
                case Game.Splatoon3 => Splooge3KitGen.renderKit 
                case Game.Splatoon2 => Splooge2KitGen.renderKit
                case Game.Splatoon1 => Splooge1KitGen.renderKit
              } 
              val kit = renderer(mainName, mainImage, subName, subImage, specialName, specialImage, None, colorPicker.color, brand)
              
              gtkFileSelector(true).foreach { it => 
                ImageIO.write(kit, "png", it)
              }
            
          }
        } 

      }
    }
  }
}

   
class BrandGroup extends FlowPanel {
  private var daimage : Option[BufferedImage] = None
  def image = daimage 
  def image_=(im  : Option[BufferedImage]) = {
    daimage = im
    im match {
      case None => 
        brandImageLabel.icon = null 
      case Some(i) => 
        val good = BufferedImage(64, 64, BufferedImage.TYPE_INT_ARGB)
        val g = good.createGraphics()
        g.setRenderingHint(
        RenderingHints.KEY_INTERPOLATION,
        RenderingHints.VALUE_INTERPOLATION_BILINEAR)
        g.drawImage(i, 0, 0, 64, 64, null)
        g.dispose()
        brandImageLabel.icon = ImageIcon(good)

    }
  }
  val brandImageLabel = new Label("", null, Alignment.Center)
  val brandDropdown = new ComboBox("None" +: Brands.brands) {
    selection.reactions += {
      case event.SelectionChanged(_) =>
        this.selection.item match {
          case "None" => image = None
          case i => 
            val res = i.toLowerCase().replace(' ', '_') 
            println(res)
            image = Some(ImageIO.read(this.getClass.getResourceAsStream(s"/brands/$res.png")))
        }
    }
  }
  contents += brandImageLabel 
  contents += brandDropdown

} 

class LabeledTextField(label : String) extends FlowPanel {
  contents += Label(label)
  val textField = new TextField(20)
  contents += textField
}
import net.bulbyvr.swing.MutableComboBox
trait WeaponGroup[W, S](protected var daimage  : BufferedImage, label : String, val weapons : Seq[W]) extends BoxPanel {
  def image = daimage 
  def image_=(i : BufferedImage) = {
    daimage = i
    val editedImage = BufferedImage(64, 64, BufferedImage.TYPE_INT_ARGB)
    val g = editedImage.createGraphics()
    g.setRenderingHint(
      RenderingHints.KEY_INTERPOLATION,
      RenderingHints.VALUE_INTERPOLATION_BILINEAR)
    g.drawImage(i, 0, 0, 64, 64, null)
    g.dispose()
    imagelabel.icon = ImageIcon(editedImage)
  }

  protected val labeledField = LabeledTextField(label)
  def fetchImage : Option[ItemLocation]
  val imagelabel = Label("", ImageIcon(daimage), Alignment.Center)
  val fileSelector = 
    new Button(s"Select $label image") {
      reactions += {
        case event.ButtonClicked(_) => 
          gtkFileSelector(false).foreach { it =>
            image = ImageIO.read(it)
          }
      }
    }
  val clearButton = 
    new Button(s"Clear $label image") {
      reactions += {
        case event.ButtonClicked(_) => 
          revertImage()
      }
    }

  def revertImage() = {
    fetchImage.foreach(it => image = it.asBufferedImage)
  }
  def textField = labeledField.textField
  def selectedName : Option[String]
  protected def weaponRenderer : ListView.Renderer[W]
  protected def styleRenderer : ListView.Renderer[S]
  protected def getWeaponStyles(weapon : W) : Seq[S] 
  // lazy to prevent sadness
  lazy val styleDropdown = {
    val item = new MutableComboBox[S](getWeaponStyles(weapons.head)) {
      selection.reactions += {
        case event.SelectionChanged(_) => 
          revertImage()
      }
    }
    item.renderer = styleRenderer
    item
  }
  lazy val weaponDropdown = { 
    val item = new ComboBox[W](weapons) {
      selection.reactions += {
        case event.SelectionChanged(_) => 
          styleDropdown.items.removeAllElements()
          styleDropdown.items ++= getWeaponStyles(selection.item)
      }
    }
    item.renderer = weaponRenderer
    item
  }
  protected val fileSelPanel = FlowPanel(fileSelector, clearButton)
  def addContents() = {
    contents += labeledField 
    contents += FlowPanel(imagelabel, weaponDropdown, styleDropdown)
    contents += fileSelPanel
    ()
  }
}
class MainWeaponGroup(d : BufferedImage, l : String, val daWeapons : Seq[Weapon]) extends BoxPanel(Orientation.Vertical) with WeaponGroup[Weapon, WeaponStyle](d, l, daWeapons) {
  
  def getPath(weapon : Weapon) = {
    val index = 
      if (this.styleDropdown.selection.index == -1) {
        0  
      } else {
        this.styleDropdown.selection.index
      }
    Some(weapon.path(weapon.styles.toSeq(index), flatcheckbox.selected))
  }

  val flatcheckbox = 
    new CheckBox("Flat Icons") {
      reactions += {
        case event.ButtonClicked(_) => 
          revertImage()
      }
    }
  override protected val weaponRenderer = ListView.Renderer.Wrapped(new GenericCellRenderer[Weapon]() {
    override def getTextOfValue(v : Weapon) = 
      Option(v).map(_.name).getOrElse("")
  })
  override protected val styleRenderer = ListView.Renderer.Wrapped(new GenericCellRenderer[WeaponStyle]() {
    override def getTextOfValue(s : WeaponStyle) = 
      Option(s) match {
        case Some(v) => s"${v.name} ${v.game.name}"
        case None => ""
      }
  })
 
  override protected def getWeaponStyles(w : Weapon) = 
    w.styles
  fileSelPanel.contents += flatcheckbox
  override def fetchImage = {
    getPath(weaponDropdown.selection.item)
  }
  override def selectedName = {
    Some(weaponDropdown.selection.item.name)
  }
  // HACK: lateinit
  addContents()
  
}
class OtherWeaponGroup(d : BufferedImage, l : String, w : Seq[OtherWeapon]) extends BoxPanel(Orientation.Vertical) with WeaponGroup[OtherWeapon, Game](d, l, w) {
  def currentWeapon = weaponDropdown.selection.item
  def pathHelper(weapon : OtherWeapon, game : Game) = {
    Some(Resource(weapon.root + "/" + game.name + "_" + weapon.name.replace(' ', '_').toLowerCase() + ".png"))

  }
  override protected val styleRenderer = ListView.Renderer.Wrapped(GameCellRenderer())
  def getPath(weapon : OtherWeapon) = {
    Option(styleDropdown.selection.item).flatMap(it => pathHelper(weapon, it))
  }
 
  override protected val weaponRenderer = ListView.Renderer.Wrapped(OtherWeaponCellRenderer())
  override def fetchImage = {
     getPath(currentWeapon)
  }
  override def selectedName = Option(currentWeapon).map(_.name)
  override def getWeaponStyles(w : OtherWeapon) = w.games
  // HACK : late init
  addContents()
}

import javax.swing.UIManager
@main def launchApp = { 
  UIManager.getInstalledLookAndFeels().find(it => it.getName() == "GTK+").foreach(it => UIManager.setLookAndFeel(it.getClassName()))
  val window = SwingApp.top
  window.centerOnScreen()
  window.open()
}

def testSplooge2 = {
  import java.nio.file.Files
  val mainImage = ImageIO.read(this.getClass.getResourceAsStream("/weapons/s2_52_gal.png"))
  val subImage = ImageIO.read(this.getClass.getResourceAsStream("/sub/s2_splat_bomb.png"))
  val specialImage = ImageIO.read(this.getClass.getResourceAsStream("/specials/s2_booyah_bomb.png"))
  val rendered = Splooge2KitGen.renderKit("52 gal", mainImage, "Splat bomb", Right(subImage), "Booyah bomb", Right(specialImage), Some("500"), Color.BLACK, None)
  ImageIO.write(rendered, "png", File("out/s2test.png")) 
}
def testSplooge1 = {
  import java.nio.file.Files
  val mainImage = ImageIO.read(this.getClass.getResourceAsStream("/weapons/s_52_gal.png"))
  val subImage = ImageIO.read(this.getClass.getResourceAsStream("/sub/s_splat_bomb.png"))
  val specialImage = ImageIO.read(this.getClass.getResourceAsStream("/specials/s_echolocator.png"))
  val rendered = Splooge1KitGen.renderKit("Glooga Dualies TEST TEST TEST", mainImage, "Splat bomb", Right(subImage), "Booyah bomb", Right(specialImage), Some("500"), Color.BLACK, None)
  ImageIO.write(rendered, "png", File("out/s1test.png")) 
}

