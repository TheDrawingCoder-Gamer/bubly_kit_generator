

import collection.immutable.ListMap
import javax.imageio.ImageIO;
import java.io.File;
import scala.collection.mutable as mut
import java.awt.Toolkit
import net.bulbyvr.splooge.core.*
import net.bulbyvr.splooge.core.util.JVMImage
case class ItemLocation(val path : String, val internal : Boolean) {
  def asBufferedImage = 
    if (internal) 
      ImageIO.read(this.getClass.getResourceAsStream("/" + path))
    else 
      ImageIO.read(File(path))
}
def Resource(path: String) = ItemLocation(path, true)
import java.awt.image.BufferedImage;
import java.awt.image.FilteredImageSource;
import java.awt.Graphics2D
import java.awt.Font
import java.awt.Color
import java.awt.RenderingHints
import java.awt.AlphaComposite 
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
  import scala.util.{Try, Success, Failure}
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
def weaponPath(weapon: Weapon, style : WeaponStyle, twodim : Boolean) : ItemLocation = {
  val name = weapon.name
  val rootPath = "weapons"
  val daName = name.replace(' ', '_').toLowerCase()
  val gStyle =
    style.name match {
      case StyleName.Named(s) => StyleName.Named(s.trim().replace(' ', '_').toLowerCase())
      case StyleName.Empty => StyleName.Empty
    }
  val gamePrefix = style.game.name
  val (noDimPath, goodPath) = {
    val firstPath = 
      gStyle match {
        case StyleName.Empty => gamePrefix + "_" + daName
        case StyleName.Named(s) => gamePrefix + "_" + s + "_" + daName
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
class GameCellRenderer extends GenericCellRenderer[GameStyle] {
  override def getTextOfValue(value : GameStyle) = 
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
        def generator(write: BufferedImage => Unit) = {
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
              def setupSvg(svg: SVGDocument, color: Color) = {
                      lazy val magicColor = {
                        val r = color.getRed()
                        val g = color.getGreen()
                        val b = color.getBlue()
                        s"rgb($r, $g, $b)" 
                      }
                      lazy val basedCss = 
                        s""".ink {
                          |
                          |  fill: $magicColor !important;
                          |}
                          |.inkstroke {
                          |  stroke: $magicColor !important;
                          |}
                          |#ink stop {
                          |  stop-color: $magicColor;
                          |}
                        """.stripMargin
                    rasterize(svg, Some(64), Some(64), basedCss)
              }
              val sub = subImage match {
                case Left(value) => setupSvg(value, colorPicker.color).get
                case Right(value) => value 
              }
              val special = specialImage match {
                case Left(value) => setupSvg(value, colorPicker.color).get
                case Right(value) => value
              }
               
              val kit = renderer(mainName, JVMImage(mainImage), subName, JVMImage(sub), specialName, JVMImage(special), None, brand.map(it => JVMImage(it)))
              write(kit.asInstanceOf[JVMImage].inner)
        }
        val generateButton = new Button("Generate!") {
          reactions += {
            case event.ButtonClicked(_) => 

              generator { kit => 
                gtkFileSelector(true).foreach { it => 
                  ImageIO.write(kit, "png", it)
                }
              }

            
          }
        }
        val genClipboardButton = new Button("Generate to clipboard") {
          reactions += {
            case event.ButtonClicked(_) =>
              generator { kit => 
                Toolkit.getDefaultToolkit().getSystemClipboard().setContents(ImageTransferable(kit), null)
              }
          }
        }
        contents += FlowPanel(generateButton, genClipboardButton)

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
    Some(weaponPath(weapon, weapon.styles.toSeq(index), flatcheckbox.selected))
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
        case Some(v) => 
          v.name match {
            case StyleName.Empty => v.game.name
            case StyleName.Named(name) => s"${name} ${v.game.name}"
          }
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
class OtherWeaponGroup(d : BufferedImage, l : String, w : Seq[OtherWeapon]) extends BoxPanel(Orientation.Vertical) with WeaponGroup[OtherWeapon, GameStyle](d, l, w) {
  def currentWeapon = weaponDropdown.selection.item
  def pathHelper(weapon : OtherWeapon, game : GameStyle) = {
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
  val rendered = Splooge2KitGen.renderKit("52 gal", JVMImage(mainImage), "Splat bomb", JVMImage(subImage), "Booyah bomb", JVMImage(specialImage), Some("500"), None)
  ImageIO.write(rendered.asInstanceOf[JVMImage].inner, "png", File("out/s2test.png")) 
}


