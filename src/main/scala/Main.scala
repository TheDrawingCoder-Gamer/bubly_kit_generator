

import javax.imageio.ImageIO;
import java.io.File;
case class ItemLocation(val path : String, val internal : Boolean) {
  def asBufferedImage = 
    if internal then 
      ImageIO.read(this.getClass.getResourceAsStream(path))
    else 
      ImageIO.read(File(path))
}
case class Weapon(primary : (String, String), val styles : Map[String, String]): 
  def keys =
    daMap.keys
  lazy val daMap = styles + primary
  def primaryKey = primary._1 
  def primaryLoc = primary._2 
def AWeapon(primary : (String, String), contents : (String, String)*) = Weapon(primary, Map(contents:_*))
def SimpleWeapon(loc : String) = 
  Weapon(("Splatoon 3", loc), Map())
def NamedWeapon(name : String, loc : String) = 
  Weapon((name, loc), Map())
def Resource(path : String) = ItemLocation(path, true)

object Mains: 
  val weapons = Map(
    ("52 Gal", SimpleWeapon("/weapons/s3_52gal.png")),
    ("96 Gal", SimpleWeapon("/weapons/s3_96gal.png")),
    ("Aerospray", SimpleWeapon("/weapons/s3_aerospray.png")),
    ("Ballpoint Splatling", SimpleWeapon("/weapons/s3_ballpoint_splatling.png")),
    ("Bamboozler 14", AWeapon(
      ("Splatoon 3", "/weapons/s3_bamboozler_14.png"),
      ("Grizzco", "/weapons/s3_grizzco_bamboozler_14.png"),
      )),
    ("Blaster", AWeapon(
      ("Splatoon 3", "/weapons/s3_blaster.png"),
      ("Grizzco", "/weapons/s3_grizzco_blaster.png")
    )),
    ("Bloblobber", SimpleWeapon("/weapons/s3_bloblobber.png")),
    ("Carbon Roller", SimpleWeapon("/weapons/s3_carbon_roller.png")),
    ("Clash Blaster", SimpleWeapon("/weapons/s3_clash_blaster.png")),
    ("Dapple Dualies", SimpleWeapon("/weapons/s3_dapple_dualies.png")),
    ("Dualie Squelchers", SimpleWeapon("/weapons/s3_dualie_squelchers.png")),
    ("Dynamo Roller", SimpleWeapon("/weapons/s3_dynamo_roller.png")),
    ("E-Liter 4K", SimpleWeapon("/weapons/s3_e-liter.png")),
    ("E-Liter 4K Scope", SimpleWeapon("/weapons/s3_e-liter_scope.png")),
    ("Explosher", SimpleWeapon("/weapons/s3_explosher.png")),
    ("Flingza Roller", SimpleWeapon("/weapons/s3_flingza_roller.png")),
    ("Glooga Dualies", SimpleWeapon("/weapons/s3_glooga_dualies.png")),
    ("Goo Tuber", SimpleWeapon("/weapons/s3_goo_tuber.png")),
    ("H-3 Nozzlenose", SimpleWeapon("/weapons/s3_h-3_nozzlenose.png")),
    ("Heavy Splatling", SimpleWeapon("/weapons/s3_heavy_splatling.png")),
    ("Hydra Splatling", SimpleWeapon("/weapons/s3_hydra_splatling.png")),
    ("Inkbrush", SimpleWeapon("/weapons/s3_inkbrush.png")),
    ("Jet Squelcher", SimpleWeapon("/weapons/s3_jet_squelcher.png")),
    ("L-3 Nozzlenose", SimpleWeapon("/weapons/s3_l-3_nozzlenose.png")),
    ("Luna Blaster", SimpleWeapon("/weapons/s3_luna_blaster.png")),
    ("Mini Splatling", SimpleWeapon("/weapons/s3_mini_splatling.png")),
    ("N-Zap", AWeapon(
      ("Splatoon 3", "/weapons/s3_n-zap.png"),
      ("N-Zap 89", "/weapons/s3_n-zap_89.png"),
      ("N-Zap 83", "/weapons/s3_n-zap_83.png")
    )
    ),
    ("Nautilus", SimpleWeapon("/weapons/s3_nautilus.png")),
    ("Octobrush", SimpleWeapon("/weapons/s3_octobrush.png")),
    ("Range Blaster", AWeapon(
      ("Splatoon 3", "/weapons/s3_range_blaster.png"),
      ("Grim Range Blaster", "/weapons/s3_grim_range_blaster.png")
    )),
    ("Rapid Blaster", SimpleWeapon("/weapons/s3_rapid_blaster.png")),
    ("Rapid Blaster Pro", SimpleWeapon("/weapons/s3_rapid_blaster_pro.png")),
    ("REEF-LUX 450", SimpleWeapon("/weapons/s3_reef-lux_450.png")),
    ("Slosher", SimpleWeapon("/weapons/s3_slosher.png")),
    ("Sloshing Machine", AWeapon(
      ("Splatoon 3", "/weapons/s3_sloshing_machine.png"),
      ("Grizzco", "/weapons/s3_grizzco_sloshing_machine.png")
    )),
    ("Splash-o-matic", SimpleWeapon("/weapons/s3_splash-o-matic.png")),
    ("Splat Brella", AWeapon(
      ("Splatoon 3", "/weapons/s3_splat_brella.png"),
      ("Grizzco", "/weapons/s3_grizzco_splat_brella.png")
      )),
    ("Splat Charger", SimpleWeapon("/weapons/s3_splat_charger.png")),
    ("Splat Dualies", AWeapon(
      ("Splatoon 3", "/weapons/s3_splat_dualies.png"),
      ("Kensa Dualies", "/weapons/s3_kensa_dualies.png")
      )),
    ("Splat Roller", SimpleWeapon("/weapons/s3_splat_roller.png")),
    ("Splatana Stamper", SimpleWeapon("/weapons/s3_splatana_stamper.png")),
    ("Splatana Wiper", SimpleWeapon("/weapons/s3_splatana_wiper.png")),
    ("Splatterscope", SimpleWeapon("/weapons/s3_splatterscope.png")),
    ("Splattershot", AWeapon(
      ("Splatoon 3", "/weapons/s3_splattershot.png"),
      ("Kensa Splattershot", "/weapons/s3_kensa_splattershot.png"),
      ("Heroshot", "/weapons/s3_heroshot.png")
    )),
    ("Splattershot Jr.", SimpleWeapon("/weapons/s3_splattershot_jr.png")),
    ("Splattershot Pro", SimpleWeapon("/weapons/s3_splattershot_pro.png")),
    ("Sploosh-o-matic", SimpleWeapon("/weapons/s3_sploosh-o-matic.png")),
    ("Squeezer", SimpleWeapon("/weapons/s3_squeezer.png")),
    ("Squiffer", SimpleWeapon("/weapons/s3_squiffer.png")),
    ("Tenta Brella", SimpleWeapon("/weapons/s3_tenta_brella.png")),
    ("Tetra Dualies", SimpleWeapon("/weapons/s3_tetra_dualies.png")),
    ("Tri-Slosher", SimpleWeapon("/weapons/s3_tri-slosher.png")),
    ("Tri-Stringer", AWeapon(
      ("Splatoon 3", "/weapons/s3_tri-stringer.png"),
      ("Grizzco", "/weapons/s3_grizzco_tri-stringer.png")
      )),
    ("Undercover Brella", SimpleWeapon("/weapons/s3_undercover_brella.png"))
  )


object Subs: 
  val weapons = Map( 
    ("Angle Shooter", SimpleWeapon("/sub/s3_angle_shooter.png")),
    ("Autobomb", AWeapon(
      ("Splatoon 3", "/sub/s3_autobomb.png"),
      ("Splatoon 2", "/sub/s2_autobomb.png")
    )),
    ("Burst Bomb", AWeapon(
      ("Splatoon 3", "/sub/s3_burst_bomb.png"),
      ("Splatoon 2", "/sub/s2_burst_bomb.png"),
      ("Splatoon", "/sub/s_burst_bomb.png")
    )),
    ("Curling Bomb", AWeapon(
      ("Splatoon 3", "/sub/s3_curling_bomb.png"),
      ("Splatoon 2", "/sub/s2_curling_bomb.png")
    )),
    ("Fizzy Bomb", AWeapon(
      ("Splatoon 3", "/sub/s3_fizzy_bomb.png"),
      ("Splatoon 2", "/sub/s2_fizzy_bomb.png")
    )),
    ("Ink Mine", AWeapon(
      ("Splatoon 3", "/sub/s3_ink_mine.png"),
      ("Splatoon 2", "/sub/s2_ink_mine.png"),
      ("Splatoon", "/sub/s_ink_mine.png")
    )),
    ("Point Sensor", AWeapon(
      ("Splatoon 3", "/sub/s3_point_sensor.png"),
      ("Splatoon 2", "/sub/s2_point_sensor.png"),
      ("Splatoon", "/sub/s_point_sensor.png")
    )),
    ("Smallfry", SimpleWeapon("/sub/s3_smallfry.png")),
    ("Splash Wall", AWeapon(
      ("Splatoon 3", "/sub/s3_splash_wall.png"),
      ("Splatoon 2", "/sub/s2_splash_wall.png"),
      ("Splatoon", "/sub/s_splash_wall.png")
    )),
    ("Splat Bomb", AWeapon(
      ("Splatoon 3", "/sub/s3_splat_bomb.png"),
      ("Splatoon 2", "/sub/s2_splat_bomb.png"),
      ("Splatoon", "/sub/s_splat_bomb.png")
    )), 
    ("Sprinkler", AWeapon(
      ("Splatoon 3", "/sub/s3_sprinkler.png"),
      ("Splatoon 2", "/sub/s2_sprinkler.png"),
      ("Splatoon", "/sub/s_sprinkler.png")
    )),
    ("Squid Beakon", AWeapon(
      ("Splatoon 3", "/sub/s3_squid_beakon.png"),
      ("Splatoon 2", "/sub/s2_squid_beakon.png"),
      ("Splatoon", "/sub/s_squid_beakon.png")
    )),
    ("Suction Bomb", AWeapon(
      ("Splatoon 3", "/sub/s3_suction_bomb.png"),
      ("Splatoon 2", "/sub/s2_suction_bomb.png"),
      ("Splatoon", "/sub/s_suction_bomb.png")
    )),
    ("Torpedo", AWeapon(
      ("Splatoon 3", "/sub/s3_torpedo.png"),
      ("Splatoon 2", "/sub/s2_torpedo.png")
    )),
    ("Toxic Mist", AWeapon(
      ("Splatoon 3", "/sub/s3_toxic_mist.png"),
      ("Splatoon 2", "/sub/s2_toxic_mist.png")
    )),
    ("Disruptor", SimpleWeapon("/sub/s_disruptor.png")),
    ("Seeker", AWeapon(
      ("Splatoon 3", "/sub/s3_seeker.png"),
      ("Splatoon", "/sub/s_seeker.png")
    ))
    )
object Specials:
  val weapons = Map(
      ("Big Bubbler", SimpleWeapon("/specials/s3_big_bubbler.png")),
      ("Booyah Bomb", AWeapon(
        ("Splatoon 3", "/specials/s3_booyah_bomb.png"),
        ("Splatoon 2", "/specials/s2_booyah_bomb.png")
      )),
      ("Crab Tank", SimpleWeapon("/specials/s3_crab_tank.png")),
      ("Ink Storm", AWeapon(
        ("Splatoon 3", "/specials/s3_ink_storm.png"),
        ("Splatoon 2", "/specials/s2_ink_storm.png")
      )),
      ("Ink Vac", SimpleWeapon("/specials/s3_ink_vac.png")),
      ("Inkjet", AWeapon(
        ("Splatoon 3", "/specials/s3_inkjet.png"),
        ("Splatoon 2", "/specials/s2_inkjet.png")
      )),
      ("Killer Wail 5.1", SimpleWeapon("/specials/s3_killer_wail_5.png")),
      ("Reefslider", SimpleWeapon("/specials/s3_reefslider.png")),
      ("Splashdown", AWeapon(
        ("Splatoon 3", "/specials/s3_splashdown.png"),
        ("Splatoon 2", "/specials/s2_splashdown.png")
      )),
      ("Tacticooler",  SimpleWeapon("/specials/s3_tacticooler.png")),
      ("Tenta Missiles", AWeapon(
        ("Splatoon 3", "/specials/s3_tenta_missiles.png"),
        ("Splatoon 2", "/specials/s2_tenta_missiles.png")
      )),
      ("Triple Inkstrike", SimpleWeapon("/specials/s3_triple_inkstrike.png")),
      ("Trizooka", SimpleWeapon("/specials/s3_trizooka.png")),
      ("Ultra Stamp", AWeapon(
        ("Splatoon 3", "/specials/s3_ultra_stamp.png"),
        ("Splatoon 2", "/specials/s2_ultra_stamp.png")
      )),
      ("Wave Breaker", SimpleWeapon("/specials/s3_wave_breaker.png")),
      ("Zipcaster", SimpleWeapon("/specials/s3_zipcaster.png")),
      ("Autobomb Rush", SimpleWeapon("/specials/s3_autobomb_rush.png")),
      ("Autobomb Launcher", SimpleWeapon("/specials/s2_autobomb_launcher.png")),
      ("Baller", AWeapon(
        ("Splatoon 3", "/specials/s3_baller.png"),
        ("Splatoon 2", "/specials/s2_baller.png")
      )),
      ("Bubbler", AWeapon(
        ("Splatoon 3", "/specials/s3_bubbler.png"),
        ("Splatoon", "/specials/s_bubbler.png")
      )),
      ("Burst Bomb Rush", AWeapon(
        ("Splatoon 3", "/specials/s3_burst_bomb_rush.png"),
        ("Splatoon", "/specials/s_burst_bomb_rush.png")
      )),
      ("Burst Bomb Launcher", AWeapon(
        ("Splatoon 3", "/specials/s3_burst_bomb_rush.png"),
        ("Splatoon 2", "/specials/s2_burst_bomb_launcher.png")
      )),
      ("Curling Bomb Rush", SimpleWeapon("/specials/s3_curling_bomb_rush.png")),
      ("Curling Bomb Launcher", AWeapon(
        ("Splatoon 3", "/specials/s3_curling_bomb_rush.png"),
        ("Splatoon 2", "/specials/s2_curling_bomb_launcher.png")
      )),
      ("Echolocator", AWeapon(
        ("Splatoon 3", "/specials/s3_echolocator.png"),
        ("Splatoon", "/specials/s_echolocator.png")
      )),
      ("Ink Armor", AWeapon(
        ("Splatoon 3", "/specials/s3_ink_armor.png"),
        ("Splatoon 2", "/specials/s2_ink_armor.png")
      )),
      ("Inkstrike", AWeapon(
        ("Splatoon 3", "/specials/s3_inkstrike.png"),
        ("Splatoon", "/specials/s_inkstrike.png")
      )),
      ("Killer Wail", AWeapon(
        ("Splatoon 3", "/specials/s3_killer_wail.png"),
        ("Splatoon", "/specials/s_killer_wail.png")
      )),
      ("Kraken", AWeapon(
        ("Splatoon 3", "/specials/s3_kraken.png"),
        ("Splatoon", "/specials/s_kraken.png")
      )),
      ("Seeker Bomb Rush", AWeapon(
        ("Splatoon 3", "/specials/s3_seeker_bomb_rush.png"), 
        ("Splatoon", "/specials/s_seeker_bomb_rush.png")
      )),
      ("Splat Bomb Rush", AWeapon(
        ("Splatoon 3", "/specials/s3_splat_bomb_rush.png"),
        ("Splatoon", "/specials/s_splat_bomb_rush.png")
      )),
      ("Splat Bomb Launcher", AWeapon(
        ("Splatoon 3", "/specials/s3_splat_bomb_rush.png"),
        ("Splatoon 2", "/specials/s3_splat_bomb_launcher.png")
      )),
      ("Suction Bomb Rush", AWeapon(
        ("Splatoon 3", "/specials/s3_suction_bomb_rush.png"),
        ("Splatoon", "/specials/s_suction_bomb_rush.png"),
      )),
      ("Suction Bomb Launcher", AWeapon(
        ("Splatoon 3", "/specials/s3_suction_bomb_rush.png"),
        ("Splatoon 2", "/specials/s2_suction_bomb_launcher.png")
      )),
      // placeholders until i make custom 3 art
      ("Inkzooka", NamedWeapon("Splatoon", "/specials/s_inkzooka.png")),
      ("Bubble Blower", NamedWeapon("Splatoon 2", "/specials/s2_bubble_blower.png")),
      ("Sting Ray", NamedWeapon("Splatoon 2", "/specials/s2_sting_ray.png"))

    )
import java.awt.image.BufferedImage;
import java.awt.image.FilteredImageSource;
import java.awt.Graphics2D
import java.awt.Font
import java.awt.Color
import java.awt.RenderingHints
import java.awt.AlphaComposite 
import java.awt.geom.AffineTransform
import javax.swing.GrayFilter 

def renderSplooge3(mainName : String, mainImage : BufferedImage, subName : String, subImage : BufferedImage, specialName : String, specialImage : BufferedImage, specialPoints : Option[String])  =
  val canvas = BufferedImage(686,507, BufferedImage.TYPE_INT_ARGB); 
  val g = canvas.createGraphics()
  val kit = ImageIO.read(this.getClass.getResourceAsStream("/ui/s3_kit_backdrop.png"))
  val w = kit.getWidth()
  val h = kit.getHeight()
  val subSpecialX = 268
  val subY = 151 
  val specialY = 240
  val transform = AffineTransform(1.1233,-0.0746, -51.0117, -0.0027, 0.8273, 126.8330) 
  g.setRenderingHint(
    RenderingHints.KEY_INTERPOLATION,
    RenderingHints.VALUE_INTERPOLATION_BILINEAR)
  g.drawImage(kit, 0, 0, 676, 413, null)
  g.drawImage(mainImage, 47, 120, 196, 196, null)
  g.drawImage(subImage, subSpecialX, subY, 64, 64, null) 
  g.drawImage(specialImage, subSpecialX, specialY, 64, 64, null)
  g.setRenderingHint(
        RenderingHints.KEY_TEXT_ANTIALIASING,
        RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
  val sploogeFont = Font.createFont(Font.TRUETYPE_FONT, this.getClass.getResourceAsStream("/font/splatoon1.otf"))
  val splooge2Font = Font.createFont(Font.TRUETYPE_FONT, this.getClass.getResourceAsStream("/font/splatoon2.otf"))
  val mainFont = sploogeFont.deriveFont(Font.PLAIN, 32)
  val subFont = splooge2Font.deriveFont(Font.PLAIN, 25)
  val pointsFont = splooge2Font.deriveFont(Font.PLAIN, 32)
  g.setFont(mainFont)
  val fm = g.getFontMetrics()
  g.setColor(Color.WHITE)
  g.drawString(mainName, 52, 25 + fm.getAscent())
  g.setFont(subFont)
  val fm2 = g.getFontMetrics()
  val subFontX = subSpecialX + 75
  g.drawString(subName, subFontX, subY + fm2.getAscent())
  g.drawString(specialName, subFontX, specialY + fm2.getAscent())
  specialPoints match 
    case Some(p) =>
      g.setFont(pointsFont)
      val pointsAscent = g.getFontMetrics().getAscent()
      g.drawString(p + "p", 475, 320 + pointsAscent)
    case None => ()
  g.dispose()
  canvas
class ComboModel[A] extends javax.swing.DefaultComboBoxModel[A] {
  def +=(elem: A) =  addElement(elem)
  def ++=(elems: TraversableOnce[A]) =  elems.foreach(addElement) 
}
import scala.swing.* 
import java.awt.Dimension
object SwingApp: 
  def top = new MainFrame {
    private val me = this
    title = "Bubly Kit Generator"
    contents = {
      new BoxPanel(Orientation.Vertical) {
        
        val mainGroup = WeaponGroup(BufferedImage(1, 1, BufferedImage.TYPE_INT_ARGB), "Main", Mains.weapons) 
        val subGroup = WeaponGroup(BufferedImage(1, 1, BufferedImage.TYPE_INT_ARGB),"Sub", Subs.weapons) 
        val specialGroup = WeaponGroup(BufferedImage(1, 1, BufferedImage.TYPE_INT_ARGB),"Special", Specials.weapons)
        def revertMainImage() =
          mainGroup.image = Resource(Mains.weapons.get(mainGroup.dropdown.selection.item).get.daMap.get(mainGroup.styleDropdown.selection.item).get).asBufferedImage
        def revertSubImage() = 
          subGroup.image = Resource(Subs.weapons.get(subGroup.dropdown.selection.item).get.daMap.get(subGroup.styleDropdown.selection.item).get).asBufferedImage
        def revertSpecialImage() = 
          specialGroup.image = Resource(Specials.weapons.get(specialGroup.dropdown.selection.item).get.daMap.get(specialGroup.styleDropdown.selection.item).get).asBufferedImage
        val selectMainImage = new Button("Select Main Image") {
          reactions += {
            case event.ButtonClicked(_) => 
              val chooser = FileChooser()
              val res = chooser.showOpenDialog(me)
              res match 
                case FileChooser.Result.Approve => 
                  val image = ImageIO.read(chooser.selectedFile)
                  mainGroup.image = image


          }
        }
        val selectSubImage = new Button("Select Sub Image") {
          reactions += {
            case event.ButtonClicked(_) => 
              val chooser = FileChooser()
              val res = chooser.showOpenDialog(me)
              res match 
                case FileChooser.Result.Approve => 
                  val image = ImageIO.read(chooser.selectedFile)
                  subGroup.image = image 


          }
        }
        val selectSpecialImage = new Button("Select Special Image") {
          reactions += {
            case event.ButtonClicked(_) => 
              val chooser = FileChooser()
              val res = chooser.showOpenDialog(me)
              res match 
                case FileChooser.Result.Approve => 
                  val image = ImageIO.read(chooser.selectedFile)
                  specialGroup.image = image


          }
        }
        val clearMainImage = new Button("Clear Main Image") {
          reactions += {
            case event.ButtonClicked(_) => 
              revertMainImage()
          }
        }
        val clearSubImage = new Button("Clear Sub Image") {
          reactions += {
            case event.ButtonClicked(_) => 
              revertSubImage()
          }
        }
        val clearSpecialImage = new Button("Clear Special Image") {
          reactions += {
            case event.ButtonClicked(_) => 
              revertSpecialImage()
          }
        }
        contents += mainGroup
        contents += FlowPanel(selectMainImage, clearMainImage)
        contents += subGroup 
        contents += FlowPanel(selectSubImage, clearSubImage)
        contents += specialGroup
        contents += FlowPanel(selectSpecialImage, clearSpecialImage)
        contents += new Button("Generate!") {
          reactions += {
            case event.ButtonClicked(_) => 
              val mainImage = mainGroup.image
              val mainName = mainGroup.textField.text match 
                case "" => mainGroup.dropdown.selection.item
                case text => text
              val subImage = subGroup.image 
              val subName = subGroup.textField.text match 
                case "" => subGroup.dropdown.selection.item 
                case text => text
              val specialImage = specialGroup.image 
              val specialName = specialGroup.textField.text match 
                case "" => specialGroup.dropdown.selection.item 
                case text => text 
              val kit = renderSplooge3(mainName, mainImage, subName, subImage, specialName, specialImage, None)
              val chooser = FileChooser()
              val res = chooser.showSaveDialog(me)
              res match 
                case FileChooser.Result.Approve => 
                  ImageIO.write(kit, "png", chooser.selectedFile)
            
          }
        } 

      }
    }
  }

   
    
class LabeledTextField(label : String) extends FlowPanel {
  contents += Label(label)
  val textField = new TextField(20)
  contents += textField
}
import javax.swing.ImageIcon
class WeaponGroup(private var daimage: BufferedImage, label : String, weaponsMap : Map[String, Weapon]) extends BoxPanel(Orientation.Vertical) {
  val labeledField = LabeledTextField(label)
  contents += labeledField 
  val weapons = Seq.from(weaponsMap.keys)
  val styleModel = ComboModel[String]()
  val styleDropdown = new ComboBox[String](Seq()) {
    selection.reactions += {
      case event.SelectionChanged(_) =>
        if this.selection.item == null then 
          ()
        else
          val weapon = weaponsMap.get(dropdown.selection.item).get
          val goodThing = weapon.daMap.get(this.selection.item).get 
          image = Resource(goodThing).asBufferedImage
    }
  }
  styleDropdown.peer.setModel(styleModel)
  val dropdown : ComboBox[String] = new ComboBox[String](weapons) {
    maximumSize = Dimension(200, 6)
    selection.reactions += {
      case event.SelectionChanged(_) => 
        val weapon = weaponsMap.get(this.selection.item).get
        image = Resource(weapon.primary._2).asBufferedImage 
        styleModel.removeAllElements()
        styleModel ++=  weapon.keys
        styleDropdown.selection.item = weapon.primaryKey
    }
  }
  val imagelabel = Label("", ImageIcon(daimage), Alignment.Center)

  contents += FlowPanel(imagelabel, dropdown, styleDropdown)
  def textField = labeledField.textField
  def image = daimage
  def image_=(i : BufferedImage) =
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
import javax.swing.UIManager
@main def launchApp = 
  UIManager.getInstalledLookAndFeels().find(it => it.getName() == "GTK+").foreach(it => UIManager.setLookAndFeel(it.getClassName()))
  val window = SwingApp.top
  window.centerOnScreen()
  window.open()
 
