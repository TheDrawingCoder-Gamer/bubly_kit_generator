import net.bulbyvr.splooge.core.*
import util.{Image, JVMImage, AffineTransform}
import cats.syntax.all.*
import cats.effect.syntax.all.*
import net.bulbyvr.swing.io.all.{*, given}
import net.bulbyvr.swing.io.wrapper.*
import net.bulbyvr.swing.io.wrapper.Image as WImage
import net.bulbyvr.swing.io.wrapper.event.*
import net.bulbyvr.swing.io.{IOSwingApp, AwtEventDispatchEC}
import cats.effect.*
import java.awt.image.BufferedImage
import java.awt.Image as JImage
import fs2.concurrent.*
import javax.imageio.ImageIO
import javax.swing.UIManager
import fs2.Pipe
import java.awt.Toolkit
def imageSelector(image: SignallingRef[IO, Option[BufferedImage]]): Resource[IO, Component[IO]] = {
  flow(
    button.withSelf { self =>(
      text := "Select Image...",
      onBtnClick --> {
        _.evalMap(_ => FileChooser[IO].open(self)).evalMap(maybeFile => maybeFile.traverse(file => IO.blocking { ImageIO.read(file) } )).foreach(image.set)
      }
    )},
    button(
      text := "Clear Image",
      onBtnClick --> {
        _.foreach(_ => image.set(None))
      }
      )
    )
}
def GameDropdown(game: SignallingRef[IO, GameStyle], games: Signal[IO, Seq[GameStyle]], image: SignallingRef[IO, Option[BufferedImage]]): Resource[IO, Component[IO]] = {
  flow(
    comboBox[GameStyle].withSelf { self => (
      items <-- games,
      onSelectionChange --> {
        _.evalMap(_ => self.item.get).foreach(game.set(_) *> image.set(None))
      },
      renderer := { (it: GameStyle) => it.longName }
    )}
  )
}

def OtherWeaponsDropdown(daLabel: String, weapon: SignallingRef[IO, OtherWeapon], game: SignallingRef[IO, GameStyle],
  selectedFile: SignallingRef[IO, Option[BufferedImage]], name: SignallingRef[IO, Option[String]], weapons: Seq[OtherWeapon]): Resource[IO, Component[IO]] = {
  val curGames = weapon.map(_.games)
  box(
    flow(
      label(text := daLabel + " Name: ", 
        icon <-- (weapon.asInstanceOf[Signal[IO, OtherWeapon]], selectedFile).tupled.discrete.evalMap((w, f) => game.get.map((w, _, f))).evalMap( (w, g, f) =>
          (f match {
            case Some(value) => JVMImage(value).pure[IO]
            case None => 
              val path = otherPath(w, g)
              for {
                exists <- pathExists(path)
                img <- 
                  (if (exists)
                    Image.loadFromResource(path)
                  else 
                    Image.loadFromResource("nothing.png")).map(_.asInstanceOf[JVMImage])
              } yield img
          }).flatMap(scaleImage).map(it => WImage[IO](it.inner))
        ).hold1Resource),
      textField.withSelf { self =>
        (
          columns := 20,
          onValueChange --> {
            _.evalMap(_ => self.text.get).map(it => if (it == "") None else Some(it)).foreach(name.set)
          } 
          )
      }
      ),
    flow(
      comboBox[OtherWeapon].withSelf { self => (
        items := weapons,
        onSelectionChange --> {
          _.evalMap(_ => self.item.get).foreach(it => game.set(it.games.head) *> weapon.set(it) *> selectedFile.set(None))
        },
        renderer := { (it: OtherWeapon) => 
          it.name
        } 
      )},
      GameDropdown(game, curGames, selectedFile)
      ),
    imageSelector(selectedFile),
    )
}

def BuildOtherWeaponsDropdown(label: String, weapons: Seq[OtherWeapon]): IO[(SignallingRef[IO, OtherWeapon], SignallingRef[IO, GameStyle], SignallingRef[IO, Option[BufferedImage]],
    SignallingRef[IO, Option[String]], Resource[IO, Component[IO]]
  )] = {
  for {
    weapon <- SignallingRef[IO].of(weapons.head)
    game <- SignallingRef[IO].of(weapons.head.games.head)
    image <- SignallingRef[IO].of[Option[BufferedImage]](None)
    name <- SignallingRef[IO].of[Option[String]](None)
    dropdown <- IO(OtherWeaponsDropdown(label, weapon, game, image, name, weapons))
  } yield (weapon, game, image, name, dropdown)
}

def scaleImage(image: JVMImage): IO[JVMImage] = {
  val canvas = Image(64, 64).getCanvas()
  val transform = AffineTransform.scaling(64.0 / image.width, 64.0 / image.width)
  for {
    _ <- canvas.drawImage(image, transform)
    r <- canvas.complete()
  } yield r.asInstanceOf[JVMImage]
}
def MainWeaponDropdown(weapon: SignallingRef[IO, Weapon], style: SignallingRef[IO, WeaponStyle], do2D: SignallingRef[IO, Boolean],
  selectedFile: SignallingRef[IO, Option[BufferedImage]],
  name: SignallingRef[IO, Option[String]],
  group: SignallingRef[IO, String],
  weapons: Map[String, Seq[Weapon]],
  groups: Seq[String]) = {
  val daWeapons = group.map(weapons(_))
  val styles = weapon.map(_.styles)
  box(
    flow(
      label(text := "Main name: ",
        icon <-- (weapon.asInstanceOf[Signal[IO, Weapon]], style, do2D, selectedFile).tupled.discrete.evalMap( (w, s, d2, f) =>
            (f match {
              case Some(value) => JVMImage(value).pure[IO]
              case None => weaponPath(w, s, d2).flatMap(Image.loadFromResource(_).map(_.asInstanceOf[JVMImage]))
            }).flatMap(scaleImage).map(it => WImage[IO](it.inner))
            
          ).hold1Resource
        ),
      textField.withSelf { self => (
        columns := 20,
        onValueChange --> {
          _.evalMap(_ => self.text.get)
            .map(it => if (it == "") None else Some(it))
            .foreach(name.set(_))
        }
      )}),
    flow(
      comboBox[String].withSelf { self => 
        ( items := groups,
          onSelectionChange --> {
            _.evalMap(_ => self.item.get)
              .foreach(it => group.set(it) *> weapon.set(weapons(it).head) *> style.set(weapons(it).head.styles.head) *> selectedFile.set(None))
          })
      },
      comboBox[Weapon].withSelf { self => 
        ( items <-- daWeapons,
          onSelectionChange --> {
            _.evalMap(_ => self.item.get)
              .foreach(it => weapon.set(it) *> style.set(it.styles.head) *> selectedFile.set(None))
          },
          renderer := { (it: Weapon) => 
            it.name.pure[IO]
          }
          )
      },
      comboBox[WeaponStyle].withSelf { self => 
        ( items <-- styles,
          onSelectionChange --> {
            _.evalMap(_ => self.item.get)
              .foreach(style.set(_) *> selectedFile.set(None))
          },
          renderer := { (it: WeaponStyle) => 
            it.pretty.pure[IO]
          }
          )
      },
      flow(
      checkbox.withSelf { self => (
        onBtnClick --> {
          _.evalMap(_ => self.selected.get)
            .foreach(do2D.set(_) *> selectedFile.set(None))
        },
        text := "Flat Icons"
      )}
      )
      ),
    imageSelector(selectedFile)
    )
}

def BuildMainWeaponDropdown: IO[(SignallingRef[IO, Weapon], SignallingRef[IO, WeaponStyle], SignallingRef[IO, Boolean], 
  SignallingRef[IO, Option[BufferedImage]], SignallingRef[IO, Option[String]], SignallingRef[IO, String], Resource[IO, Component[IO]])] = {
  val weapons = Mains.groupedWeapons
  val groups = Mains.groups 
  val headWeapon = weapons(groups.head).head
  for {
    weapon <- SignallingRef[IO].of(headWeapon)
    style <- SignallingRef[IO].of(headWeapon.styles.head)
    do2D <- SignallingRef[IO].of(false)
    selectedFile <- SignallingRef[IO].of[Option[BufferedImage]](None)
    name <- SignallingRef[IO].of[Option[String]](None)
    group <- SignallingRef[IO].of(groups.head)
    dropdown <- IO(MainWeaponDropdown(weapon, style, do2D, selectedFile, name, group, weapons, groups))
  } yield (weapon, style, do2D, selectedFile, name, group, dropdown)
}
def OnlyGameDropdown(game: SignallingRef[IO, Game]): Resource[IO, Component[IO]] = {
  flow(
      comboBox[Game].withSelf { self => (
        items := Game.AllGames,
        renderer := { (it: Game) => it.longName },
        onSelectionChange --> {
          _.evalMap(_ => self.item.get).foreach(game.set(_))
        }
      )}
    )
}
def BuildGameDropdown: IO[(SignallingRef[IO, Game], Resource[IO, Component[IO]])] =
  SignallingRef[IO].of(Game.AllGames.head).map { ref => 
    (ref, OnlyGameDropdown(ref))
  }
def pathExists(path: String): IO[Boolean] = {
  IO.delay { this.getClass.getResource(path) }.map(_ != null)
}

def otherPath(weapon: OtherWeapon, game: GameStyle): String = {
  weapon.root + "/" + game.name + "_" + weapon.name.replace(' ', '_').toLowerCase() + ".png"
}
def weaponPath(weapon: Weapon, style : WeaponStyle, twodim : Boolean) : IO[String] = {

  val (noDimPath, goodPath) = weapon.basePath(style, twodim)

  for {
    path <- 
      if (twodim)
        for {
          exists <- pathExists(goodPath)
        } yield {
          if (exists) goodPath else noDimPath 
        }
      else 
        for {
          exists <- pathExists(noDimPath)
        } yield {
          if (exists) noDimPath else goodPath
        }
  } yield path
}
def renderKit(mainWeapon: Weapon, mainStyle: WeaponStyle, sub: OtherWeapon, subGame: GameStyle, 
  special: OtherWeapon, spGame: GameStyle, twodim: Boolean, mainName: Option[String], subName: Option[String],
  spName: Option[String], mainImage: Option[BufferedImage], subImage: Option[BufferedImage], spImage: Option[BufferedImage],
  game: Game, spPoints: Option[String], brand: Option[String]): IO[JVMImage] = {
  val renderer = game match {
    case Game.Splatoon3 => Splooge3KitGen.renderKit
    case Game.Splatoon2 => Splooge2KitGen.renderKit
    case Game.Splatoon1 => Splooge1KitGen.renderKit
      
  }
  val daMainName = mainName.getOrElse(mainWeapon.name)
  val daSubName = subName.getOrElse(sub.name)
  val daSpName = spName.getOrElse(special.name)
  
  for {
    mainImg <- mainImage.map(it => JVMImage(it).pure[IO]).getOrElse(weaponPath(mainWeapon, mainStyle, twodim) >>= Image.loadFromResource)
    subImg <- subImage.map(it => JVMImage(it).pure[IO]).getOrElse(Image.loadFromResource(otherPath(sub, subGame)))
    spImg <- spImage.map(it => JVMImage(it).pure[IO]).getOrElse(Image.loadFromResource(otherPath(special, spGame)))
    brandImg <- brand.traverse(it => Image.loadFromResource(s"brands/${it.toLowerCase().replace(' ', '_')}.png"))
    res <- renderer(daMainName, mainImg, daSubName, subImg, daSpName, spImg, spPoints, brandImg)
  } yield res.asInstanceOf[JVMImage]
}
import javax.swing.JFileChooser
import java.io.File

def BrandDropdown(brand: SignallingRef[IO, Option[String]]): Resource[IO, Component[IO]] = 
  flow(
    label(icon <-- brand.discrete.evalMap { brand =>
      Image.loadFromResource(brandImage(brand)).map(_.asInstanceOf[JVMImage]).flatMap(scaleImage).map(it => WImage[IO](it.inner))
    }.hold1Resource),
    comboBox[Option[String]].withSelf { self => (
        items := Brands.brands.map(Option(_)).prepended(None),
        renderer := { (it: Option[String]) => it.getOrElse("None") },
        onSelectionChange --> {
          _.evalMap(_ => self.item.get).foreach(brand.set)
        }
      )}
    )
def brandImage(name: Option[String]): String = {
  name.map(_.toLowerCase()) match {
    case None => "nothing.png"
    case Some(name) =>
      val n = name.replace(' ', '_')
      s"brands/$n.png"
  }
}

import java.awt.datatransfer.*

class ImageTransferable(private val image: JImage) extends Transferable {
  def getTransferData(x: DataFlavor): Object = {
    if (isDataFlavorSupported(x)) {
      image 
    } else {
      throw new UnsupportedFlavorException(x)
    }
  }
  def isDataFlavorSupported(x: DataFlavor): Boolean = x == DataFlavor.imageFlavor
  def getTransferDataFlavors(): Array[DataFlavor] = Array(DataFlavor.imageFlavor)
}


object App extends IOSwingApp {
  def render = for {
    (mainWeapon, mainStyle, do2D, mainImage, mainName, _, mainDropdown) <- BuildMainWeaponDropdown.toResource
    (sub, subGame, subImage, subName, subDropdown) <- BuildOtherWeaponsDropdown("Sub", Subs.weapons).toResource
    (special, spGame, spImage, spName, spDropdown) <- BuildOtherWeaponsDropdown("Special", Specials.weapons).toResource
    (kitGame, kitDropdown) <- BuildGameDropdown.toResource
    spPoints <- SignallingRef[IO].of[Option[String]](None).toResource
    spPointsTxt = flow("Special points: ", textField.withSelf { self => 
      (
        columns := 20,
        onValueChange --> {
          _.evalMap(_ => self.text.get).map(it => if (it == "") None else Some(it)).foreach(spPoints.set)
        }
        )
      })
    brand <- SignallingRef[IO].of[Option[String]](None).toResource
    brandDropdown = BrandDropdown(brand)
    pipe: Pipe[IO, ButtonClicked[IO], JVMImage] = {
      _.evalMap { _ =>
        (mainWeapon.get, mainStyle.get, sub.get, subGame.get, special.get,
          spGame.get, do2D.get, mainName.get, subName.get, spName.get, mainImage.get,
          subImage.get, spImage.get, kitGame.get, spPoints.get, brand.get).tupled
      }.evalMap { (main, mainStyle, sub, subGame, special, spGame, do2D, mainName,
        subName, spName, mainImage, subImage, spImage, kitGame, spPoints, brand) => 
        renderKit(main, mainStyle, sub, subGame, special, spGame, do2D, mainName,
          subName, spName, mainImage, subImage, spImage, kitGame, spPoints, brand)
      }
    }
    win <- window(
        title := "Splatoon 3 Kit Generator",
        lookAndFeel := UIManager.getInstalledLookAndFeels().find(_.getName() == "GTK+").map(_.getClassName()).getOrElse(UIManager.getSystemLookAndFeelClassName()),
        box(
          mainDropdown,
          subDropdown,
          spDropdown,
          brandDropdown,
          spPointsTxt,
          kitDropdown,
          flow(
            button.withSelf{self => (
              text := "Generate!",
              onBtnClick --> {
                pipe
                  .andThen(_.foreach(img => FileChooser[IO]
                    .save(self)
                    .flatMap(file => file.traverse(it => IO.blocking { ImageIO.write(img.inner, "png", it) }).void ) ))
              }
              )},
            button(
              text := "Generate to clipboard",
              onBtnClick --> pipe.andThen {
                _.foreach(img => IO.blocking { Toolkit.getDefaultToolkit().getSystemClipboard().setContents(ImageTransferable(img.inner), null) }.evalOn(AwtEventDispatchEC)) 
              }
              )
          )
        ),

      )

  } yield win

}
