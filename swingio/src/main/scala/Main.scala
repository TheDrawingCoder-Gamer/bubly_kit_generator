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
import fs2.concurrent.*
import javax.imageio.ImageIO
import javax.swing.UIManager
def imageSelector(image: SignallingRef[IO, Option[BufferedImage]]): Resource[IO, Component[IO]] = {
  flow(
    button(
      text := "Select Image...",
      onBtnClick --> {
        _.evalMap(_ => FileChooser[IO].open).evalMap(maybeFile => maybeFile.traverse(file => IO.blocking { ImageIO.read(file) } )).foreach(image.set)
      }
      ),
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
      }
    )}
  )
}

def OtherWeaponsDropdown(daLabel: String, weapon: SignallingRef[IO, OtherWeapon], game: SignallingRef[IO, GameStyle],
  selectedFile: SignallingRef[IO, Option[BufferedImage]], name: SignallingRef[IO, Option[String]], weapons: Seq[OtherWeapon]): Resource[IO, Component[IO]] = {
  val curGames = weapon.map(_.games)
  curGames.discrete.debug().compile.drain.background.void *>
  box(
    flow(
      label(text := daLabel + " Name: ", 
        icon <-- (weapon.asInstanceOf[Signal[IO, OtherWeapon]], game, selectedFile).tupled.discrete.evalMap( (w, g, f) =>
          (f match {
            case Some(value) => JVMImage(value).pure[IO]
            case None => Image.loadFromResource(otherPath(w, g)).map(_.asInstanceOf[JVMImage])
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
          _.evalMap(_ => self.item.get).foreach(it => weapon.set(it) *> game.set(it.games.head) *> selectedFile.set(None))
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
  weapons: Seq[Weapon]) = {
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
            .foreach(name.set(_) *> selectedFile.set(None))
        }
      )},
      comboBox[Weapon].withSelf { self => 
        ( items := weapons,
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

def BuildMainWeaponDropdown(weapons: Seq[Weapon]): IO[(SignallingRef[IO, Weapon], SignallingRef[IO, WeaponStyle], SignallingRef[IO, Boolean], 
  SignallingRef[IO, Option[BufferedImage]], SignallingRef[IO, Option[String]], Resource[IO, Component[IO]])] = {
  for {
    weapon <- SignallingRef[IO].of(weapons.head)
    style <- SignallingRef[IO].of(weapons.head.styles.head)
    do2D <- SignallingRef[IO].of(false)
    selectedFile <- SignallingRef[IO].of[Option[BufferedImage]](None)
    name <- SignallingRef[IO].of[Option[String]](None)
    dropdown <- IO(MainWeaponDropdown(weapon, style, do2D, selectedFile, name, weapons))
  } yield (weapon, style, do2D, selectedFile, name, dropdown)
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
  spName: Option[String], mainImage: Option[BufferedImage], subImage: Option[BufferedImage], spImage: Option[BufferedImage]): IO[JVMImage] = {
  val renderer = Splooge3KitGen.renderKit
  val daMainName = mainName.getOrElse(mainWeapon.name)
  val daSubName = subName.getOrElse(sub.name)
  val daSpName = spName.getOrElse(special.name)
  
  for {
    mainImg <- mainImage.map(it => JVMImage(it).pure[IO]).getOrElse(weaponPath(mainWeapon, mainStyle, twodim) >>= Image.loadFromResource)
    subImg <- subImage.map(it => JVMImage(it).pure[IO]).getOrElse(Image.loadFromResource(otherPath(sub, subGame)))
    spImg <- spImage.map(it => JVMImage(it).pure[IO]).getOrElse(Image.loadFromResource(otherPath(special, spGame)))
    res <- renderer(daMainName, mainImg, daSubName, subImg, daSpName, spImg, None, None)
  } yield res.asInstanceOf[JVMImage]
}
import javax.swing.JFileChooser
import java.io.File

object App extends IOSwingApp {
  def render = for {
    (mainWeapon, mainStyle, do2D, mainImage, mainName, mainDropdown) <- BuildMainWeaponDropdown(Mains.weapons).toResource
    (sub, subGame, subImage, subName, subDropdown) <- BuildOtherWeaponsDropdown("Sub", Subs.weapons).toResource
    (special, spGame, spImage, spName, spDropdown) <- BuildOtherWeaponsDropdown("Special", Specials.weapons).toResource
    win <- window(
        title := "Splatoon 3 Kit Generator",
        lookAndFeel := UIManager.getInstalledLookAndFeels().find(_.getName() == "GTK+").map(_.getClassName()).getOrElse(UIManager.getSystemLookAndFeelClassName()),
        box(
          mainDropdown,
          subDropdown,
          spDropdown,
          button(
            text := "Generate!",
            onBtnClick --> {
              _.evalMap { _ => 
                (mainWeapon.get, mainStyle.get, sub.get, subGame.get, special.get,
                  spGame.get, do2D.get, mainName.get, subName.get, spName.get, mainImage.get, subImage.get,
                  spImage.get).tupled
              }.evalMap { (main, mainStyle, sub, subGame, special, spGame, do2D, mainName, subName, spName, mainImage, subImage, spImage) =>
                for {
                  _ <- IO.println(main.name)
                  img <- renderKit(main, mainStyle, sub, subGame, special, spGame, do2D, mainName, subName, spName, mainImage, subImage, spImage)
                } yield img
              }.foreach(img => FileChooser[IO].save.flatMap(file => file.traverse(it => IO.blocking { ImageIO.write(img.inner, "png", it) }).void ) )
            }
            )
        ),

      )

  } yield win

}