import net.bulbyvr.splooge.core.*
import fs2.dom.*
import calico.*
import calico.html.io.{*, given}
import cats.effect.*
import fs2.*
import fs2.concurrent.*
import cats.implicits.*
import net.bulbyvr.splooge.core.util.JSImage
import net.bulbyvr.splooge.core.util.Image
import net.bulbyvr.splooge.core.util.{JSImageCanvas, JSImageElement}

import org.scalajs.dom.window as dawindow
import org.scalajs.dom
import org.scalajs.dom.Fetch
import org.scalajs.dom.RequestInit
import org.scalajs.dom.HttpMethod
import net.bulbyvr.splooge.core.Game.Splatoon1
import net.bulbyvr.splooge.core.Game.Splatoon2
import net.bulbyvr.splooge.core.Game.Splatoon3
import org.scalajs.dom.FileReader
import scala.concurrent.Future
import scala.scalajs.js
import cats.Show

def GameDropdown(game: SignallingRef[IO, GameStyle], games: Signal[IO, Seq[GameStyle]], image: SignallingRef[IO, Option[String]]): Resource[IO, HtmlElement[IO]] = {
  IO.pure(game).toResource.flatMap { game =>
    select.withSelf { self => 
        (
          children <-- games.map(_.traverse(g => option(value := g.name, g.name)).map(_.toList)),
          value <-- game.map(_.name),
          onChange --> {
            _.evalMap(_ => self.value.get).evalMap[IO, Option[GameStyle]](it => games.map(_.find(_.name == it)).get).unNone.foreach(game.set)
          }
          )
      }

  }
}
def OtherWeaponDropdowns(label: String, weapon: SignallingRef[IO, OtherWeapon], game: SignallingRef[IO, GameStyle], 
  selectedFile: SignallingRef[IO, Option[String]], name: SignallingRef[IO, Option[String]], weapons: List[OtherWeapon]): Resource[IO, HtmlElement[IO]] = { 
    val curGames = weapon.map(_.games)
    div(
      p(
        label,
      input.withSelf { self =>
        (
          tpe := "text",
          value <-- name.map(_.getOrElse("")),
          onChange --> {
            _.evalMap(_ => self.value.get).map(it => if (it == "") None else Some(it)).foreach(name.set)
          }
          )
      }),
      div(
        img(
          src <-- (weapon.asInstanceOf[Signal[IO, OtherWeapon]], game, selectedFile).tupled.map((w, g, f) => 
              f match {
                case Some(value) => value 
                case None => "resources/" + otherPath(w, g)
              }),
          widthAttr := 64,
          heightAttr := 64,
          ),
        select.withSelf { self => 
          (
            weapons.map(w => option(value := w.name, w.name)),
            value <-- weapon.map(_.name),
            onChange --> {
              _.evalMap(_ => self.value.get).map(it => weapons.find(_.name == it)).unNone.foreach(it => weapon.set(it) *> game.set(it.games.head) *> selectedFile.set(None))
            }
            )
        },
        GameDropdown(game, curGames, selectedFile),
        ImageGroup(selectedFile),
        )
    )
}
def BuildOtherWeaponDropdowns(label: String, weapons: Seq[OtherWeapon]): IO[(SignallingRef[IO, OtherWeapon], SignallingRef[IO, GameStyle], 
  SignallingRef[IO, Option[String]], SignallingRef[IO, Option[String]], Resource[IO, HtmlElement[IO]])] = {
  for {
    weapon <- SignallingRef[IO].of(weapons.head)
    game <- SignallingRef[IO].of(weapons.head.games.head)
    image <- SignallingRef[IO].of[Option[String]](None)
    name <- SignallingRef[IO].of[Option[String]](None)
    dropdown <- IO(OtherWeaponDropdowns(label, weapon, game, image, name, weapons.toList))
  } yield (weapon, game, image, name, dropdown)
}
def inputFiles[F[_]](input: HtmlInputElement[F])(using Async[F]): Ref[F, dom.FileList] = {
  new WrappedRef(() => input.asInstanceOf[dom.HTMLInputElement].files, input.asInstanceOf[dom.HTMLInputElement].files = _)
}
def ImageGroup(image: SignallingRef[IO, Option[String]]): Resource[IO, HtmlElement[IO]] = {
  div(
    input.withSelf { self =>
      (
      tpe := "file",
      accept := "image/png",
      onChange --> {
        _.evalMap(_ => inputFiles(self).get).map(v => if (v.isEmpty) None else Some(v(0)) ).evalMap(_.traverse(loadImage _)).foreach(image.set _)
      }
      )
    },
    button(
      tpe := "button",
      "Clear image",
      onClick --> {
        _.foreach(_ => image.set(None))
      }
      
      )
    ) 
}
def MainWeaponDropdowns(weapon: SignallingRef[IO, Weapon], style: SignallingRef[IO, WeaponStyle], do2D: SignallingRef[IO, Boolean], 
  selectedFile: SignallingRef[IO, Option[String]],
  name: SignallingRef[IO, Option[String]],
  group: SignallingRef[IO, String],
  weapons: Map[String, Seq[Weapon]],
  groups: Seq[String]): Resource[IO, HtmlElement[IO]] = {
  val daWeapons = group.map(weapons(_))
  val styles = weapon.map(_.styles)
  div(
    p(
    "Main name: ",
    input.withSelf { self =>
      (
        tpe := "text",
        value <-- name.map(_.getOrElse("")),
        onChange --> {
          _.evalMap(_ => self.value.get).map(it => if (it == "") None else Some(it)).foreach(name.set)
        }
        )
    }
    ),
    div(
      img(
        src <-- (weapon.asInstanceOf[Signal[IO, Weapon]], style, do2D, selectedFile).tupled.discrete.evalMap((w, s, d, f) =>
            f match {
              case None => weaponPath(w, s, d).map("resources/" + _)
              case Some(value) => IO.pure(value)
            }
            ).holdResource("resources/nothing.png"),
        widthAttr := 64,
        heightAttr := 64
        ),
      select.withSelf { self =>
        (
          groups.toList.map(g => option(value := g, g)),
          value <-- group,
          onChange --> {
            _.evalMap(_ => self.value.get)
              .foreach(g => group.set(g) *> weapon.set(weapons(g).head) *> style.set(weapons(g).head.styles.head) *> selectedFile.set(None))
          }
          )
      },
      select.withSelf { self =>
        (
          children <-- daWeapons.map(_.map(w => option(value := w.name, w.name)).toList),
          value <-- weapon.map(_.name),
          onChange --> {
            _.evalMap(_ => (self.value.get, daWeapons.get).tupled).map((it, weapons) => weapons.find(_.name == it)).unNone.foreach(it => weapon.set(it) *> style.set(it.styles.head) *> selectedFile.set(None))
          }
        )
      },
      select.withSelf { self =>
        (
          children <-- styles.map(_.traverse(s => option(value := s.pretty, s.pretty)).map(_.toList)),
          value <-- style.map(_.pretty),
          onChange --> {
            _.evalMap(_ => self.value.get).evalMap[IO, Option[WeaponStyle]](it => styles.map(_.find(_.pretty == it)).get).unNone.foreach(it => style.set(it) *> selectedFile.set(None))
          }
          )
      },
      p(
        "Flat icons",
        input.withSelf { self =>
          (
            tpe := "checkbox",
            checked <-- do2D,
            onChange --> {
              _.evalMap(_ => self.checked.get).foreach(do2D.set)
            }
          )
        }
      ),
      ImageGroup(selectedFile) 
      )
  )
}
def BuildMainWeaponDropdowns: IO[(SignallingRef[IO, Weapon], SignallingRef[IO, WeaponStyle], SignallingRef[IO, Boolean], 
  SignallingRef[IO, Option[String]], SignallingRef[IO, Option[String]], Resource[IO, HtmlElement[IO]])] = {
  val headWeapon = Mains.groupedWeapons(Mains.groups.head)
  for {
    weapon <- SignallingRef[IO].of(headWeapon.head)
    style <- SignallingRef[IO].of(headWeapon.head.styles.head)
    do2D <- SignallingRef[IO].of(false)
    selectedFile <- SignallingRef[IO].of[Option[String]](None)
    name <- SignallingRef[IO].of[Option[String]](None)
    group <- SignallingRef[IO].of(Mains.groups.head)
    dropdown <- IO(MainWeaponDropdowns(weapon, style, do2D, selectedFile, name, group, Mains.groupedWeapons, Mains.groups))
  } yield (weapon, style, do2D, selectedFile, name, dropdown)
}
def OnlyGameDropdown[T <: GameStyle](game: SignallingRef[IO, T], games: List[T]): Resource[IO, HtmlElement[IO]] = {
    div(
      select.withSelf { self =>
        (
        games.map(g => option(value := g.name, g.longName)),
        value <-- game.map(_.name),
        onChange --> {
          _.evalMap(_ => self.value.get).map(it => games.find(_.name == it)).unNone.foreach(game.set)
        }
          )
      }
      )
}
def BuildGameDropdown[T <: GameStyle](games: List[T]): IO[(SignallingRef[IO, T], Resource[IO, HtmlElement[IO]])] = {
  for {
    game <- SignallingRef[IO].of(games.head)
    dropdown <- IO(OnlyGameDropdown(game, games))
  } yield (game, dropdown)
}
def brandImage(name: String): String = {
  name.toLowerCase() match {
    case "none" => "resources/nothing.png"
    case name => 
      val n = name.replace(' ', '_')
      s"resources/brands/$n.png"
  }
}
def BrandDropdown(brand: SignallingRef[IO, String], brands: List[String]): Resource[IO, HtmlElement[IO]] = {
  div(
    img(
      src <-- brand.map(brandImage _),
      widthAttr := 64,
      heightAttr := 64
      ),
    select.withSelf { self =>
      (
        brands.map(b => option(value := b, b)),
        value <-- brand,
        onChange --> {
          _.evalMap(_ => self.value.get).foreach(brand.set)
        }
        )

    }
    )
}
def BuildBrandDropdown(brands: List[String]): IO[(SignallingRef[IO, String], Resource[IO, HtmlElement[IO]])] = {
  for {
    brand <- SignallingRef[IO].of(brands.head)
    dropdown <- IO(BrandDropdown(brand, brands))
  } yield (brand, dropdown)
}
def pathExists(path: String): IO[Boolean] = {
  IO.fromPromise(IO(Fetch.fetch(path, new RequestInit {
    method = HttpMethod.HEAD 
  }).`then`(
      (res) => res.ok,
      (_) => false
    )))
}
def weaponPath(weapon: Weapon, style : WeaponStyle, twodim : Boolean) : IO[String] = {

  val (noDimPath, goodPath) = weapon.basePath(style, twodim)

  for {
    path <- 
      if (twodim)
        for {
          exists <- pathExists("resources/" + goodPath)
        } yield {
          if (exists) goodPath else noDimPath 
        }
      else 
        for {
          exists <- pathExists("resources/" + noDimPath)
        } yield {
          if (exists) noDimPath else goodPath
        }
  } yield path

}
def otherPath(weapon: OtherWeapon, game: GameStyle): String = {
  weapon.root + "/" + game.name + "_" + weapon.name.replace(' ', '_').toLowerCase() + ".png"
}
def promiseFileReader[A](action: FileReader => Unit): IO[A] = {
  IO.async_[A] { cb =>
    val reader = new FileReader()
    reader.onloadend = (event) => {
      val result = reader.result.asInstanceOf[A]
      cb(Right(result))
    }
    reader.onerror = (error) => {
      println(error)
      cb(Left(js.JavaScriptException(error)))
    }
    action(reader)
  }
}
def loadImage(str: dom.File): IO[String] = {
    for {
      data <- promiseFileReader[String](_.readAsDataURL(str))
    } yield data
}
def renderKit(mainWeapon: Weapon, mainStyle: WeaponStyle, sub: OtherWeapon, subGame: GameStyle, 
  special: OtherWeapon, spGame: GameStyle, twodim: Boolean, kit: Game, brand: String,
  selectedMain: Option[String], selectedSub: Option[String], selectedSpecial: Option[String], spPoints: Option[String],
  mainName: Option[String], subName: Option[String], spName: Option[String], artist: Option[String]): IO[JSImageCanvas] = {
  val renderer = 
    kit match {
      case Splatoon1 => Splooge1KitGen.renderKit
      case Splatoon2 => Splooge2KitGen.renderKit
      case Splatoon3 => Splooge3KitGen.renderKit
    }
  val brandName = 
    brand.toLowerCase() match {
      case "none" => None
      case value => 
        Some(value.replace(' ', '_'))
    }
  val daMainName = mainName.getOrElse(mainWeapon.name)
  val daSubName = subName.getOrElse(sub.name)
  val daSpName = spName.getOrElse(special.name)
  for {
    mainImg <- selectedMain.map(Image.fromDataURL _).getOrElse(weaponPath(mainWeapon, mainStyle, twodim) >>= Image.loadFromResource)
    subImg <- selectedSub.map(Image.fromDataURL).getOrElse(Image.loadFromResource(otherPath(sub, subGame)))
    spImg <- selectedSpecial.map(Image.fromDataURL).getOrElse(Image.loadFromResource(otherPath(special, spGame)))
    brandImg <- brandName.traverse(it => Image.loadFromResource(s"brands/${it}.png"))
    res <- renderer(daMainName, mainImg, daSubName, subImg, daSpName, spImg, spPoints, brandImg, artist)
  } yield res.asInstanceOf[JSImageCanvas]
}
object App extends IOWebApp {
  def render: Resource[cats.effect.IO, HtmlElement[cats.effect.IO]] = 
    for {
      mainDropdownsGroup <- BuildMainWeaponDropdowns.toResource
      (mainWeapon, mainStyle, do2D, selectedMain, mainName, mainDropdowns) = mainDropdownsGroup
      subDropdownsGroup <- BuildOtherWeaponDropdowns("Sub name: ", Subs.weapons).toResource
      (sub, subGame, subImage, subName, subDropdowns) = subDropdownsGroup
      spDropdownsGroup <- BuildOtherWeaponDropdowns("Special name: ", Specials.weapons).toResource
      (special, spGame, spImage, spName, spDropdowns) = spDropdownsGroup
      kitDropdownGroup <- BuildGameDropdown[Game](Game.AllGames.toList).toResource
      (kit, kitDropdown) = kitDropdownGroup
      brandDropdownGroup <- BuildBrandDropdown(Brands.brands.prepended("None").toList).toResource
      (brand, brandDropdown) = brandDropdownGroup
      spPoints <- SignallingRef[IO].of[Option[String]](None).toResource
      artist <- SignallingRef[IO].of[Option[String]](None).toResource
      renderedKit <- SignallingRef[IO].of[String]("resources/nothing.png").toResource
      res <- {
        div(
          cls := "horz",
          div(
            mainDropdowns,
            subDropdowns,
            spDropdowns,
            kitDropdown,
            brandDropdown,
            div(
              p("Special points: "),
            input.withSelf { self => (
              tpe := "text",
              value <-- spPoints.map(_.getOrElse("")),
              onChange --> {
                _.evalMap(_ => self.value.get).map(it => if (it == "") None else Some(it)).foreach(spPoints.set _)
              }
              )}
            ),
            div(
              p("Kit made by: "),
              input.withSelf { self => (
                tpe := "text",
                onChange --> {
                  _.evalMap(_ => self.value.get).map(it => if (it == "") None else Some(it)).foreach(artist.set)
                }
              )}
              ),
            cls := "vertical",
            button(
              `type` := "button",
              "Generate!",
              onClick --> {
                _.evalMap { _ =>
                  (mainWeapon.get, mainStyle.get, sub.get, subGame.get, special.get, 
                    spGame.get, do2D.get, kit.get, brand.get, selectedMain.get,
                    subImage.get, spImage.get, spPoints.get,
                    mainName.get, subName.get, spName.get, artist.get).tupled
                }.evalMap { (main, mainStyle, sub, subGame, special, spGame, do2D, kit, 
                  brand, selectedMain, subImage, spImage, spPoints,
                  mainName, subName, spName, artist) =>
                  for {
                    _ <- IO.println(main.name)
                    img <- renderKit(main, mainStyle, sub, subGame, special, spGame, do2D, kit, 
                      brand, selectedMain, subImage, spImage, spPoints,
                      mainName, subName, spName, artist) 
                    data <- IO {
                      img.inner.toDataURL("image/png")
                    }
                  } yield data
                }.foreach(renderedKit.set)
              }
              )
            ),
          a(
            href <-- renderedKit,
            download := "kit.png",
            img(
              src <-- renderedKit,
              widthAttr := 512,
              cls := "alignEnd"
            ),
            cls := "alignEnd"
          )
        )
      }
    } yield res 
}
