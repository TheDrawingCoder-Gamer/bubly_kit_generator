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
import net.bulbyvr.splooge.core.util.JSImageCanvas
import org.scalajs.dom.window as dawindow
import org.scalajs.dom.Fetch
import org.scalajs.dom.RequestInit
import org.scalajs.dom.HttpMethod

def GameDropdown(game: SignallingRef[IO, GameStyle], games: Signal[IO, Seq[GameStyle]]): Resource[IO, HtmlElement[IO]] = {
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
def OtherWeaponDropdowns(weapon: SignallingRef[IO, OtherWeapon], game: SignallingRef[IO, GameStyle], weapons: Seq[OtherWeapon]): Resource[IO, HtmlElement[IO]] = { 
    val curGames = weapon.map(_.games)
    div(
      img(
        src <-- (weapon.asInstanceOf[Signal[IO, OtherWeapon]], game).tupled.map((w, g) => "resources/" + otherPath(w, g)),
        widthAttr := 64,
        heightAttr := 64,
        ),
      select.withSelf { self => 
        (
          weapons.map(w => option(value := w.name, w.name)),
          value <-- weapon.map(_.name),
          onChange --> {
            _.evalMap(_ => self.value.get).map(it => weapons.find(_.name == it)).unNone.foreach(it => weapon.set(it) *> game.set(it.games.head))
          }
          )
      },
      GameDropdown(game, curGames)
      )
}
def BuildOtherWeaponDropdowns(weapons: Seq[OtherWeapon]): IO[(SignallingRef[IO, OtherWeapon], SignallingRef[IO, GameStyle], Resource[IO, HtmlElement[IO]])] = {
  for {
    weapon <- SignallingRef[IO].of(weapons.head)
    game <- SignallingRef[IO].of(weapons.head.games.head)
    dropdown <- IO(OtherWeaponDropdowns(weapon, game, weapons))
  } yield (weapon, game, dropdown)
}
def MainWeaponDropdowns(weapon: SignallingRef[IO, Weapon], style: SignallingRef[IO, WeaponStyle], do2D: SignallingRef[IO, Boolean], weapons: Seq[Weapon]): Resource[IO, HtmlElement[IO]] = {
  val styles = weapon.map(_.styles)
  div(
    img(
      // src <-- (weapon.asInstanceOf[Signal[IO, Weapon]], style, do2D).tupled.discrete.evalMap(weaponPath _).holdResource("resources/unknown.png"),
      widthAttr := 64,
      heightAttr := 64
      ),
    select.withSelf { self =>
      (
        weapons.map(w => option(value := w.name, w.name)),
        value <-- weapon.map(_.name),
        onChange --> {
          _.evalMap(_ => self.value.get).map(it => weapons.find(_.name == it)).unNone.foreach(it => weapon.set(it) *> style.set(it.styles.head))
        }
      )
    },
    select.withSelf { self =>
      (
        children <-- styles.map(_.traverse(s => option(value := s.pretty, s.pretty)).map(_.toList)),
        value <-- style.map(_.pretty),
        onChange --> {
          _.evalMap(_ => self.value.get).evalMap[IO, Option[WeaponStyle]](it => styles.map(_.find(_.pretty == it)).get).unNone.foreach(style.set)
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
    )
    )
}
def BuildMainWeaponDropdowns(weapons: Seq[Weapon]): IO[(SignallingRef[IO, Weapon], SignallingRef[IO, WeaponStyle], SignallingRef[IO, Boolean], Resource[IO, HtmlElement[IO]])] = {
  for {
    weapon <- SignallingRef[IO].of(weapons.head)
    style <- SignallingRef[IO].of(weapons.head.styles.head)
    do2D <- SignallingRef[IO].of(false)
    dropdown <- IO(MainWeaponDropdowns(weapon, style, do2D, weapons))
  } yield (weapon, style, do2D, dropdown)
}
def OnlyGameDropdown(game: SignallingRef[IO, GameStyle], games: Seq[GameStyle]): Resource[IO, HtmlElement[IO]] = {
    IO.pure(game).toResource.flatMap { game =>
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
def renderKit(mainWeapon: Weapon, mainStyle: WeaponStyle, sub: OtherWeapon, subGame: GameStyle, special: OtherWeapon, spGame: GameStyle, twodim: Boolean): IO[JSImageCanvas] = {
  for {
    mainImg <- weaponPath(mainWeapon, mainStyle, twodim) >>= Image.loadFromResource
    subImg <- Image.loadFromResource(otherPath(sub, subGame))
    spImg <- Image.loadFromResource(otherPath(special, spGame))
    res <- Splooge3KitGen.renderKit(mainWeapon.name, mainImg, sub.name, subImg, special.name, spImg, None, None)
  } yield res.asInstanceOf[JSImageCanvas]
}
object App extends IOWebApp {
  def render: Resource[cats.effect.IO, HtmlElement[cats.effect.IO]] = 
    for {
      mainDropdownsGroup <- BuildMainWeaponDropdowns(Mains.weapons).toResource
      (mainWeapon, mainStyle, do2D, mainDropdowns) = mainDropdownsGroup
      subDropdownsGroup <- BuildOtherWeaponDropdowns(Subs.weapons).toResource
      (sub, subGame, subDropdowns) = subDropdownsGroup
      spDropdownsGroup <- BuildOtherWeaponDropdowns(Specials.weapons).toResource
      (special, spGame, spDropdowns) = spDropdownsGroup
      res <- {
        div(
          mainDropdowns,
          subDropdowns,
          spDropdowns,
          cls := "vertical",
          button(
            `type` := "button",
            "Generate!",
            onClick --> {
              _.evalMap { _ =>
                (mainWeapon.get, mainStyle.get, sub.get, subGame.get, special.get, spGame.get, do2D.get).tupled
              }.evalMap { (main, mainStyle, sub, subGame, special, spGame, do2D) =>
                for {
                  _ <- IO.println(main.name)
                  img <- renderKit(main, mainStyle, sub, subGame, special, spGame, do2D) 
                  data <- IO {
                    img.inner.toDataURL("image/png")
                  }
                } yield data
              }.foreach(data => IO(dawindow.open(data)))
            }
            )
          )
      }
    } yield res 
}
