package net.bulbyvr.splooge.core

import scala.collection.mutable as mut 
import scala.collection.immutable.ListMap
case class WeaponStyle(game: GameStyle, name: StyleName) {
  def pretty: String = {
    val daName = name match {
      case StyleName.Named(name) => name 
      case StyleName.Empty => ""
    }
    s"${game.name} ${daName}"
  }
}

case class Weapon(name: String, styles: Seq[WeaponStyle])
type WeaponStyles = Seq[WeaponStyle]
type AWeapon = WeaponStyles
def Style(name : StyleName, games : Seq[GameStyle]) = 
  games.map(game => (name, game))
given Conversion[WeaponBuilder, Seq[(StyleName, GameStyle)]] = _.getItems()
given Conversion[WeaponBuilder, AWeapon] = it => { 
  val items = it.getItems()
  items.map((a, b) => WeaponStyle(b, a))
}
trait WeaponBuilder {
  private final val items: mut.ArrayBuffer[(StyleName, GameStyle)] = mut.ArrayBuffer()
  extension (name: StyleName) {
    def +=(style: GameStyle): Unit = 
      items ++= Style(name, Seq(style))
    def ++=(styles: Seq[GameStyle]): Unit =
      items ++= Style(name, styles)
  }
  def add(it: Seq[(StyleName, GameStyle)]) = items ++= it
  final def getItems(): Seq[(StyleName, GameStyle)] =
    items.toSeq
}

trait MainBuilder(val name: String) extends WeaponBuilder {
  def build(): (String, Seq[(StyleName, GameStyle)]) = 
    name -> getItems()
}

def MapToWeapon(daMap : Map[String, AWeapon]) : Seq[Weapon] = 
  Seq.from(for ((k, styles) <- daMap) yield {
    Weapon(k, styles)
  })
def WeaponList(contents : (String, AWeapon)*) = 
  MapToWeapon(ListMap(contents:_*))

case class OtherWeapon(root : String, name : String, games : Seq[GameStyle])
type NonMainWeaponList = Seq[OtherWeapon]
def OtherWeaponList(root : String, contents : (String, Seq[GameStyle])*) : NonMainWeaponList = 
  contents.map((k, v) => OtherWeapon(root, k, v))

given buildToWeaponList: Conversion[MainBuilder, (String, AWeapon)] = it => {
  val (name, v) = it.build()
  name -> v.map((a, b) => WeaponStyle(b, a))
}
object Mains { 
  import Game.*
  import StyleName.*
  val Default = StyleName.Empty
  val heroWeapon = Style(StyleName.Named("Hero"), Seq(Splatoon2))
  val Simple3Weapon = Seq(WeaponStyle(Splatoon3, Default))
  val splatterscope = 
    new MainBuilder("") {
      Default ++= AllGames
      Named("Zekkofin") += Splatoon3
      Named("Kelp") += Splatoon1
      Named("Kensa") += Splatoon2
      Named("Sheldon's Picks") += Splatoon1
    }
  val eliter = buildToWeaponList(new MainBuilder("") {
      Default ++= AllGames
      Named("Custom") ++= OldGames
  })._2
  val weapons = WeaponList(
    new MainBuilder("52 Gal") {
      Default ++= AllGames
      Named("Deco") ++= OldGames
      Named("Kensa") += Splatoon2
    },
    new MainBuilder("96 Gal") {
      Default ++= AllGames
      Named("Deco") ++= AllGames
    },
    new MainBuilder("Aerospray") {
      Default ++= AllGames 
      Named("Gold") ++= AllGames
      Named("Sheldon's Picks") ++= OldGames
    },
    new MainBuilder("Ballpoint Splatling") {
      Default ++= NewGames
      Named("Nouveau") += Splatoon2
    },
    new MainBuilder("Bamboozler 14") {
      Default ++= AllGames
      Named("Grizzco") ++= NewGames
      Named("Cuttlegear") ++= OldGames
      Named("Sheldon's Picks") ++= OldGames
    },
    "Big Swig" -> Simple3Weapon,
    new MainBuilder("Blaster") {
      Default ++= AllGames
      Named("Grizzco") ++= NewGames
      Named("Custom") ++= OldGames
      add(heroWeapon)
    },
    new MainBuilder("Bloblobber") {
      Default ++= NewGames
      Named("Deco") += Splatoon2
    },
    new MainBuilder("Carbon Roller") {
      Default ++= AllGames
      Named("Deco") ++= AllGames
    },
    new MainBuilder("Clash Blaster") {
      Default ++= NewGames
      Named("Neo") ++= NewGames
    },
    new MainBuilder("Dapple Dualies") {
      Default ++= NewGames
      Named("Nouveau") ++= NewGames
      Named("Sheldon's Picks") += Splatoon2
    },
    new MainBuilder("Dualie Squelchers") {
      Default ++= NewGames
      Named("Custom") += Splatoon2
    },
    new MainBuilder("Dual Squelcher") {
      Default += Splatoon1
      Named("Custom") += Splatoon1
    },
    new MainBuilder("Dynamo Roller") {
      Default ++= AllGames
      Named("Gold") ++= OldGames
      Named("Kensa") += Splatoon2
      Named("Sheldon's Picks") += Splatoon1
    },
    ("E-Liter", eliter),
    ("E-Liter Scope", eliter),
    new MainBuilder("Explosher") {
      Default ++= NewGames
      Named("Custom") += Splatoon2
    },
    new MainBuilder("Flingza Roller") {
      Default ++= NewGames
      Named("Foil") += Splatoon2
    },
    new MainBuilder("Glooga Dualies") {
      Default ++= NewGames
      val deco = Named("Deco")
      deco += S3Custom
      deco += Splatoon2
      val kensa = Named("Kensa")
      kensa += S3Custom
      kensa += Splatoon2
    },
    new MainBuilder("Goo Tuber") {
      Default ++= NewGames
      Named("Custom") += Splatoon2
    },
    new MainBuilder("H-3 Nozzlenose") {
      Default ++= AllGames
      Named("D") ++= OldGames
      Named("Sheldon's Picks") ++= OldGames
    },
    new MainBuilder("Heavy Splatling") {
      Default ++= AllGames
      Named("Deco") ++= OldGames
      Named("Sheldon's Picks") ++= OldGames
      add(heroWeapon)
    },
    new MainBuilder("Hydra Splatling") {
      Default ++= AllGames
      Named("Custom") ++= OldGames
    },
    new MainBuilder("Inkbrush") {
      Default ++= AllGames
      Named("Nouveau") ++= AllGames
      Named("Sheldon's Picks") ++= OldGames
    },
    new MainBuilder("Jet Squelcher") {
      Default ++= AllGames
      Named("Custom") ++= AllGames
    },
    new MainBuilder("L-3 Nozzlenose") {
      Default ++= AllGames
      Named("D") ++= AllGames
      Named("Kensa") += Splatoon2
    },
    new MainBuilder("Luna Blaster") {
      Default ++= AllGames
      Named("Neo") ++= AllGames
      Named("Kensa") += Splatoon2
      Named("Emberz") += S3Custom
    },
    new MainBuilder("Mini Splatling") {
      Default ++= AllGames
      Named("Zink") ++= AllGames
      Named("Kensa") += Splatoon2
      Named("Sheldon's Picks") += S3Custom
      Named("Sheldon's Picks") += Splatoon1
    },
    new MainBuilder("N-Zap") {
      Default ++= AllGames
      Named("89") ++= Seq(Splatoon3, S3Custom, Splatoon2, Splatoon1)
      Named("Sheldon's Picks") ++= Seq(S3Custom, Splatoon2, Splatoon1)
    },
    new MainBuilder("Nautilus") {
      Default ++= NewGames
      Named("Gold") += Splatoon2
    },
    new MainBuilder("Octobrush") {
      Default ++= AllGames
      Named("Nouveau") ++= OldGames
      Named("Kensa") += Splatoon2
      add(heroWeapon)
    },
    new MainBuilder("Range Blaster") {
      Default ++= AllGames
      Named("Custom") ++= OldGames
      Named("Sheldon's Picks") += S3Custom
      Named("Sheldon's Picks") ++= OldGames
    },
    new MainBuilder("Rapid Blaster") {
      Default ++= AllGames
      Named("Deco") ++= AllGames
      Named("Kensa") += Splatoon2
      Named("Wicked") += S3Custom
    },
    
    new MainBuilder("Rapid Blaster Pro") {
      Default ++= AllGames
      Named("Deco") ++= OldGames
      Named("Wicked") += S3Custom
    },
    "REEF-LUX 450" -> new WeaponBuilder {
      Default += Splatoon3
    },
    new MainBuilder("Slosher") {
      Default ++= AllGames
      Named("Deco") ++= AllGames
      Named("Sheldon's Picks") ++= OldGames
      add(heroWeapon)
    },
    new MainBuilder("Sloshing Machine") {
      Default ++= AllGames
      Named("Grizzco") ++= NewGames
      Named("Neo") ++= OldGames
      Named("Kensa") += Splatoon2
    },
    "Snipewriter" -> new WeaponBuilder {
      Default += Splatoon3
    },
    new MainBuilder("Splash-o-matic") {
      Default ++= AllGames
      Named("Neo") ++= AllGames
    },
    new MainBuilder("Splat Brella") {
      Default ++= NewGames
      Named("Grizzco") ++= NewGames
      Named("Sorella") += Splatoon2
      add(heroWeapon)
    },
    new MainBuilder("Splat Charger") {
      add(splatterscope.build()._2)
      add(heroWeapon) 
    },
    new MainBuilder("Splat Dualies") {
      Default ++= NewGames
      Named("Enperry") += Splatoon2
      val kensa = Named("Kensa")
      kensa += S3Custom
      kensa += Splatoon2
      add(heroWeapon)
    },
    new MainBuilder("Splat Roller") {
      Default ++= AllGames
      Named("Hero") ++= OldGames
      val krakon = Named("Krak-on")
      krakon += S3Custom
      krakon ++= AllGames
      Named("Sheldon's Picks") += Splatoon1
      Named("Kensa") += Splatoon2
    },
    new MainBuilder("Splatana Stamper") {
      Default += Splatoon3
      Named("Grizzco") += Splatoon3
    },
    "Splatana Wiper" -> new WeaponBuilder {
      Default += Splatoon3
    },
    ("Splatterscope", buildToWeaponList(splatterscope)._2),
    new MainBuilder("Splattershot") {
      Default ++= AllGames
      val kensa = Named("Kensa")
      kensa += S3Custom
      kensa += Splatoon2
      Named("Hero") ++= AllGames
      Named("Sheldon's Picks") += Splatoon1
      Named("Tentatek") ++= AllGames
      // BEST SPLATTERSHOT :heart:
      val octo = Named("Octo")
      octo += S3Custom
      octo ++= OldGames
    },
    new MainBuilder("Splattershot Jr"){
      Default ++= AllGames
      Named("Custom") ++= AllGames
      Named("Kensa") += Splatoon2
    },
    ("Splattershot Nova", Simple3Weapon),
    new MainBuilder("Splattershot Pro") {
      Default ++= AllGames
      Named("Forge") ++= AllGames
      Named("Kensa") += Splatoon2
      Named("Sheldon's Picks") += Splatoon1
    },
    new MainBuilder("Sploosh-o-matic") {
      Default ++= AllGames
      Named("Neo") ++= AllGames
      Named("Sheldon's Picks") ++= OldGames
    },
    new MainBuilder("Squeezer") {
      Default ++= NewGames
      Named("Foil") += Splatoon2
    },
    new MainBuilder("Squiffer") {
      Default ++= AllGames
      Named("New") ++= OldGames
      Named("Sheldon's Picks") ++= OldGames
    },
    new MainBuilder("Tenta Brella") {
      Default ++= NewGames
      Named("Sorella") += Splatoon2
      Named("Sheldon's Picks") += Splatoon2
    },
    new MainBuilder("Tetra Dualies") {
      Default ++= NewGames
      Named("Tentatek") += Splatoon2
    },
    new MainBuilder("Tri-Slosher") {
      Default ++= AllGames
      Named("Nouveau") ++= AllGames

    },
    new MainBuilder("Tri-Stringer") {
      Default += Splatoon3
      Named("Grizzco") += Splatoon3
    },
    new MainBuilder("Undercover Brella") {
      Default ++= NewGames
      Named("Sorella") += Splatoon2
      Named("Kensa") += Splatoon2
      Named("Sheldon's Picks") += S3Custom
    }
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
    ("Seeker", Seq(S3Custom, Splatoon1))
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
      "Kraken Royale" -> Seq(Splatoon3),
      ("Reefslider", Seq(Splatoon3, Splatoon1)),
      ("Splashdown", Seq(Splatoon3, Splatoon2)),
      ("Super Chump", Seq(Splatoon3, S3Custom)),
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
      ("Autobomb Rush", Seq(S3Custom)),
      ("Autobomb Launcher", Seq(Splatoon2)),
      ("Baller", Seq(
        S3Custom, 
        Splatoon2
      )),
      ("Bubbler", Seq(
        S3Custom, 
        Splatoon1
      )),
      ("Burst Bomb Rush", Seq(
        S3Custom,
        Splatoon1
      )),
      ("Burst Bomb Launcher", Seq(
        Splatoon2
      )),
      ("Curling Bomb Rush", Seq(S3Custom)),
      ("Curling Bomb Launcher", Seq(
        Splatoon2 
      )),
      ("Echolocator", Seq(
        S3Custom, 
        Splatoon1
      )),
      ("Ink Armor", Seq(
        S3Custom, 
        Splatoon2
      )),
      ("Inkstrike", Seq(
        S3Custom,
        Splatoon1
      )),
      ("Killer Wail", Seq(
        S3Custom,
        Splatoon1
      )),
      ("Kraken", Seq(
        S3Custom,
        Splatoon1
      )),
      ("Seeker Bomb Rush", Seq(
        S3Custom,
        Splatoon1
      )),
      ("Splat Bomb Rush", Seq(
        S3Custom,
        Splatoon1
      )),
      ("Splat Bomb Launcher", Seq(
        Splatoon2
      )),
      ("Suction Bomb Rush", Seq(
        S3Custom,
        Splatoon1
      )),
      ("Suction Bomb Launcher", Seq(
        Splatoon2
      )),
      ("Inkzooka", Seq(S3Custom, Splatoon1)),
      ("Bubble Blower", Seq(S3Custom, Splatoon2)),
      ("Sting Ray", Seq(S3Custom, Splatoon2))

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
      "Zekkofin",
      "Zink Blue",
      "Zink White",
      "Zink Yellow"
    )
}
