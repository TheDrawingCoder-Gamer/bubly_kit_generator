package net.bulbyvr.splooge.core

import scala.collection.mutable as mut 
import scala.collection.immutable.ListMap
case class WeaponStyle(game: GameStyle, name: StyleName) {
  def pretty: String = {
    val daName = name match {
      case StyleName.Named(name) => name 
      case StyleName.Empty => ""
    }
    s"${game.name} ${daName}".trim()
  }
}

case class Weapon(name: String, styles: Seq[WeaponStyle], group: String) {
  /**
   * @returns a tuple containing (3dPath, 2dPath)
   */
  def basePath(style: WeaponStyle, twodim: Boolean): (String, String) = {
    val wName = name.replace(' ', '_').replace('.', '_').toLowerCase()
    val sName = style.name match {
      case StyleName.Named(name) => StyleName.Named(name.trim().replace(' ', '_').replace('.', '_').toLowerCase())
      case StyleName.Empty => StyleName.Empty
    }
    
    val gamePrefix = style.game.name
    val firstPath = 
      sName match {
        case StyleName.Named(sName) => gamePrefix + "_" + sName + "_" + wName
        case StyleName.Empty => gamePrefix + "_" + wName
      }
    (s"weapons/$firstPath.png", s"weapons/${firstPath}_2d.png")
      
  }
}
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

trait MainBuilder(val name: String, val group: String) extends WeaponBuilder {
  def build(): ((String, String), Seq[(StyleName, GameStyle)]) = 
    (name, group) -> getItems()
}

def MapToWeapon(daMap : Map[(String, String), AWeapon]) : Seq[Weapon] = 
  Seq.from(for (((k, g), styles) <- daMap) yield {
    Weapon(k, styles, g)
  })
def WeaponList(contents : ((String, String), AWeapon)*) = 
  MapToWeapon(ListMap(contents:_*))

case class OtherWeapon(root : String, name : String, games : Seq[GameStyle])
type NonMainWeaponList = Seq[OtherWeapon]
def OtherWeaponList(root : String, contents : (String, Seq[GameStyle])*) : NonMainWeaponList = 
  contents.map((k, v) => OtherWeapon(root, k, v))

given buildToWeaponList: Conversion[MainBuilder, ((String, String), AWeapon)] = it => {
  val (name, v) = it.build()
  name -> v.map((a, b) => WeaponStyle(b, a))
}
object Mains { 
  import Game.*
  import StyleName.*
  val Default = StyleName.Empty
  val Charger = "Charger"
  val Shooter = "Shooter"
  val Blaster = "Blaster"
  val Splatling = "Splatling"
  val Slosher = "Slosher"
  val Splatana = "Splatana"
  val Bow = "Stringer"
  val Roller = "Roller"
  val Dualies = "Dualies"
  val Brella = "Brella"
  val Brush = "Brush"
  val groups = Seq(
      Shooter,
      Roller,
      Charger,
      Slosher,
      Splatling,
      Dualies,
      Brella,
      Blaster,
      Brush,
      Bow,
      Splatana
    )
  val heroWeapon = Style(StyleName.Named("Hero"), Seq(Splatoon2))
  def Simple3Weapon(name: String, group: String) = 
    new MainBuilder(name, group) {
      Default += Splatoon3
    }
  def splatchargerShared(build: WeaponBuilder) = {
    import build.*
    Default ++= AllGames
    Named("Zekkofin") += Splatoon3
    Named("Barazushi") += Splatoon3
    Named("Firefin") += Splatoon2
    Named("Kelp") += Splatoon1
    Named("Kensa") += Splatoon2
    Named("Sheldon's Picks") += Splatoon1
  }
  def eliter(name: String) = new MainBuilder(name, Charger) {
      Default ++= AllGames
      Named("Custom") ++= AllGames
  }
  lazy val groupedWeapons: Map[String, Seq[Weapon]] = { weapons.groupBy(_.group) }
  val weapons = WeaponList(
    new MainBuilder(".52 Gal", Shooter) {
      Default ++= AllGames
      Named("Deco") ++= AllGames
      Named("Deco") += S3Custom
      Named("Kensa") += Splatoon2
      Named("Wicked") += S3Custom
    },
    new MainBuilder(".96 Gal", Shooter) {
      Default ++= AllGames
      Named("Deco") ++= AllGames
      Named("Emberz") += Splatoon3
    },
    new MainBuilder("Aerospray", Shooter) {
      Default ++= AllGames 
      Named("Gold") ++= AllGames
      Named("Emberz") += Splatoon3
      Named("Sheldon's Picks") ++= OldGames
    },
    new MainBuilder("Ballpoint Splatling", Splatling) {
      Default ++= NewGames
      Named("Nouveau") ++= NewGames
    },
    new MainBuilder("Bamboozler 14", Charger) {
      Default ++= AllGames
      Named("Grizzco") ++= NewGames
      Named("Cuttlegear") ++= AllGames
      Named("Sheldon's Picks") ++= OldGames
    },
    new MainBuilder("Big Swig", Roller) {
      Default += Splatoon3
      Named("Express") += Splatoon3
      Named("Emberz") += Splatoon3
      Named("Pear") += S3Custom
    },
    new MainBuilder("Blaster", Blaster) {
      Default ++= AllGames
      Named("Grizzco") ++= NewGames
      Named("Custom") ++= AllGames
      Named("Emberz") += Splatoon3
      add(heroWeapon)
    },
    new MainBuilder("Bloblobber", Slosher) {
      Default ++= NewGames
      Named("Deco") ++= NewGames
    },
    new MainBuilder("Carbon Roller", Roller) {
      Default ++= AllGames
      Named("Deco") ++= AllGames
      Named("Barazushi") += Splatoon3
    },
    new MainBuilder("Clash Blaster", Blaster) {
      Default ++= NewGames
      Named("Neo") ++= NewGames
    },
    new MainBuilder("Dapple Dualies", Dualies) {
      Default ++= NewGames
      Named("Nouveau") ++= NewGames
      Named("Barazushi") += Splatoon3
      Named("Sheldon's Picks") += Splatoon2
    },
    new MainBuilder("Dread Wringer", Slosher) {
      Default += Splatoon3
      Named("D") += Splatoon3
      Named("Emberz") += Splatoon3
    },
    new MainBuilder("Douser Dualies", Dualies) {
      Default += Splatoon3
      Named("Custom") += Splatoon3
    },
    new MainBuilder("Dualie Squelchers", Dualies) {
      Default ++= NewGames
      Named("Custom") ++= NewGames
      Named("Custom") += S3Custom
      Named("Emberz") += Splatoon3
    },
    new MainBuilder("Dual Squelcher", Shooter) {
      Default += Splatoon1
      Default += S3Custom
      Named("Custom") += Splatoon1
      Named("Custom") += S3Custom
    },
    new MainBuilder("Dynamo Roller", Roller) {
      Default ++= AllGames
      Named("Gold") += S3Custom
      Named("Gold") ++= AllGames
      Named("Emberz") += Splatoon3
      Named("Kensa") += Splatoon2
      Named("Sheldon's Picks") += Splatoon1
      Named("Grizzco") += Splatoon3
      
    },
    eliter("E-Liter"),
    eliter("E-Liter Scope"),
    new MainBuilder("Explosher", Slosher) {
      Default ++= NewGames
      Named("Custom") ++= NewGames
    },
    new MainBuilder("Flingza Roller", Roller) {
      Default ++= NewGames
      Named("Foil") ++= NewGames
      Named("Deco") += S3Custom
    },
    new MainBuilder("Glooga Dualies", Dualies) {
      Default ++= NewGames
      val deco = Named("Deco")
      deco += S3Custom
      deco ++= NewGames
      val kensa = Named("Kensa")
      kensa += S3Custom
      kensa += Splatoon2
      Named("Zekkofin") += S3Custom
      Named("Cherry") += S3Custom
    },
    new MainBuilder("Goo Tuber", Charger) {
      Default ++= NewGames
      Named("Custom") ++= NewGames
    },
    new MainBuilder("H-3 Nozzlenose", Shooter) {
      Default ++= AllGames
      Named("D") ++= AllGames
      Named("Sheldon's Picks") ++= OldGames
      Named("Barazushi") += Splatoon3
    },
    new MainBuilder("Heavy Edit Splatling", Splatling) {
      Default += Splatoon3
      Named("Nouveau") += Splatoon3
    },
    new MainBuilder("Heavy Splatling", Splatling) {
      Default ++= AllGames
      Named("Deco") ++= AllGames
      Named("Order") += Splatoon3
      Named("Sheldon's Picks") ++= OldGames
      add(heroWeapon)
    },
    new MainBuilder("Hydra Splatling", Splatling) {
      Default ++= AllGames
      Named("Custom") ++= AllGames
      Named("Emberz") += Splatoon3
      Named("Copper") += S3Custom
      Named("Gold") += S3Custom
    },
    new MainBuilder("Inkbrush", Brush) {
      Default ++= AllGames
      Named("Nouveau") ++= AllGames
      Named("Sheldon's Picks") ++= OldGames
    },
    new MainBuilder("Jet Squelcher", Shooter) {
      Default ++= AllGames
      Named("Custom") ++= AllGames
      Named("Barazushi") += Splatoon3
    },
    new MainBuilder("L-3 Nozzlenose", Shooter) {
      Default ++= AllGames
      Named("D") ++= AllGames
      Named("Emberz") += Splatoon3
      Named("Kensa") += Splatoon2
    },
    new MainBuilder("Luna Blaster", Blaster) {
      Default ++= AllGames
      Named("Neo") ++= AllGames
      Named("Order") += Splatoon3
      Named("Kensa") += Splatoon2
      Named("Emberz") += S3Custom
      Named("Pastel") += S3Custom
    },
    new MainBuilder("Mini Splatling", Splatling) {
      Default ++= AllGames
      Named("Zink") ++= AllGames
      Named("Kensa") += Splatoon2
      Named("Barazushi") += Splatoon3
      Named("Sheldon's Picks") += S3Custom
      Named("Sheldon's Picks") += Splatoon1
      Named("Zekkofin") += S3Custom
      Named("Leaf") += S3Custom
    },
    new MainBuilder("Mint Decavitator", Splatana) {
      Default += Splatoon3
      Named("Neo") += Splatoon3
    },
    new MainBuilder("N-Zap", Shooter) {
      Default ++= AllGames
      Named("89") ++= Seq(Splatoon3, S3Custom, Splatoon2, Splatoon1)
      Named("Sheldon's Picks") ++= Seq(S3Custom, Splatoon2, Splatoon1)
    },
    new MainBuilder("Nautilus", Splatling) {
      Default ++= NewGames
      Named("Gold") ++= NewGames
    },
    new MainBuilder("Octobrush", Brush) {
      Default ++= AllGames
      Named("Nouveau") ++= AllGames
      Named("Order") += Splatoon3
      Named("Emberz") += Splatoon3
      Named("Kensa") += Splatoon2
      add(heroWeapon)
    },
    new MainBuilder("Painbrush", Brush) {
      Default += Splatoon3
      Named("Nouveau") += Splatoon3
      Named("Barazushi") += Splatoon3
    },
    new MainBuilder("Range Blaster", Blaster) {
      Default ++= AllGames
      Named("Custom") ++= AllGames
      Named("Sheldon's Picks") += S3Custom
      Named("Sheldon's Picks") ++= OldGames
      Named("Zekkofin") += S3Custom
      Named("Cherry") += S3Custom
    },
    new MainBuilder("Rapid Blaster", Blaster) {
      Default ++= AllGames
      Named("Deco") ++= AllGames
      Named("Kensa") += Splatoon2
      Named("Wicked") += S3Custom
    },
    
    new MainBuilder("Rapid Blaster Pro", Blaster) {
      Default ++= AllGames
      Named("Deco") ++= AllGames
      Named("Barazushi") += Splatoon3
      Named("Wicked") += S3Custom
    },
    new MainBuilder("Recycled Brella", Brella) {
      Default += Splatoon3
      Named("Cuttlegear") += Splatoon3
    },
    new MainBuilder("REEF-LUX 450", Bow) {
      Default += Splatoon3
      Named("Deco") += Splatoon3
      Named("Barazushi") += Splatoon3
      Named("Gold") += S3Custom
    },
    new MainBuilder("S-BLAST", Blaster) {
      Default += Splatoon3
      Named("91") += Splatoon3
    },
    new MainBuilder("Slosher", Slosher) {
      Default ++= AllGames
      Named("Deco") ++= AllGames
      Named("Order") += Splatoon3
      Named("Sheldon's Picks") ++= OldGames
      Named("Zekkofin") += S3Custom
      Named("Melon") += S3Custom
      add(heroWeapon)
    },
    new MainBuilder("Sloshing Machine", Slosher) {
      Default ++= AllGames
      Named("Grizzco") ++= NewGames
      Named("Neo") += S3Custom
      Named("Neo") ++= AllGames
      Named("Kensa") += Splatoon2
      Named("Zekkofin") += S3Custom
    },
    new MainBuilder("Snipewriter", Charger) {
      Default += Splatoon3
      Named("Nouveau") += Splatoon3
    },
    new MainBuilder("Splash-o-matic", Shooter) {
      Default ++= AllGames
      // 3.0.0 updated the sploosh & splash icons
      Named("Base") += Splatoon3
      Named("Neo") ++= AllGames
      Named("Barazushi") += Splatoon3
      Named("Copper") += S3Custom
    },
    new MainBuilder("Splat Brella", Brella) {
      Default ++= NewGames
      Named("Grizzco") ++= NewGames
      Named("Order") += Splatoon3
      Named("Sorella") ++= NewGames
      Named("Cherry") += S3Custom
      add(heroWeapon)
    },
    new MainBuilder("Splat Charger", Charger) {
      splatchargerShared(this)
      Named("Order") += Splatoon3
      Named("Pastel") += S3Custom
      add(heroWeapon) 
    },
    new MainBuilder("Splat Dualies", Dualies) {
      Default ++= NewGames
      Named("Enperry") += Splatoon3
      Named("Enperry") += S3Custom
      Named("Enperry") += Splatoon2
      Named("Emberz") += Splatoon3
      val kensa = Named("Kensa")
      kensa += S3Custom
      kensa += Splatoon2
      Named("Order") += Splatoon3
      Named("Zekkofin") += S3Custom
      Named("Grizzco") += Splatoon3
      add(heroWeapon)
    },
    new MainBuilder("Splat Roller", Roller) {
      Default ++= AllGames
      Named("Hero") ++= OldGames
      Named("Order") += Splatoon3
      val krakon = Named("Krak-on")
      krakon += S3Custom
      krakon ++= AllGames
      Named("Sheldon's Picks") += Splatoon1
      Named("Kensa") += Splatoon2
      Named("Enperry") += S3Custom
    },
    new MainBuilder("Splatana Stamper", Splatana) {
      Default += Splatoon3
      Named("Nouveau") += Splatoon3
      Named("Emberz") += Splatoon3
      Named("Order") += Splatoon3
      Named("Grizzco") += Splatoon3
    },
    new MainBuilder("Splatana Wiper", Splatana) {
      Default += Splatoon3
      Named("Deco") += Splatoon3
      Named("Barazushi") += Splatoon3
    },
    new MainBuilder("Splatterscope", Charger) {
      splatchargerShared(this)
    },
    new MainBuilder("Splattershot", Shooter) {
      Default ++= AllGames
      val kensa = Named("Kensa")
      kensa += S3Custom
      kensa += Splatoon2
      Named("Order") += Splatoon3
      Named("Hero") ++= AllGames
      Named("Sheldon's Picks") += Splatoon1
      Named("Tentatek") ++= AllGames
      Named("Emberz") += Splatoon3
      // BEST SPLATTERSHOT :heart:
      val octo = Named("Octo")
      octo += S3Custom
      octo ++= AllGames
      Named("Zekkofin") += S3Custom
      Named("Zekkofin 2") += S3Custom
      Named("Zekkofin 3") += S3Custom
    },
    new MainBuilder("Splattershot Jr", Shooter){
      Default ++= AllGames
      Named("Custom") ++= AllGames
      Named("Kensa") += Splatoon2
    },
    new MainBuilder("Splattershot Nova", Shooter) {
      Default += Splatoon3
      Named("Annaki") += Splatoon3
      Named("Tentatek") += S3Custom
      Named("Pastel") += S3Custom
    },
    new MainBuilder("Splattershot Pro", Shooter) {
      Default ++= AllGames
      Named("Forge") ++= AllGames
      Named("Barazushi") += Splatoon3
      Named("Kensa") += Splatoon2
      Named("Sheldon's Picks") += Splatoon1
    },
    new MainBuilder("Sploosh-o-matic", Shooter) {
      Default ++= AllGames
      // v3.0.0 changed the icon of sploosh & splash
      Named("Base") += Splatoon3
      Named("Neo") ++= AllGames
      Named("Sheldon's Picks") ++= OldGames
    },
    new MainBuilder("Squeezer", Shooter) {
      Default ++= NewGames
      Named("Foil") ++= NewGames
    },
    new MainBuilder("Squiffer", Charger) {
      Default ++= AllGames
      Named("New") ++= AllGames
      Named("Sheldon's Picks") ++= OldGames
    },
    new MainBuilder("Tenta Brella", Brella) {
      Default ++= NewGames
      Named("Sorella") ++= NewGames
      Named("Barazushi") += Splatoon3
      Named("Sheldon's Picks") += Splatoon2
    },
    new MainBuilder("Tetra Dualies", Dualies) {
      Default ++= NewGames
      Named("Tentatek") ++= NewGames
    },
    new MainBuilder("Tri-Slosher", Slosher) {
      Default ++= AllGames
      Named("Nouveau") ++= AllGames
      Named("Barazushi") += Splatoon3
      Named("Zekkofin") += S3Custom
      Named("Pastel") += S3Custom

    },
    new MainBuilder("Tri-Stringer", Bow) {
      Default += Splatoon3
      Named("Inkline") += Splatoon3
      Named("Grizzco") += Splatoon3
      Named("Order") += Splatoon3
      Named("Emberz") += Splatoon3
      Named("Emberz") += S3Custom
    },
    new MainBuilder("Undercover Brella", Brella) {
      Default ++= NewGames
      Named("Sorella") ++= NewGames
      Named("Emberz") += Splatoon3
      Named("Kensa") += Splatoon2
      Named("Sheldon's Picks") += S3Custom
    },
    new MainBuilder("Wellstring V", Bow) {
      Default += Splatoon3
      Named("Custom") += Splatoon3
    }
  )
}

object Subs {
  import Game.*
  val weapons = OtherWeaponList("sub",  
    ("Angle Shooter", Seq(Splatoon3, MSPaint)),
    ("Autobomb", Seq(Splatoon3, Splatoon2, MSPaint)),
    ("Burst Bomb", Seq(Splatoon3, Splatoon2, Splatoon1, MSPaint)),
    ("Curling Bomb", Seq(Splatoon3, Splatoon2, MSPaint)),
    ("Fizzy Bomb", Seq(Splatoon3, Splatoon2, MSPaint)),
    ("Ink Mine", Seq(Splatoon3, Splatoon2, Splatoon1, MSPaint)),
    ("Point Sensor", Seq(Splatoon3, Splatoon2, Splatoon1, MSPaint)),
    ("Smallfry", Seq(Splatoon3)),
    ("Splash Wall", Seq(Splatoon3, Splatoon2, Splatoon1, MSPaint)),
    ("Splat Bomb", Seq(Splatoon3, Splatoon2, Splatoon1, MSPaint)),
    ("Sprinkler", Seq(Splatoon3, Splatoon2, Splatoon1, MSPaint)),
    ("Squid Beakon", Seq(Splatoon3, Splatoon2, Splatoon1, MSPaint)),
    ("Suction Bomb", Seq(Splatoon3, Splatoon2, Splatoon1, MSPaint)),
    ("Torpedo", Seq(Splatoon3, Splatoon2, MSPaint)),
    ("Toxic Mist", Seq(Splatoon3, Splatoon2, MSPaint)),
    ("Disruptor", Seq(Splatoon1)),
    ("Seeker", Seq(S3Custom, Splatoon1))
    )
}


object Specials {
  import Game.* 

  val weapons = OtherWeaponList("specials",
      ("Big Bubbler", Seq(Splatoon3, MSPaint)),
      ("Booyah Bomb", Seq(Splatoon3, Splatoon2, MSPaint)),
      ("Crab Tank", Seq(Splatoon3, Splatoon1, MSPaint)),
      ("Ink Storm", Seq(Splatoon3, Splatoon2, Splatoon1, MSPaint)),
      ("Ink Vac", Seq(Splatoon3, Splatoon1, MSPaint)),
      ("Inkjet", Seq(Splatoon3, Splatoon2, Splatoon1, MSPaint)),
      ("Killer Wail 5.1", Seq(Splatoon3, Splatoon1, MSPaint)),
      "Kraken Royale" -> Seq(Splatoon3, MSPaint),
      ("Reefslider", Seq(Splatoon3, Splatoon1, MSPaint)),
      ("Splashdown", Seq(Splatoon3, Splatoon2)),
      ("Splattercolor Screen", Seq(Splatoon3, MSPaint)),
      ("Super Chump", Seq(Splatoon3, S3Custom, MSPaint)),
      ("Tacticooler",  Seq(Splatoon3, Splatoon1, MSPaint)),
      ("Tenta Missiles", Seq(Splatoon3, Splatoon2, Splatoon1, MSPaint)),
      ("Triple Inkstrike", Seq(Splatoon3, Splatoon1, MSPaint)),
      ("Triple Splashdown", Seq(Splatoon3, MSPaint)),
      ("Trizooka", Seq(
        Splatoon3,
        Splatoon2,
        Splatoon1,
        MSPaint
      )),
      ("Ultra Stamp", Seq(Splatoon3,Splatoon2,Splatoon1, MSPaint)),
      ("Wave Breaker", Seq(Splatoon3, Splatoon1, MSPaint)),
      ("Zipcaster", Seq(
        Splatoon3,
        Splatoon2,
        Splatoon1,
        MSPaint
      )),
      ("Autobomb Rush", Seq(S3Custom)),
      ("Autobomb Launcher", Seq(Splatoon2)),
      ("Baller", Seq(
        S3Custom, 
        Splatoon2,
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
      ("Inkzooka", Seq(S3Custom, Splatoon2, Splatoon1)),
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
      "Express",
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
