package net.bulbyvr.splooge.core

trait GameStyle {
  val name: String
  val longName: String
}

sealed trait Game extends GameStyle 

object Game {
  case object Splatoon1 extends Game {
    val name = "s"
    val longName = "Splatoon"
  }
  case object Splatoon2 extends Game {
    val name = "s2"
    val longName = "Splatoon 2"
  }
  case object Splatoon3 extends Game {
    val name = "s3"
    val longName = "Splatoon 3"
  }
  val OldGames = Seq(Splatoon2, Splatoon1)
  val NewGames = Seq(Splatoon3, Splatoon2)
  val AllGames = Seq(Splatoon3, Splatoon2, Splatoon1)
}

object S3Custom extends GameStyle {
  val name = "s3custom"
  val longName = "Splatoon 3 (Custom)"
}

object MSPaint extends GameStyle {
  val name = "ms_paint"
  val longName = "MS Paint"
}

enum StyleName {
  case Named(name: String)
  case Empty
}
