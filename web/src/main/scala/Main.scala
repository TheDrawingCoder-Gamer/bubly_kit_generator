import net.bulbyvr.splooge.core.*
import fs2.dom.*
import calico.*
import calico.html.io.{*, given}
import cats.effect.*
import fs2.*
import fs2.concurrent.*

def otherWeaponCellRenderer(weapon: OtherWeapon): Resource[IO, HtmlElement[IO]] = 
  SignallingRef[IO].of(weapon.name)
