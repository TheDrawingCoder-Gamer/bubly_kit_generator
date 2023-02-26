package net.bulbyvr.splooge.core.util

import cats.effect.*
trait Image {
  def width: Int
  def height: Int
  def getCanvas(): Canvas
}
object Image extends ImageFactory
enum CompositeMode {
  case SrcOver
  case SrcIn
  case SrcOut
  case SrcAtop
  case DestOver
  case DestIn
  case DestOut
  case DestAtop
}
case class Color(r: Int, g: Int, b: Int, a: Int)

enum InterpMode {
  case Linear
  case Nearest
}

trait Canvas {
  def fillRect(x: Int, y: Int, width: Int, height: Int): IO[Unit]
  def drawImage(image: Image, x: Int, y: Int): IO[Unit]
  def drawImage(image: Image, transform: AffineTransform): IO[Unit]
  def setColor(color: Color): IO[Unit]
  def setComposite(mode: CompositeMode): IO[Unit]
  def setFont(font: FontInfo): IO[Unit]
  def drawString(txt: String, x: Int, y: Int): IO[Unit]
  def setInterpMode(mode: InterpMode): IO[Unit]
  def doFontAliasing(value: Boolean): IO[Unit]
  def complete(): IO[Image]
}

