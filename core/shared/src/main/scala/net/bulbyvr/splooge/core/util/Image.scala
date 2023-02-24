package net.bulbyvr.splooge.core.util

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
  def fillRect(x: Int, y: Int, width: Int, height: Int): Unit
  def drawImage(image: Image, x: Int, y: Int): Unit
  def drawImage(image: Image, transform: AffineTransform): Unit
  def setColor(color: Color): Unit
  def setComposite(mode: CompositeMode): Unit
  def setFont(font: FontInfo): Unit
  def drawString(txt: String, x: Int, y: Int): Unit
  def setInterpMode(mode: InterpMode): Unit
  def doFontAliasing(value: Boolean): Unit
  def complete(): Image
}

