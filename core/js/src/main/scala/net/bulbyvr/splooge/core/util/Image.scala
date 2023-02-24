package net.bulbyvr.splooge.core.util

import org.scalajs.dom.{HTMLCanvasElement, CanvasRenderingContext2D, HTMLImageElement, document}
import org.scalajs.dom.HTMLElement
sealed trait JSImage extends Image
class JSImageElement(val inner: HTMLImageElement) extends JSImage {
  def width = inner.width
  def height = inner.height
  def getCanvas() = {
    val canvas = document.createElement("canvas").asInstanceOf[HTMLCanvasElement]
    val ctx = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
    JSCanvas(canvas, ctx)
  }
}
class JSImageCanvas(val inner: HTMLCanvasElement) extends JSImage {
  def width = inner.clientWidth
  def height = inner.clientHeight
  def getCanvas() = {
    val ctx = inner.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
    JSCanvas(inner, ctx)
  }
}
class JSCanvas(val inner: HTMLCanvasElement, val context: CanvasRenderingContext2D) extends Canvas {
  var curFont: FontInfo = null
  def complete() = {
    JSImageCanvas(inner)
  }
  def doFontAliasing(value: Boolean): Unit = ()
  def setInterpMode(mode: InterpMode): Unit = {
    mode match {
      case InterpMode.Linear =>
        context.imageSmoothingEnabled = true
      case InterpMode.Nearest =>
        context.imageSmoothingEnabled = false
    }
  }
  def setComposite(mode: CompositeMode): Unit = {
    val good = mode match {
      case CompositeMode.SrcOver => 
        "source-over"
      case CompositeMode.SrcIn =>
        "source-in"
      case CompositeMode.SrcOut =>
        "source-out"
      case CompositeMode.SrcAtop =>
        "source-atop"
      case CompositeMode.DestOver =>
        "destination-over"
      case CompositeMode.DestIn =>
        "destination-in"
      case CompositeMode.DestOut =>
        "destination-out"
      case CompositeMode.DestAtop =>
        "destination-atop"
    }
    context.globalCompositeOperation = good
  }
  def drawImage(image: Image, x: Int, y: Int): Unit = {
    val elem: HTMLElement = image.asInstanceOf[JSImage] match {
      case img: JSImageElement => img.inner
      case canvas: JSImageCanvas => canvas.inner
    }
    context.drawImage(elem, x, y)
  }
  def drawImage(image: Image, transform: AffineTransform): Unit = {
    context.setTransform(transform.m00, transform.m01, transform.m10, transform.m11, transform.m20, transform.m21)
    val elem: HTMLElement = image.asInstanceOf[JSImage] match {
      case img: JSImageElement => img.inner
      case canvas: JSImageCanvas => canvas.inner
    }
    context.drawImage(elem, 0, 0)
    context.setTransform(1, 0, 0, 1, 0, 0)
  }
  def fillRect(x: Int, y: Int, width: Int, height: Int): Unit = {
    context.fillRect(x, y, width, height)
  }
  def setColor(color: Color): Unit = {
    val str = s"rgb(${color.r}, ${color.g}, ${color.b}, ${color.a})"
    context.fillStyle = str
  }
  def setFont(font: FontInfo): Unit = {
    curFont = font

  }
  def drawString(txt: String, x: Int, y: Int): Unit = {
    context.fillText(txt, x, y)
  }
}
