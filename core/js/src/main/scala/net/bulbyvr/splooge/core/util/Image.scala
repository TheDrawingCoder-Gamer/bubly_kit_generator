package net.bulbyvr.splooge.core.util

import org.scalajs.dom.{HTMLCanvasElement, CanvasRenderingContext2D, HTMLImageElement, document}
import org.scalajs.dom.HTMLElement
import cats.effect.IO
import scala.scalajs.js.Promise
import scala.util.Try
import typings.cssFontLoadingModule.*
import typings.cssFontLoadingModule.mod.global.FontFace
import typings.cssFontLoadingModule.mod.global.FontFaceCls
import typings.cssFontLoadingModule.mod.global.FontFaceDescriptors
import scala.scalajs.js
sealed trait JSImage extends Image
class JSImageElement(val inner: HTMLImageElement) extends JSImage {
  def width = inner.width
  def height = inner.height
  def getCanvas() = {
    val canvas = document.createElement("canvas").asInstanceOf[HTMLCanvasElement]
    val ctx = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
    JSCanvas(canvas, ctx)
  }
  def promiseLoad(): IO[JSImageElement] = {
    if (inner.complete)
      IO(this)
    else {
      IO.fromPromise(IO(Promise((resolve, reject) => {
        inner.addEventListener("load", (event) => resolve(this))
        inner.addEventListener("error", reject.apply _)
      })))
    }
  }
}
class JSImageCanvas(val inner: HTMLCanvasElement) extends JSImage {
  def width = inner.width
  def height = inner.height
  def getCanvas() = {
    val ctx = inner.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
    JSCanvas(inner, ctx)
  }
}
class JSCanvas(val inner: HTMLCanvasElement, val context: CanvasRenderingContext2D) extends Canvas {
  var curFont: FontFace = null
  var curInfo: FontInfo = null
  def complete() = IO {
    JSImageCanvas(inner)
  }
  def doFontAliasing(value: Boolean): IO[Unit] = IO(())
  def setInterpMode(mode: InterpMode): IO[Unit] = IO {
    mode match {
      case InterpMode.Linear =>
        context.imageSmoothingEnabled = true
      case InterpMode.Nearest =>
        context.imageSmoothingEnabled = false
    }
  }
  def setComposite(mode: CompositeMode): IO[Unit] = IO {
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
  def drawImage(image: Image, x: Int, y: Int): IO[Unit] = {
    val elem: IO[HTMLElement] = image.asInstanceOf[JSImage] match {
      case img: JSImageElement => img.promiseLoad().map(_.inner)
      case canvas: JSImageCanvas => IO(canvas.inner)
    }
    elem.map(it => context.drawImage(it, x, y))
  }
  def withTransform[T](transform: AffineTransform)(fn: => T): T = {
    context.setTransform(transform.m00, transform.m01, transform.m10, transform.m11, transform.m20, transform.m21)
    val res = fn
    context.setTransform(1, 0, 0, 1, 0, 0)
    res
  }
  def drawImage(image: Image, transform: AffineTransform): IO[Unit] = {
    for {
      elem: HTMLElement <- image.asInstanceOf[JSImage] match {
        case img: JSImageElement => img.promiseLoad().map(_.inner)
        case canvas: JSImageCanvas => IO(canvas.inner)
      }
      _ <- IO(withTransform(transform) { context.drawImage(elem, 0, 0) })
    } yield ()
  }
  def fillRect(x: Int, y: Int, width: Int, height: Int): IO[Unit] = IO {
    context.fillRect(x, y, width, height)
  }
  def setColor(color: Color): IO[Unit] = IO {
    val str = s"rgb(${color.r}, ${color.g}, ${color.b}, ${color.a.toDouble / 255})"
    context.fillStyle = str
  }
  def setFont(font: FontInfo): IO[Unit] = {
    val fontFace = FontFaceCls(font.name, s"url(resources/${font.path})")
    for {
      _ <- IO(this.curInfo = font)
      face <- IO.fromPromise(IO(fontFace.load())).flatMap(it => IO.fromPromise(IO(it.loaded)))
      _ <- IO(this.curFont = face)
      _ <- IO(document.asInstanceOf[js.Dynamic].fonts.add(face))
      css = s"${font.size}px ${font.name}"
      _ <- IO(context.font = css)
    } yield ()
  }
  def drawString(txt: String, x: Int, y: Int): IO[Unit] = IO {

    context.textBaseline = "top"
    // hack
    val t = AffineTransform.translation(x, y + (curInfo.size / 2))
    t += curInfo.transform 
    withTransform(t) {
      context.fillText(txt, 0, 0)
    }
  }
}

