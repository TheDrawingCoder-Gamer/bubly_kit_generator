package net.bulbyvr.splooge.core.util

import org.scalajs.dom.HTMLCanvasElement
import org.scalajs.dom.HTMLImageElement
import org.scalajs.dom.document
import cats.effect.IO

trait ImageFactory {
  def apply(w: Int, h: Int): Image = {
    val elem = document.createElement("canvas").asInstanceOf[HTMLCanvasElement]
    elem.width = w
    elem.height = h
    JSImageCanvas(elem)
  }
  def loadFromResource(str: String): IO[Image] = {
    val elem = document.createElement("img").asInstanceOf[HTMLImageElement]
    elem.src = "resources/" + str
    JSImageElement(elem).promiseLoad()
  }
  def fromDataURL(str: String): IO[Image] = IO {
    val elem = document.createElement("img").asInstanceOf[HTMLImageElement]
    elem.src = str
    JSImageElement(elem)
  }
}
