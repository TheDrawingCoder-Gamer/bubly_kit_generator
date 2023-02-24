package net.bulbyvr.splooge.core.util

import org.scalajs.dom.HTMLCanvasElement
import org.scalajs.dom.HTMLImageElement
import org.scalajs.dom.document

trait ImageFactory {
  def apply(w: Int, h: Int): Image = {
    val elem = document.createElement("canvas").asInstanceOf[HTMLCanvasElement]
    JSImageCanvas(elem)
  }
  def loadFromResource(str: String): Image = {
    val elem = document.createElement("img").asInstanceOf[HTMLImageElement]
    elem.src = str
    JSImageElement(elem)
  }
}
