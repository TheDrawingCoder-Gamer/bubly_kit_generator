package net.bulbyvr.splooge.core.util

import javax.imageio.ImageIO
import java.awt.image.BufferedImage
import cats.effect.IO
trait ImageFactory {
  def apply(w: Int, h: Int): Image = {
    new JVMImage(new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB))

  }
  def loadFromResource(resource: String) = IO.blocking {
    new JVMImage(ImageIO.read(this.getClass.getResourceAsStream("/" + resource)))
  }
}