package net.bulbyvr.splooge.core.util

import javax.imageio.ImageIO
import java.awt.image.BufferedImage
trait ImageFactory {
  def apply(w: Int, h: Int): Image = {
    new JVMImage(new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB))

  }
  def loadFromResource(resource: String) = {
    new JVMImage(ImageIO.read(this.getClass.getResourceAsStream("/" + resource)))
  }
}
