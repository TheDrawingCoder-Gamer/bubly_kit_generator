package net.bulbyvr.splooge.core.util

import java.awt.image.BufferedImage
import java.awt.Graphics2D
import java.awt.Color as JColor
import java.awt.AlphaComposite
import java.awt.geom.AffineTransform as JAffine
import java.awt.Font
import java.awt.RenderingHints
import cats.effect.*
class JVMImage(val inner: BufferedImage) extends Image {
  override def width:  Int = inner.getWidth()
  override def height: Int = inner.getHeight()
  override def getCanvas(): Canvas = JVMCanvas(inner, inner.getGraphics().asInstanceOf[Graphics2D])
}



class JVMCanvas(val img: BufferedImage, val inner: Graphics2D) extends Canvas {
  def fillRect(x: Int, y: Int, width: Int, height: Int): IO[Unit] = IO {
    inner.fillRect(x, y, width, height) 
  }
  def drawImage(image: Image, x: Int, y: Int): IO[Unit] = IO {
    val img = image.asInstanceOf[JVMImage]
    inner.drawImage(img.inner, null, x, y)
  }
  private def transformToTransform(transform: AffineTransform): JAffine = 
    JAffine(transform.m00, transform.m01, transform.m10, transform.m11, transform.m20, transform.m21)
  def drawImage(image: Image, transform: AffineTransform): IO[Unit] = IO {
    val img = image.asInstanceOf[JVMImage]
    val goodTransform = transformToTransform(transform) 
    inner.drawImage(img.inner, goodTransform, null)
    ()
  }
  def setColor(color: Color): IO[Unit] = IO {
    inner.setColor(new JColor(color.r,  color.g, color.b, color.a))
  }
  def setComposite(mode: CompositeMode): IO[Unit] = IO {
    val composite = mode match {
      case CompositeMode.SrcOver => AlphaComposite.SrcOver
      case CompositeMode.SrcIn => AlphaComposite.SrcIn
      case CompositeMode.SrcOut => AlphaComposite.SrcOut
      case CompositeMode.SrcAtop => AlphaComposite.SrcAtop
      case CompositeMode.DestOver => AlphaComposite.DstOver
      case CompositeMode.DestIn => AlphaComposite.DstIn
      case CompositeMode.DestOut => AlphaComposite.DstOut
      case CompositeMode.DestAtop => AlphaComposite.DstAtop
    }
    inner.setComposite(composite)
  }
  def setFont(info: FontInfo): IO[Unit] = IO {
    val goodTransform = transformToTransform(info.transform)
    val font = Font.createFont(Font.TRUETYPE_FONT, this.getClass.getResourceAsStream("/" + info.path)).deriveFont(Font.PLAIN, info.size).deriveFont(goodTransform)
    inner.setFont(font)
  }
  def drawString(txt: String, x: Int, y: Int) = IO {
    val ascent = inner.getFontMetrics().getAscent()
    inner.drawString(txt, x, y + ascent)
  }
  def doFontAliasing(value: Boolean): IO[Unit] = IO {
    val res = 
      if (value) 
        RenderingHints.VALUE_TEXT_ANTIALIAS_ON
      else
        RenderingHints.VALUE_TEXT_ANTIALIAS_OFF
    inner.setRenderingHint(
      RenderingHints.KEY_TEXT_ANTIALIASING,
      res
      )
  }
  def setInterpMode(mode: InterpMode): IO[Unit] = IO {
    val interp = mode match {
      case InterpMode.Linear => RenderingHints.VALUE_INTERPOLATION_BILINEAR
      case InterpMode.Nearest => RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR
    }
    inner.setRenderingHint(
      RenderingHints.KEY_INTERPOLATION,
      interp
      )
  } 
  def complete(): IO[Image] = IO {
    inner.dispose()
    JVMImage(img)
  }
}
