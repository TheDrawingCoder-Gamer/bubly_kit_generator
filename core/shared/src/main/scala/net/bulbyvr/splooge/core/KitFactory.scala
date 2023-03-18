package net.bulbyvr.splooge.core

import net.bulbyvr.splooge.core.util.*
import cats.effect.IO
import cats.implicits.*
trait KitFactory {
  extension (pos: (Int, Int)) {
    inline def x: Int = pos._1
    inline def y: Int = pos._2
  }
  protected def kit : IO[Image]
  protected def canvasSize : (Int, Int)
  protected def subSize : Int 
  protected def specialSize : Int 
  protected def kitWidth : Int 
  protected def kitPos : (Int, Int)
  protected def mainSize : Int 
  protected def mainPos : (Int, Int)
  protected def subPos : (Int, Int) 
  protected def specialPos : (Int, Int)
  protected def weaponFont : FontInfo
  protected def subFont : FontInfo
  protected def specialFont : FontInfo
  protected def spPointsFont : FontInfo
  protected def weaponTextPos : (Int, Int)
  protected def subTextPos : (Int, Int)
  protected def specialTextPos : (Int, Int)
  protected def spPointsTextPos : (Int, Int)
  protected def renderSubShadow : Boolean = false
  protected def renderSpecialShadow : Boolean = false 
  protected def artistPos: (Int, Int)
  protected def artistFont: FontInfo
  private def makeShadow(loadImg: Image): IO[Image] = {
    val img = Image(loadImg.width, loadImg.height)
    val canvas = img.getCanvas()
    for {
      _ <- canvas.drawImage(loadImg, 0, 0)
      _ <- canvas.setComposite(CompositeMode.SrcIn)
      _ <- canvas.setColor(Color(0, 0, 0, 255))
      _ <- canvas.fillRect(0, 0, loadImg.width, loadImg.height)
      _ <- canvas.setColor(Color(0, 0, 0, 100))
      _ <- canvas.fillRect(0, 0, loadImg.width, loadImg.height)
      i <- canvas.complete()
    } yield i
  }
  def renderKit(mainName: String, mainImage: Image, subName: String, subImage: Image,
    specialName: String, specialImage: Image, specialPoints: Option[String], brand: Option[Image], kitMaker: Option[String] = None): IO[Image] = {
    val (canvasW, canvasH) = canvasSize
    val img = Image(canvasW, canvasH)
    val canvas = img.getCanvas()
    val (mainX, mainY) = mainPos
    val (subX, subY) = subPos
    val (specialX, specialY) = specialPos
    val widthPng = mainSize
    val brandSize = widthPng / 2d
    val brandOffset = brandSize 
    val brandX = mainX + brandOffset 
    val brandY = mainY + brandOffset

    val (kitX, kitY) = kitPos

    val widthSubPng = subSize 
    val widthSpecialPng = specialSize
    val subScaling = widthSubPng.toDouble / subImage.width
    val specialScaling = widthSpecialPng.toDouble / specialImage.width

    val (mainTxtX, mainTxtY) = weaponTextPos

    val (subFontX, subFontY) = subTextPos

    val (spFontX, spFontY) = specialTextPos
      
    val (spX, spY) = spPointsTextPos

    for {
      shadow <- makeShadow(mainImage)
      kit <- this.kit

      kitScaling = kitWidth.toDouble / kit.width
      mainTransform = AffineTransform.translation(mainX, mainY)
      shadowTransform = AffineTransform.scaling(widthPng.toDouble / shadow.width, (widthPng * 0.75) / shadow.width)
      kitTransform = AffineTransform.translation(kitX, kitY)
      subTransform = AffineTransform.translation(subX, subY)
      specialTransform = AffineTransform.translation(specialX, specialY)
      _ <- IO {
        mainTransform.scale(widthPng.toDouble / mainImage.width, widthPng.toDouble / mainImage.width)
        shadowTransform.prepend(AffineTransform.translation(mainX, mainY + (widthPng.toDouble / 4)))
        kitTransform.scale(kitScaling, kitScaling)
        subTransform.scale(subScaling, subScaling)
        specialTransform.scale(specialScaling, specialScaling)
      }
      // hangs some point after this point
      _ <- canvas.setInterpMode(InterpMode.Linear)
      _ <- canvas.drawImage(kit, kitTransform)
      _ <- canvas.drawImage(shadow, shadowTransform)
      _ <- canvas.drawImage(mainImage, mainTransform)
      _ <- brand.traverse { b => 
        val brandTransform = AffineTransform.translation(brandX, brandY)

        brandTransform.scale(brandSize.toDouble / b.width, brandSize.toDouble / b.width)
        canvas.drawImage(b, brandTransform)
      }
      _ <- 
        if (renderSubShadow) {
          for {
            subShadow <- makeShadow(subImage)
            shadowTransform = subTransform.copy()
            _ = {
              shadowTransform.prepend(AffineTransform.translation(0, widthSubPng.toDouble / 4))
              shadowTransform.scale(1, 0.75)
            }
            _ <- canvas.drawImage(subShadow, shadowTransform)
          } yield ()
        } else IO(())
      _ <- 
        if (renderSpecialShadow) {
          for {
            spShadow <- makeShadow(specialImage)
            shadowTransform = specialTransform.copy()
            _ = {
              shadowTransform.prepend(AffineTransform.translation(0, widthSpecialPng.toDouble / 4))
              shadowTransform.scale(1, 0.75)
            }
            _ <- canvas.drawImage(spShadow, shadowTransform)
          } yield ()
        } else IO(())
      _ <- canvas.drawImage(subImage, subTransform)
      _ <- canvas.drawImage(specialImage, specialTransform)
      _ <- canvas.doFontAliasing(true)
      _ <- canvas.setFont(weaponFont)
      _ <- canvas.setColor(Color(255, 255, 255, 255))
      _ <- canvas.drawString(mainName, mainTxtX, mainTxtY)
      _ <- canvas.setFont(subFont)
      _ <- canvas.drawString(subName, subFontX, subFontY)
      _ <- canvas.setFont(specialFont)
      _ <- canvas.drawString(specialName, spFontX, spFontY)
      _ <- specialPoints match {
        case Some(p) =>
          for {
            _ <- canvas.setFont(spPointsFont)
            _ <- canvas.drawString(p + "p", spX, spY)
          } yield ()
        case None => IO(())
      }
      _ <- kitMaker.traverse_ { artist =>
        for {
          _ <- canvas.setFont(artistFont)
          _ <- canvas.drawString("Kit made by: " + artist, artistPos.x, artistPos.y)
        } yield ()
      }
      complete <- canvas.complete()
    } yield complete






  }
} 
object Splooge3KitGen extends KitFactory {
  // TODO: is holding an image in memory really worth it?
  override def kit = Image.loadFromResource("ui/s3_kit_backdrop.png")
  override val kitWidth = 676
  override val canvasSize = (686, 507)
  private val subSpSize = 62 
  override val mainSize = 170
  override val mainPos = (50, 135)
  override val subSize = subSpSize 
  override val specialSize = subSpSize
  private val subSpX = 268
  private val subY = 148 
  private val specialY = 235
  override val subPos = (subSpX, subY)
  override val specialPos = (subSpX, specialY)
  override val kitPos = (0, 0)
  private val fontTransform = AffineTransform.rotation(Math.toRadians(1))
  private lazy val splooge1Font = FontInfo("Splatoon1", "font/splatoon1.otf", 0)
  private lazy val splooge2Font = FontInfo("Splatoon2", "font/splatoon2.otf", 0)
  private lazy val subSpFont = splooge2Font.copy(size = 25)
  override lazy val weaponFont = splooge1Font.copy(size = 32, transform = fontTransform)
  override lazy val subFont = subSpFont 
  override lazy val specialFont = subSpFont 
  override lazy val spPointsFont = splooge2Font.copy(size = 32)
  override val weaponTextPos = (52, 25)
  private val subSpFontX = subSpX + 75
  override val subTextPos = (subSpFontX, subY)
  override val specialTextPos = (subSpFontX, specialY)
  override val spPointsTextPos = (475, 320)
  override val artistPos = (52, 420)
  override val artistFont = splooge2Font.copy(size = 25)
}
object Splooge2KitGen extends KitFactory {
  // TODO: is holding an image in memory really worth it?
  override def kit = Image.loadFromResource("ui/s2_kit_backdrop.png")
  override val kitWidth = 676
  override val canvasSize = (686, 600)
  private val subSpSize = 100 
  override val mainSize = 150
  override val mainPos = (80, 260)
  override val subSize = subSpSize 
  override val specialSize = subSpSize
  private val subSpX = 280
  private val subY = 215
  private val specialY = 355
  override val subPos = (subSpX, subY)
  override val specialPos = (subSpX, specialY)
  override val kitPos = (0, 0)
  private lazy val splooge2Font = FontInfo("Splatoon2", "font/splatoon2.otf", 0)
  private lazy val subSpFont = splooge2Font.copy(size = 28)
  override lazy val weaponFont = splooge2Font.copy(size = 35)
  override lazy val subFont = subSpFont 
  override lazy val specialFont = subSpFont 
  override lazy val spPointsFont = splooge2Font.copy(size = 32)
  override val weaponTextPos = (70, 120)
  private val subSpFontX = subSpX + 150 
  private val textOffset = 50
  override val subTextPos = (subSpFontX, subY + textOffset)
  override val specialTextPos = (subSpFontX, specialY + textOffset)
  override val spPointsTextPos = (485, 485)
  override val artistPos = (52, 550)
  override val artistFont = splooge2Font.copy(size = 25)
}
object Splooge1KitGen extends KitFactory {
  // TODO: is holding an image in memory really worth it?
  override def kit = Image.loadFromResource("ui/s_kit_backdrop.png")
  override val kitWidth = 676
  override val canvasSize = (686, 507)
  private val subSpSize = 64 
  override val mainSize = 125
  override val mainPos = (80, 120)
  override val subSize = subSpSize 
  override val specialSize = subSpSize
  private val subX = 240 
  private val specialX = 105
  private val subY = 185
  private val specialY = 275
  override val subPos = (subX, subY)
  override val specialPos = (specialX, specialY)
  override val kitPos = (0, 0)
  private lazy val splooge1Font = FontInfo("Splatoon1", "font/splatoon1.otf", 0)
  private lazy val michromaFont = FontInfo("Michroma", "font/michroma-regular.ttf", 0)
  private lazy val subSpFont = michromaFont.copy(size = 20)
  private val fontTransform = AffineTransform.rotation(Math.toRadians(4))
  // me omw to derive ur mom
  override lazy val weaponFont = splooge1Font.copy(size = 32, transform = fontTransform)
  override lazy val subFont = subSpFont 
  override lazy val specialFont = subSpFont 
  override lazy val spPointsFont = michromaFont.copy(size = 14)
  override val weaponTextPos = (52, 35)
  private val subFontX = subX + 90 
  private val specialFontX = specialX + 120 
  private val textYOff = 25 
  override val subTextPos = (subFontX, subY + textYOff + 5)
  override val specialTextPos = (specialFontX, specialY + textYOff)
  override val spPointsTextPos = (410, 367)
  override val renderSubShadow = true 
  override val renderSpecialShadow = true
  override val artistPos = (52, 450)
  override val artistFont = splooge1Font.copy(size = 25)
}
