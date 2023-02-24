package net.bulbyvr.splooge.core

import net.bulbyvr.splooge.core.util.*
trait KitFactory {
  protected def kit : Image
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
  private def makeShadow(loadImg: Image): Image = {
    val img = Image(loadImg.width, loadImg.height)
    val canvas = img.getCanvas()
    canvas.drawImage(loadImg, 0, 0)
    canvas.setComposite(CompositeMode.SrcIn)
    canvas.setColor(Color(0, 0, 0, 255))
    canvas.fillRect(0, 0, loadImg.width, loadImg.height)
    canvas.setColor(Color(0, 0, 0, 100))
    canvas.fillRect(0, 0, loadImg.width, loadImg.height)
    canvas.complete()
  }
  def renderKit(mainName: String, mainImage: Image, subName: String, subImage: Image,
    specialName: String, specialImage: Image, specialPoints: Option[String], brand: Option[Image]): Image = {
    val (canvasW, canvasH) = canvasSize
    val img = Image(canvasW, canvasH)
    val canvas = img.getCanvas()
    val kit = this.kit
    val (mainX, mainY) = mainPos
    val (subX, subY) = subPos
    val (specialX, specialY) = specialPos
    val shadow = makeShadow(mainImage)
    val widthPng = mainSize
    val brandSize = widthPng / 2d
    val brandOffset = brandSize 
    val brandX = mainX + brandOffset 
    val brandY = mainY + brandOffset

    val widthSubPng = subSize 
    val widthSpecialPng = specialSize

    val mainTransform = AffineTransform.translation(mainX, mainY)
    mainTransform.scale(widthPng.toDouble / mainImage.width, widthPng.toDouble / mainImage.width)

    val transform = AffineTransform.scaling(widthPng.toDouble / shadow.width, (widthPng * 0.75) / shadow.width)
    transform.prepend(AffineTransform.translation(mainX, mainY + (widthPng.toDouble / 4)))
    val kitScaling = kitWidth.toDouble / kit.width
    val (kitX, kitY) = kitPos

    val kitTransform = AffineTransform.translation(kitX, kitY)
    kitTransform.scale(kitScaling, kitScaling)

    val subTransform = AffineTransform.translation(subX, subY)
    val subScaling = widthSubPng.toDouble / subImage.width
    subTransform.scale(subScaling, subScaling)

    val specialTransform = AffineTransform.translation(specialX, specialY)
    val specialScaling = widthSpecialPng.toDouble / specialImage.width
    specialTransform.scale(specialScaling, specialScaling)
    canvas.setInterpMode(InterpMode.Linear)
    canvas.drawImage(kit, kitTransform)
    canvas.drawImage(shadow, transform)
    canvas.drawImage(mainImage, mainTransform)
    brand match {
      case None => ()
      case Some(b) => {
        val brandTransform = AffineTransform.translation(brandX, brandY)

        brandTransform.scale(brandSize.toDouble / b.width, brandSize.toDouble / b.width)

        canvas.drawImage(b, brandTransform)
      }
    }
    if (renderSubShadow) {
      val subShadow = makeShadow(subImage)
      val shadowTransform = subTransform.copy()
      shadowTransform.prepend(AffineTransform.translation(0, widthSubPng.toDouble / 4))
      shadowTransform.scale(1, 0.75)
      canvas.drawImage(subShadow, shadowTransform)
    }
    if (renderSpecialShadow) {
      val spShadow = makeShadow(specialImage)
      val shadowTransform = specialTransform.copy()

      shadowTransform.prepend(AffineTransform.translation(0, widthSubPng.toDouble / 4))
      shadowTransform.scale(1, 0.75)

      canvas.drawImage(spShadow, shadowTransform)
    }
    canvas.drawImage(subImage, subTransform)
    canvas.drawImage(specialImage, specialTransform)
    canvas.doFontAliasing(true)
    canvas.setFont(weaponFont)
    canvas.setColor(Color(255, 255, 255, 255))
    val (mainTxtX, mainTxtY) = weaponTextPos
    canvas.drawString(mainName, mainTxtX, mainTxtY)

    canvas.setFont(subFont)
    val (subFontX, subFontY) = subTextPos
    canvas.drawString(subName, subFontX, subFontY)

    canvas.setFont(specialFont)
    val (spFontX, spFontY) = specialTextPos
    canvas.drawString(specialName, spFontX, spFontY)

    specialPoints match {
      case Some(p) => 
        canvas.setFont(spPointsFont)
        val (spX, spY) = spPointsTextPos
        canvas.drawString(p + "p", spX, spY)
      case None => ()
    }
    canvas.complete()
  }
} 
object Splooge3KitGen extends KitFactory {
  // TODO: is holding an image in memory really worth it?
  override lazy val kit = Image.loadFromResource("ui/s3_kit_backdrop.png")
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
  private lazy val splooge1Font = FontInfo("font/splatoon1.otf", 0)
  private lazy val splooge2Font = FontInfo("font/splatoon2.otf", 0)
  private lazy val subSpFont = splooge2Font.copy(size = 25)
  override lazy val weaponFont = splooge1Font.copy(size = 32)
  override lazy val subFont = subSpFont 
  override lazy val specialFont = subSpFont 
  override lazy val spPointsFont = splooge2Font.copy(size = 32)
  override val weaponTextPos = (52, 25)
  private val subSpFontX = subSpX + 75
  override val subTextPos = (subSpFontX, subY)
  override val specialTextPos = (subSpFontX, specialY)
  override val spPointsTextPos = (475, 320)
}
object Splooge2KitGen extends KitFactory {
  // TODO: is holding an image in memory really worth it?
  override lazy val kit = Image.loadFromResource("ui/s2_kit_backdrop.png")
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
  private lazy val splooge2Font = FontInfo("font/splatoon2.otf", 0)
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
}
object Splooge1KitGen extends KitFactory {
  // TODO: is holding an image in memory really worth it?
  override lazy val kit = Image.loadFromResource("ui/s_kit_backdrop.png")
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
  private lazy val splooge1Font = FontInfo("font/splatoon1.otf", 0)
  private lazy val michromaFont = FontInfo("font/michroma-regular.ttf", 0)
  private lazy val subSpFont = michromaFont.copy(size = 20)
  private val fontTransform = AffineTransform.rotation(Math.toRadians(-5))
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
}
