package net.bulbyvr.splooge.core.util

trait SFont {
  def deriveFont(size: Int): SFont
  def deriveFont(transform: AffineTransform): SFont
}
