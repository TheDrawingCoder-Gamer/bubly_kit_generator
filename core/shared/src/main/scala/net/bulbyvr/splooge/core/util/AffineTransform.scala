package net.bulbyvr.splooge.core.util

class AffineTransform(var m00: Double, var m10: Double, var m20: Double, var m01: Double, var m11: Double, var m21: Double) {
  def translate(tx: Double, ty: Double) = {
    this += AffineTransform.translation(tx, ty)
  }
  def scale(sx: Double, sy: Double) = {
    this += AffineTransform.scaling(sx, sy) 
  }
  def rotate(theta: Double) = {
    this += AffineTransform.rotation(theta)
  }
  def *(that: AffineTransform) = {
    val t00 = that.m00
    val t10 = that.m10
    val t20 = that.m20
    val t01 = that.m01
    val t11 = that.m11
    val t21 = that.m21
    val mX0 = MathVector(m00, m10, m20)
    val mX1 = MathVector(m01, m11, m21)
    val xX2 = MathVector(0, 0, 1)
    val t0X = MathVector(t00, t01, 0)
    val t1X = MathVector(t10, t11, 0)
    val t2X = MathVector(t20, t21, 1)
    val r00 = mX0.dot(t0X).get
    val r10 = mX0.dot(t1X).get
    val r20 = mX0.dot(t2X).get
    val r01 = mX1.dot(t0X).get
    val r11 = mX1.dot(t1X).get
    val r21 = mX1.dot(t2X).get
    // Implied last row of 0 0 1
    AffineTransform(
        r00, r10, r20,
        r01, r11, r21
      )
  }
  def +=(that: AffineTransform) = {
    val res = this * that
    this.m00 = res.m00
    this.m10 = res.m10
    this.m20 = res.m20
    this.m01 = res.m01
    this.m11 = res.m11
    this.m21 = res.m21
  }
  def prepend(that: AffineTransform) = {
    val res = that * this
    this.m00 = res.m00
    this.m10 = res.m10
    this.m20 = res.m20
    this.m01 = res.m01
    this.m11 = res.m11
    this.m21 = res.m21
  }
  def copy(): AffineTransform = {
    AffineTransform(m00, m10, m20, m01, m11, m21)
  }
}
object AffineTransform {
  def identity: AffineTransform = 
    AffineTransform(
      1, 0, 0,
      0, 1, 0
      )
  def translation(tx: Double, ty: Double) =
    AffineTransform(
      1, 0, tx,
      0, 1, ty
      )
  def scaling(sx: Double, sy: Double) = 
    AffineTransform(
      sx, 0, 0,
      0, sy, 0
      )
  /// clockwise rotation
  def rotation(theta: Double) = {
    val cos = Math.cos(theta)
    val sin = Math.sin(theta)
    AffineTransform(
      cos, sin, 0,
      -sin, cos, 0
      )
  }
}
case class MathVector(items: Vector[Double]) {
  def dot(that: MathVector): Option[Double] = {
    if (this.items.length != that.items.length) {
      None 
    } else {
      Some(this.items.zip(that.items).map(_ * _).sum)
    }
  }
}

object MathVector {
  def apply(items: Double*) = {
    new MathVector(items.toVector)
  }
}

case class Point2D(x: Double, y: Double) {
  def transformed(transform: AffineTransform): Point2D = {
    val vector = MathVector(x, y, 1)
    val mX0 = MathVector(transform.m00, transform.m10, transform.m20)
    val mX1 = MathVector(transform.m01, transform.m11, transform.m21)
    // val mX2 = MathVector(0, 0, 1)
    val r0 = mX0.dot(vector).get
    val r1 = mX1.dot(vector).get
    // no need todo last row?

    Point2D(r0, r1)
    
  }
}


