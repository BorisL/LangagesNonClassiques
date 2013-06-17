package fr.enst.plnc2013

class Rational(n: Int, d: Int) {

  import Rational._

  require( d != 0)

  private[this] val g = gcd(n, d)

  val numerator = n / g
  val denominator = d / g

  override lazy val toString = 
    if (denominator == 1)
      numerator.toString
    else
      numerator + "/" + denominator

  def +(other: Rational) = new Rational (numerator * other.denominator
      + other.numerator * denominator , 
    denominator * other.denominator
  )

  // def *(other: Rational) = new Rational (
}

object Rational {
  def gcd(a: Int, b: Int): Int =
    if(b == 0)
      a
    else
      gcd(b, a % b)

  def apply(n: Int) = new Rational(n,1)
  def apply(n: Int, d: Int) = new Rational(n,d)

  implicit def toRational(n: Int) = new Rational(n, 1)
  implicit def toDouble(r: Rational): Double = (r.numerator : Double) / r.denominator
}
