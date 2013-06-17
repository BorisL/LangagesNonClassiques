import fr.enst.plnc2013._
import org.scalacheck._
import Prop._

object RationalTest extends Properties ("Rational") {
  val smallInt = Gen.choose(-1000, 1000)

  import Rational._

  property("gcd") = {
    forAll(smallInt, smallInt) { (a,b) =>
      (a > 0 && b > 0) ==> {
        val g = gcd(a,b)
        (g >= 1) :| "gcd greather than 0"
        (g <= a) :| "gcd smaller or equal to a"
      }
    }
/*
    property("simplified") = {
      forAll(smallInt, smallInt) { (a, b) =>
        b != 0 
    }
 */
  }
}

