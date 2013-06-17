import org.specs2.mutable._
import fr.enst.plnc2013._

class OptionSpec extends Specification {
  "The option" should {
    "be able to return an Int" in {
      Some(3).get must_== 3
    }

    "be able to return an Int or a String" in {
      Some(3).getOrElse("foobar") must_== 3
    }

    "be able to return the defaul" in {
      None.getOrElse("foobar") must_== "foobar"
    }

    "have no side effect" in {
      var x = false
      Some(42) getOrElse { x = true; 0 }
      x must_== false
    }

    
  }

  "isEmpty" should {
      "be true for None" in {
        None must be empty
      }
       "be false for Some" in {
         Some(42) must not be empty
       }
    }

}
