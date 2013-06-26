package fr.enst.plnc2013.td1

object TD1 {

  def isOdd(number: Int): Boolean = { (number%2)==1 }
  def isEven(number: Int): Boolean = { !isOdd(number) }

  def myWhile(test: => Boolean, f: => Unit): Any = {
    if(test) {
      f
      myWhile(test,f)
    }
  }

 // def solveQueens(numberOfQueens: Int, f: List[(Int, Int)] => Unit): Unit
  

}

class ExtCond(test: => Boolean) {
  
   def doWhile[T](f: => T): Unit = {
     if(test) {
       f
       doWhile(f)
     }
   }
 }

 object ExtCond {

   implicit def toExtCond(test: Boolean): ExtCond = new ExtCond(test)

 }

case class Complex (réelle: Double, imaginaire: Double) {
  
  override def toString = {
    if(imaginaire == 0)
      réelle.toString
    else
      if(réelle == 0)
      imaginaire.toString + "i"
    else
      if(imaginaire > 0)
      réelle.toString + "+" +imaginaire.toString + "i"
    else
réelle.toString +imaginaire.toString + "i"

  }

}

case class ExtSeq[T] (mySeq: Seq[T]) {
  def any (f: T => Boolean): Boolean = {
    mySeq.map(f).contains(true) 
  }

  def all (f: T => Boolean): Boolean = {
    mySeq.forall(f)
  }

}

object ExtSeq {

  implicit def toExtSeq [T] (mySeq: Seq[T]): ExtSeq[T] = new ExtSeq(mySeq)

}

object Main extends App {

  import TD1._

}
