package fr.enst.plnc2013

// ~compile pour compilation continue
// +A : indique Option[B] derive de Option[A] si B derive de A c'est 
//      de la covariance
// sealed : interdit de créer de nouvelles dérivés de Option hors de l'unité de compilation

sealed abstract class Option[+A] {
  def isEmpty: Boolean

  def get: A
  def getOrElse[ B >: A] (default: => B): B =
    if (isEmpty)
      default
    else 
      get
}

/*
class Some[+A](override val get: A)  extends Option[A] {
 */
class Some[+A](x: A)  extends Option[A] {
  override def isEmpty = false

  val get = x

  override lazy val toString = "Some(" + x + ")"
  // val crée à l'instanciation
  // def crée à chaque execution
  // lazy val à la première execution de l'instance
}

object Some {
/*
  def apply(s: String) {
    println("Hello " ++ s)
  }
 */

 def apply[A](x: A) = new Some(x)

}

case class Some2[+A](val get: A) extends Option[A] {
  val isEmpty = false
}

case object None extends Option[Nothing] {
  val isEmpty = true
  def get = error("no get for None")
}

