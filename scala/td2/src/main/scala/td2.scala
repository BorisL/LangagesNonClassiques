package fr.enst.plnc2013

trait Answers { this: Human =>
  def answer(question: String): String
  def answerNot(question: String): String =
    "not " ++ answer(question) ++ " (said " ++ name ++ ")"
}

trait Walks { this: Human =>
  def walk: Unit = {println("I'm walking") }
}

class Human {
  val name = "John Doe"
}

class Normand extends Human with Answers with Walks {
  def answer(question: String) = "Maybe"
}

class Picard extends Normand with Answers {

}

class Bibliothéquaire extends Human with Answers {
  def answer(question: String) = "Oook"
  override def answerNot(question: String) ="Oook"
}
