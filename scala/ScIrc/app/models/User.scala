package models

case class User (pseudo: String, droits: String)

object User {
  def all(): List[User] = Nil
  def create (pseudo: String, droits: String) {}
  def delete(pseudo: String) {}
}
