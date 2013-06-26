package models

import play.api.db._
import play.api.Play.current
import anorm._
import anorm.SqlParser._
case class BanWord (badword: String, goodword: String)

object BanWord {
  def all(): List[BanWord] = Nil
  def create (badword: String, goodword: String) {
    DB.withConnection {implicit c =>
    // implicit value used for the implicite connexion
      SQL("insert into wordsbanlist (badword, goodword) values ({badword},{goodword})").on(
        'badword -> badword,
        'goodword -> goodword
    ).executeUpdate()
    }
  }
  def delete(badword: String) {
    DB.withConnection {implicit c =>
      // implicit value used for the implicite connexion
      SQL("delete from wordsbanlist where badword = {badword}").on(
      'badword -> badword).executeUpdate()
    }
  }
}
