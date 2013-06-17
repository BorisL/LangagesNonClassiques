package controllers

import play.api._
import play.api.mvc._

import play.api.libs.json._

import models._

import akka.actor._
import scala.concurrent.duration._

object Application extends Controller {

  /**
    *  Display the home page
    **/
  def index = Action { implicit request =>
     Ok(views.html.index())
  }

  /**
    * Display the IRC room page
    **/
  def chatRoom(username: Option[String]) = Action { implicit request =>
    username.filterNot(_.isEmpty).map {username =>
      Ok(views.html.chatRoom(username))
    }.getOrElse {
      Redirect(routes.Application.index).flashing(
        "error" -> "Please choose a valid username."
      )
    }
  }

  /**
   * Handles the chat websocket.
   */
  def chat(username: String) = WebSocket.async[JsValue] { request  =>
    println("Hello");
    ChatRoom.join(username)
    
  }
  

}
