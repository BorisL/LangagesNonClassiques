package models

import akka.actor._
import akka.util.Timeout
import akka.pattern.ask

import scala.concurrent.duration._

import play.api._
import play.api.libs.json._
import play.api.libs.iteratee._
import play.api.libs.concurrent._ // import for Akka
import play.api.Play.current
import play.api.libs.concurrent.Execution.Implicits._

import anorm._
import anorm.SqlParser._

import play.api.db._


object Robot {
  
  def apply(chatRoom: ActorRef) {
    
    // Create an Iteratee that logs all messages to the console.
    val loggerIteratee = Iteratee.foreach[JsValue](event => Logger("robot").info(event.toString))
    
    implicit val timeout = Timeout(1 second)
    // Make the robot join the room
    chatRoom ? (Join("Robot")) map {
      case Connected(robotChannel) => 
        // Apply this Enumerator on the logger.
        robotChannel |>> loggerIteratee
    }
    
    // Make the robot talk every 30 seconds
    Akka.system.scheduler.schedule(
      30 seconds,
      30 seconds,
      chatRoom,
      Talk("Robot", "I'm still alive")
    )
  }
  
}

object ChatRoom {

 implicit val timeout = Timeout(1 second)
  
  lazy val default = {
    var roomActor = Akka.system.actorOf(Props[ChatRoom])
    
    // Create a bot user (just for fun)
    Robot(roomActor)
    
    roomActor
  }

  def join(username:String):scala.concurrent.Future[(Iteratee[JsValue,_],Enumerator[JsValue])] = {

    (default ? Join(username)).map {
      
      case Connected(enumerator) => 
      
        // Create an Iteratee to consume the feed
        val iteratee = Iteratee.foreach[JsValue] { event =>
          default ! Talk(username, (event \ "text").as[String])
        }.mapDone { _ =>
          default ! Quit(username)
        }

        (iteratee,enumerator)
        
      case CannotConnect(error) => 
      
        // Connection error

        // A finished Iteratee sending EOF
        val iteratee = Done[JsValue,Unit]((),Input.EOF)

        // Send an error and close the socket
        val enumerator =  Enumerator[JsValue](JsObject(Seq("error" -> JsString(error)))).andThen(Enumerator.enumInput(Input.EOF))
        
        (iteratee,enumerator)
         
    }

  }

}

class ChatRoom extends Actor {

  var members = Set.empty[String];
  val (chatEnumerator, chatChannel) = Concurrent.broadcast[JsValue]
  var banwordlist = List.empty[BanWord];

  def receive = {

    case Join(username) => {
      if(members.contains(username)) {
        sender ! CannotConnect("This username is already used")
      } else {
        members = members + username
        sender ! Connected(chatEnumerator)
        self ! NotifyJoin(username)
      }
    }

    case NotifyJoin(username) => {
      notifyAll("join", username, "has entered the room")
    }
      
    case Talk(username, text) => {
      notifyAll("talk", username, text)
    }
      
    case Quit(username) => {
      members = members - username
      notifyAll("quit", username, "has left the room")
    }
      
  }
  
  def parseCommand(kind: String, user: String, myList: List[String], text: String):JsObject = myList match {
    case "/list" :: _ => JsObject(
      Seq(
        "kind" -> JsString("list"),
        "user" -> JsString(user),
        "to" -> JsString("All"),
        "message" -> JsString(
          members.toString),
        "members" -> JsArray(
          members.toList.map(JsString)
        )
      )
    )

    case "/quit" :: msg => JsObject(
      Seq(
        "kind" -> JsString("quit"),
        "user" -> JsString(user),
        "to" -> JsString("All"),
        "message" -> JsString(
          msg.mkString),
        "members" -> JsArray(
          members.toList.map(JsString)
        )
      )
    )

    case "/talk" :: to :: msg =>  JsObject(
      Seq(
        "kind" -> JsString("private"),
        "user" -> JsString(user),
        "to" -> JsString(to),
        "message" -> JsString(
          msg.mkString(" ")),
        "members" -> JsArray(
          members.toList.map(JsString)
        )
      )
    )

     
      case "/banword-add" :: badword :: goodword :: _ =>  {
        if(checkAuthorisation(user,"admin")) {
        BanWord.create(badword,goodword)
        JsObject(
          Seq(
            "kind" -> JsString("info"),
            "user" -> JsString("System"),
            "to" -> JsString("All"),
            "message" -> JsString(badword+" is now a very bad word ! He will be replaced by "+goodword),
            "members" -> JsArray(
              members.toList.map(JsString)
            )
          )
        )
        }
        else 
          notAdmin(user)
      }

    case "/banword-remove" :: badword :: _ =>  {
      if(checkAuthorisation(user,"admin")) {
      BanWord.delete(badword)
      JsObject(
      Seq(
        "kind" -> JsString("info"),
        "user" -> JsString("System"),
        "to" -> JsString("All"),
        "message" -> JsString(badword+" is no more a bad word !"),
        "members" -> JsArray(
          members.toList.map(JsString)
        )
      )
    )
      }
      else
        notAdmin(user)
    }
case "/banword-list" :: _ => {
      getBanWordList
      JsObject(
      Seq(
        "kind" -> JsString("info"),
        "user" -> JsString("System"),
        "to" -> JsString(user),
        "message" -> JsString(banwordlist.toString),
        "members" -> JsArray(
          members.toList.map(JsString)
        )
      )
    )
}
      case "/banuser" :: to :: msg =>  
      if(checkAuthorisation(user,"admin"))
        // user is admin
        JsObject(
          Seq(
            "kind" -> JsString("ban"),
            "user" -> JsString(user),
            "to" -> JsString(to),
            "message" -> JsString(
              to +" has been ban by " + user +" ("+msg.mkString(" ")+")"),
            "members" -> JsArray(
              members.toList.map(JsString)
            )
          )
        )
      else
        // user is not admin
        notAdmin(user)

    case _ :: _ => 
      {
        getBanWordList
        var goodtext = text
        for(banword <- banwordlist) {
          println(banword)
          goodtext = goodtext replaceAllLiterally(banword.badword,banword.goodword)
        }
        println(banwordlist)
        JsObject(
          Seq(
            "kind" -> JsString(kind),
            "user" -> JsString(user),
            "to" -> JsString("All"),
            "message" -> JsString(goodtext),
            "members" -> JsArray(
              members.toList.map(JsString)
            )
          )
        )
      }
  }

  def notifyAll(kind: String, user: String, text: String) {
    getBanWordList
    val xs = text.split(" ").toList 
    val msg = parseCommand(kind, user, xs, text)
    if(msg \ "kind" == "quit")
      sender ! Quit(user)
    chatChannel.push(msg)
  }

  def notAdmin(user: String):JsObject = {
    JsObject(
          Seq(
            "kind" -> JsString("private"),
            "user" -> JsString("system"),
            "to" -> JsString(user),
            "message" -> JsString(
              "You are not admin !"),
            "members" -> JsArray(
              members.toList.map(JsString)
            )
          )
        )
  }
  def checkAuthorisation(mypseudo: String, command: String):Boolean ={
    val user = {
      get[String] ("pseudo") ~
      get[String] ("droits") map {
        case pseudo~droits => User(pseudo,droits)
      }
    }
    def all(pseudo: String): List[User] = DB.withConnection {implicit c =>
      // implicit value used for the implicite connexion
    SQL("select * from superuser").as(user *)
          
    }

    // return True if the user is admin, else False
    val results = all(mypseudo)
    val data = results find  {e => e.pseudo == mypseudo}
    if(data != None)
      ((data get).droits == "admin" )
    else
      false
  }

  def getBanWordList {
    val banword = {
      get[String] ("badword") ~
      get[String] ("goodword") map {
        case badword~goodword => BanWord(badword,goodword)
      }
    }
  def all(): List[BanWord] = DB.withConnection {implicit c =>
    // implicit value used for the implicite connexion
    SQL("select * from wordsbanlist").as(banword *)
  }
    banwordlist = all()
  }

  def changeBanWord(word: String): String = {
    val data = banwordlist find  {e => e.badword == word}
    if(data != None)
      (data get).goodword
    else
      word
  }

}
case class Join(username: String)
case class Quit(username: String)
case class Talk(username: String, text: String)
case class NotifyJoin(username: String)

case class Connected(enumerator:Enumerator[JsValue])
case class CannotConnect(msg: String)
