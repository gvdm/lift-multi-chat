package code.comet

import net.liftweb.actor.LiftActor
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js.jquery.JqJsCmds._
import net.liftweb.util._
import net.liftweb.util.Helpers._
import code.lib.currentM
import code.model.M
import scala.xml.NodeSeq
import org.joda.time.DateTime
import code.model.User

case class ChatMessage(message: String, from: User, timestamp: DateTime)
case class UserJoin(user: User)

class ChatServer(ls: M, n: String) extends LiftActor with ListenerManager {
  val M = ls
  val name = n
  
  var users : List[User] = Nil

  private var messages = List[ChatMessage]()
  
  def createUpdate = messages

  override def lowPriority = {
    case cm@ChatMessage(message, from, timestamp) => {
      messages = cm :: messages
      updateListeners()
    }
    
    case newUser@UserJoin(user) => {
      users = user :: users
      updateListeners()
    }
    
    case _ => println("Yikes")
  }
}

class ChatUser extends CometActor with CometListener with Logger {

  private var chatServer: ChatServer = getChatServer()
  
  def getChatServer() = currentM.get match {
    case Full(m) => {
      println("current m is "+m.id)
      ChatRoomManager.serveByM(m)
    }
    case _ => {
      println ("NO CURRENT M")
      throw new Exception("shouldn't try instantiating chat without a M")
    }
  }
  def registerWith = chatServer
  
  private var messages: List[ChatMessage] = Nil

  private val user = User.currentUser.getOrElse(throw new Exception("chat should only be served to logged in users"))
  println("current user is "+user.username.value)
  
  override def localSetup() = {
    super.localSetup()

    info("localSetup(%s)" format user.firstName)
    //sendMessage(NewUser(user))
  }

  override protected def localShutdown() {

    // remove from user lists if still there.
    //sendMessage(UserLeft(user))
    super.localShutdown

  }


  override def lowPriority = {
    case msg: ChatMessage ⇒ {
      messages = msg:: messages
      partialUpdate(AppendHtml("message-list",renderMessage(msg)))
    }
  }
  
  def renderMessage(msg: ChatMessage) = <li><span>{msg.timestamp.toString()}</span><span>{msg.from.username}</span><span>{msg.message}</span></li>

  private var message = ""
  
  def render = {
    "#messages *" #> <ul id="message-list"> {messages.map(renderMessage(_))} </ul> &
    "#message" #> SHtml.text(message, str => message = str) &
    "#send-message" #> SHtml.ajaxSubmit("Send", () => {
      if (message.nonEmpty) {
         chatServer! ChatMessage(message, user, DateTime.now)
      }
      SetValueAndFocus("message", "")
    })
  }
}

object ChatRoomManager {
  private var chatServers = List[ChatServer]()

  def serveByM(m: M): ChatServer = synchronized {
    def addNewChatRoom() = {
      val newServer = new ChatServer(m, StringHelpers.randomString(8))
      chatServers ::= newServer
      newServer
    }
    
    chatServers.filter(server => server.M == m && server.users.size < 20).headOption match {
      case Some(server) => server
      case _ => addNewChatRoom
    }
  }
}