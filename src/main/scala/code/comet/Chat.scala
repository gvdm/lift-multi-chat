package code.comet

import net.liftweb.actor.LiftActor
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.http.SHtml._
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js.jquery.JqJsCmds._
import net.liftweb.util._
import net.liftweb.util.Helpers._
import code.model.M
import code.model.User
import scala.xml.NodeSeq
import org.joda.time.DateTime
import code.model.ChatRoom
import code.snippet.ChatBox
import code.snippet.UserSnippets
import org.joda.time.format.DateTimeFormatter
import org.joda.time.format.DateTimeFormat

case class NewChatRoom(cr: ChatRoom)
case class ChatMessage(message: String, from: User, timestamp: DateTime)
//case class UserJoin(user: User)

class ChatServer(cr: ChatRoom) extends LiftActor with ListenerManager with Loggable {
  val chatRoom = cr
  
  logger.info("created chat server for "+cr.id.value)
  
  var users : List[User] = Nil

  private var messages = List[ChatMessage]()
  
  def createUpdate = messages

  override def lowPriority = {
    case cm: ChatMessage => {
      messages ::= cm
      sendListenersMessage(cm)
    }
  }
}

class ChatUser extends CometActor with CometListener with Loggable {

  private var _chatServer: ChatServer = null
  
  def registerWith = _chatServer

  private var messages: List[ChatMessage] = Nil

  def user = User.currentUser

  override def localSetup() = {
    _chatServer = name match {
      case Full(id) ⇒ ChatRoomManager.serve(id)
      case Empty    ⇒ throw new Exception("chat user has no name for chat room")
    }
    super.localSetup()

    logger.info("localSetup of "+user.map(_.username.value) + " for "+htmlIdName)
  }

  override def lowPriority = {
    case msg: ChatMessage ⇒ {
      logger.info("got chat msg in "+htmlIdName+" from "+msg.from.username.value)
      messages ::= msg
      partialUpdate(AppendHtml(htmlIdName()+"-message-list", renderMessage(msg)))
    }
    
    case msgs: List[ChatMessage] ⇒ {
      messages = msgs
      partialUpdate(SetHtml(htmlIdName()+"-message-list", renderMessages()))
    }
  }
  
  def htmlIdName() = name.getOrElse("ERROR_CHATUSER_SHOULD_HAVE_NAME")
  
  def renderMessages() = messages.reverse.map(renderMessage(_))

  def renderMessage(msg: ChatMessage) =
    <li>
      <span>{ msg.timestamp.toString(DateTimeFormat.shortTime()) }</span>
      &nbsp;
      <span>{ msg.from.username }</span>
      &nbsp;
      <span float="right">{ msg.message }</span>
    </li>

  
  def render = {
    var message = ""
    
    ".messages *" #> <ul id={ htmlIdName() + "-message-list" }> { renderMessages() } </ul> &
    "@message" #> SHtml.text(message, str ⇒ message = str, "id" -> (htmlIdName() + "-message-input")) &
    "@send-message" #> SHtml.ajaxSubmit("Send", () ⇒ {
      if (message.nonEmpty) {
        _chatServer ! ChatMessage(message, user.getOrElse(throw new Exception("non logged in user sending message - what?")), DateTime.now)
      }
      SetValueAndFocus(htmlIdName() + "-message-input", "")
    })
  }
}

case class InChat(id: String)
class ChatControls extends CometActor with CometListener with Loggable {
  
  var openChats = scala.collection.mutable.Set[String]()
 
  def render = {
    var chatName = ""
    "#chat-list li *" #> ChatRoom.findAll.map(openChatButton(_)) &
    "#chat-name" #> text("", chatName = _) &
    "#make-chat-room" #> ajaxSubmit("make chat", () => {
      val chatId = StringHelpers.clean(chatName)
      val cr = ChatRoomManager.createChatRoom(chatId, chatName)
      openChats += chatId
      AppendHtml("open-chat-rooms", ChatBox(chatId)) &
      Focus(chatId + "-message-input")
    })
  }
  
  def newOpenChatButton(cr: ChatRoom) = AppendHtml("chat-list", <li class="chat-room">{openChatButton(cr)}</li>)
  
  def openChatButton(cr: ChatRoom) = ajaxButton("Open "+cr.name.value, () => {
    if (openChats.contains(cr.id.value)) {
      Focus(cr.id.value + "-message-input")
    } else {
      openChats += cr.id.value
      AppendHtml("open-chat-rooms", ChatBox(cr.id.value))
    }
  })
  
  def registerWith = ChatRoomManager
  
  override def lowPriority = {
    case m: scala.collection.mutable.Map[String, ChatServer] => logger.info("got first update")
    
    case NewChatRoom(cr) => partialUpdate(newOpenChatButton(cr))
    // should only be sent when first inited to make sure that openChats is set correctly
    case InChat(id) => {
      logger.info("adding "+id+" as our initial chat")
      openChats += id
    }
  }
  
}

object ChatRoomManager extends LiftActor with ListenerManager with Logger {
  private var chatServers = scala.collection.mutable.Map[String, ChatServer](ChatRoom.findAll.map(cr => cr.id.value -> new ChatServer(cr)): _*)
  
  def createUpdate = chatServers
  
  def createChatRoom(id: String, name: String) = synchronized {
      info("added new ChatServer for "+id)
      ChatRoom.find(id) match {
        case Empty => {
          val cr = ChatRoom.createRecord.id(id).name(name)
          User.currentUser match {
            case Full(u) => cr.owner(u.id.value)
          }
          
          sendListenersMessage(NewChatRoom(cr))
          cr.save(true)
        }
        case Full(cr) => cr
      }
    }

  def serve(id: String): ChatServer = synchronized {
    chatServers.get(id) match {
      case Some(server) => server
      case None ⇒ {
        val cr = ChatRoom.find(id) match {
          case Full(cr) ⇒ cr
          case Empty    ⇒ createChatRoom(id, id)
        }
        val newServer = new ChatServer(cr)
        chatServers.put(id, newServer)
        newServer
      }
    }
  }
}
