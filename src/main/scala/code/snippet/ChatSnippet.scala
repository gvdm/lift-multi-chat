package code.snippet

import net.liftweb.common._
import net.liftweb.util.Helpers._
import net.liftweb.http._
import net.liftweb.http.SHtml._
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js.jquery.JqJsCmds._
import code.model.User
import net.liftweb.util.StringHelpers
import code.model.M
  
class ChatSnippet(m: M) extends Loggable {
  
  def render = "div" #> idMemoize { template ⇒ User.currentUser match {
      case Empty ⇒ {
        logger.info("set postLoginCmds")
        "*" #> <div> Please sign in to chat { ajaxButton("Sign in and press this button", template.setHtml _) }</div>
      }
      case Full(u) ⇒ {
        val chatControlName = StringHelpers.randomString(8)
        "*" #> <div id="open-chat-rooms"><div id="stream-chat">{ ChatBox(m.id) }</div></div>
               <div data-lift={ "comet?type=ChatControls;name=" + chatControlName }>
                 <ul id="chat-list"><li class="chat-room"></li></ul>
                 <form id="create-chat-room" class="lift:form.ajax">
                   Make a chat room:
                   <input type="text" id="chat-name">Name</input>
                   <button type="button" id="make-chat-room">Create the chat room</button>
                 </form>
               </div>
      }
    }
  }
}
  
object ChatBox {
  def apply(id: String) =
    <div class="chat-box" data-lift={ "comet?type=ChatUser;name=" + id }>
      Now chatting on { id }
      <div class="messages"></div>
      <form class="lift:form.ajax">
        <input type="text" name="message"/>
        <input type="submit" name="send-message"/>
      </form>
    </div>
}
