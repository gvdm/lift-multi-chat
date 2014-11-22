package code.snippet

import net.liftweb.common.Loggable
import net.liftweb.sitemap.Menu
import code.model.User
import net.liftweb.http.StatefulSnippet
import net.liftweb.http.RequestVar
import net.liftweb.http.S
import net.liftweb.http.SHtml
import net.liftweb.util.Helpers._
import net.liftweb.common._
import scala.xml.Text
import net.liftweb.http.js.JsCmd

class UserRegister extends StatefulSnippet with Loggable {

  private object email extends RequestVar("")
  private object conf_email extends RequestVar("")
  private object pw extends RequestVar("")
  private object conf_pw extends RequestVar("")
  private object referer extends RequestVar(S.referer openOr "/")

  def dispatch = {
    case "register" ⇒ register
  }

  def register = {
    def processRegister(): JsCmd = {

      logger.debug("processRegister: email=" + email + " confirm email=" + conf_email + " pw=" + pw + " referer=" + referer + " confirm pw=" + conf_pw)

      User.findByEmail(email) match { //all is well check if new or known user
        case Empty ⇒ { //yes it is a new user
          if (pw.trim == conf_pw.trim) {
            val newUser = User.createRecord.email(email).password(pw)
            newUser.password.hashIt
            newUser.save()
            //login the new user
            User.logUserIn(User.findByEmail(email).openOr(null), false, false)
            S.redirectTo("/")
          } else {
            S.error("pw", "Passwords don't match!")
          }
        }
        case Full(user)       ⇒ { S.error("email", "This email is already registered!") }
        case Failure(_, _, _) ⇒ logger.debug("WHAT WHAT WHAT")
      }
    }

    //logger.debug("referer.is="+referer.is)
    "name=email" #> SHtml.email(email) &
      /*"name=conf_email" #> SHtml.textElem(conf_email) &*/
      "name=conf_pw" #> SHtml.password("", (x) ⇒ conf_pw.set(x), "id" -> "input_conf_pw") &
      "name=pw" #> (SHtml.password("", (x) ⇒ pw.set(x), "id" -> "input_pw", "title" -> "Minimum 6 alphanumeric characters", "pattern" -> "\\w{6,}", "placeholder" -> "password") ++ SHtml.hidden(() ⇒ referer.set(referer.is))) &
      "type=submit" #> SHtml.ajaxSubmit("Register", processRegister)
  }

}