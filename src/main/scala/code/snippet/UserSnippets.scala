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
import net.liftweb.http.js.JsCmds._
import net.liftmodules.mongoauth.model.ExtSession
import net.liftweb.http.js.JsCmds.RedirectTo
import net.liftmodules.mongoauth.LoginRedirect
import scala.xml.NodeSeq
import net.liftweb.http.js.JsCmd
import net.liftweb.json.JsonAST._
import net.liftweb.json.JsonDSL._
import net.liftweb.http.js.JsCmd._
import net.liftweb.http.js.JsCmds
import net.liftweb.http.js.JsExp
import net.liftweb.http.js.JsExp._
import net.liftweb.http.js.JsObj
import net.liftweb.http.js.JE.AnonFunc
import net.liftweb.http.js.JE._
import net.liftweb.http.js.JE.JsRaw
import net.liftweb.http.js.jquery.JqJE.Jq

class UserSnippets extends StatefulSnippet with Loggable {

  def dispatch = {
    case "login"     ⇒ login
    case "logout"    ⇒ logout
  }

  def login = {
    object email extends RequestVar("")
    object pw extends RequestVar("")
    object referer extends RequestVar(S.referer openOr "/")

    def processLogin(): JsCmd = {

      if (pw.length > 0 && email.length > 0) {
        User.findByEmail(email) match {
          case Full(user) ⇒ {
            if (user.password.isMatch(pw)) {
              logger.debug("pwd matched")
              User.logUserIn(user, true, true)
              RedirectTo(LoginRedirect.openOr("/"))
            }
          }
          case _ ⇒ {
            S.error("Invalid credentials")
            Noop
          }
        }
      } else if (pw.length() > 0) {
        S.error("Please enter email")
      } else {
        S.error("Please enter password")
      }
      
    }

    "name=email" #> SHtml.email(email, "placeholder" -> "email") &
      "name=pw" #> (SHtml.password("", (x) ⇒ pw.set(x), "id" -> "input_pw", "placeholder" -> "password") ++ SHtml.hidden(() ⇒ referer.set(referer.is))) &
      "type=submit" #> SHtml.ajaxSubmit("Login", processLogin)
  }

  def logout = {
    def processLogout() = {
      User.logUserOut()
    }
    "type=submit" #> SHtml.onSubmitUnit(processLogout)
  }

}