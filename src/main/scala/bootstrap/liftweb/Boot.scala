package bootstrap.liftweb

import com.mongodb.MongoClient
import net.liftweb.common.Full
import net.liftweb.http.LiftRules
import net.liftweb.http.LiftRulesMocker.toLiftRules
import net.liftweb.sitemap._
import net.liftweb.sitemap.Loc._
import net.liftweb.util.DefaultConnectionIdentifier
import net.liftmodules.JQueryModule
import net.liftmodules.mongoauth.MongoAuth
import net.liftweb.mongodb.MongoDB
import net.liftweb.sitemap.{* => *}
import net.liftweb.sitemap.Loc._
import net.liftweb.sitemap.Loc._
import net.liftweb.sitemap.LocPath.stringToLocPath
import net.liftweb.sitemap.Menu
import net.liftweb.sitemap.SiteMap
import net.liftmodules.mongoauth.Locs
import net.liftmodules.mongoauth.Locs.RequireNotLoggedIn

import code.model.M
import code.model.User

/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {

  def boot {
    
    MongoDB.defineDb(DefaultConnectionIdentifier, new MongoClient, "test")

    LiftRules.addToPackages("code")

    MongoAuth.init(authUserMeta = User)
    
    // Build SiteMap
    def sitemap = SiteMap(
      Menu.i("Home") / "index", // the simple way to declare a menu

      Menu.param[M]("M", "M", s ⇒ Full(M(s)), m => m.id) / "m" / *,
      Menu.i("Register") / "user" / "register" >> RequireNotLoggedIn,
      Menu.i("Login") / "user" / "login" >> RequireNotLoggedIn,
      
      // more complex because this menu allows anything in the
      // /static path to be visible
      Menu(Loc("Static", Link(List("static"), true, "/static/index"),
             "Static Content")))
    LiftRules.setSiteMapFunc(() => sitemap)

    JQueryModule.InitParam.JQuery = JQueryModule.JQuery211
    JQueryModule.init()

    //Show the spinny image when an Ajax call starts
    LiftRules.ajaxStart =
      Full(() => LiftRules.jsArtifacts.show("ajax-loader").cmd)

    // Make the spinny image go away when it ends
    LiftRules.ajaxEnd =
      Full(() => LiftRules.jsArtifacts.hide("ajax-loader").cmd)

    // Force the request to be UTF-8
    LiftRules.early.append(_.setCharacterEncoding("UTF-8"))

  }
}
