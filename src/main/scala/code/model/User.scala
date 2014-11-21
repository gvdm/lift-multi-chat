package code.model

import scala.xml.Text
import org.bson.types.ObjectId
import net.liftmodules.mongoauth.AuthUtil
import net.liftmodules.mongoauth.MongoAuth
import net.liftmodules.mongoauth.MongoAuthUser
import net.liftmodules.mongoauth.Permission
import net.liftmodules.mongoauth.ProtoAuthUserMeta
import net.liftmodules.mongoauth.field.PasswordField
import net.liftmodules.mongoauth.field.PermissionListField
import net.liftmodules.mongoauth.model.LoginToken
import net.liftmodules.mongoauth.model.Role
import net.liftweb.common.Box
import net.liftweb.common.Empty
import net.liftweb.common.Full
import net.liftweb.http.LiftResponse
import net.liftweb.http.RedirectResponse
import net.liftweb.http.RedirectState
import net.liftweb.http.RedirectWithState
import net.liftweb.http.S
import net.liftweb.http.SessionVar
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds._
import net.liftweb.json.JsonDSL._
import net.liftweb.mongodb.record.field.ObjectIdPk
import net.liftweb.mongodb.record.field.StringRefListField
import net.liftweb.util.FieldError
import net.liftweb.util.Mailer.From
import net.liftweb.util.Mailer.PlainMailBodyType
import net.liftweb.util.Mailer.Subject
import net.liftweb.util.Mailer.To
import net.liftweb.util.Mailer.sendMail
import net.liftweb.record.field.StringField
import net.liftweb.record.field.EmailField
import net.liftweb.record.field.BooleanField

class User extends MongoAuthUser[User] with ObjectIdPk[User] {

  def meta = User

  def userIdAsString = id.toString

  object username extends StringField(this, 32) {
    override def displayName = "Username"
    override def setFilter = trim _ :: super.setFilter

    private def valUnique(msg: ⇒ String)(value: String): List[FieldError] = {
      if (value.length > 0)
        meta.findAll(name, value).filterNot(_.id.get == owner.id.get).map(u ⇒ FieldError(this, Text(msg)))
      else
        Nil
    }
    override def validations =
      valUnique(S ? "liftmodule-monogoauth.monogoAuthUser.username.validation.unique") _ ::
        valMinLen(3, S ? "liftmodule-monogoauth.monogoAuthUser.username.validation.min.length") _ ::
        valMaxLen(32, S ? "liftmodule-monogoauth.monogoAuthUser.username.validation.max.length") _ ::
        super.validations
  }
  
  object externId extends StringField(this, 128)
  object provider extends StringField(this, 64)
  object firstName extends StringField(this, 64)
  object lastName extends StringField(this, 64)

  object email extends EmailField(this, 254) {
    override def displayName = "Email"
    override def setFilter = trim _ :: toLower _ :: super.setFilter

    private def valUnique(msg: ⇒ String)(value: String): List[FieldError] = {
      owner.meta.findAll(name, value).filter(_.id.get != owner.id.get).map(u ⇒
        FieldError(this, Text(msg))
      )
    }
    override def validations =
      valUnique("That email address is already registered with us") _ ::
        valMaxLen(254, "Email must be 254 characters or less") _ ::
        super.validations
  }

  // email address has been verified by clicking on a LoginToken link
  object verified extends BooleanField(this) {
    override def displayName = "Verified"
  }

  object password extends PasswordField(this, 6, 32) {
    override def displayName = "Password"
  }

  object permissions extends PermissionListField(this)
  object roles extends StringRefListField(this, Role) {
    def permissions: List[Permission] = objs.flatMap(_.permissions.get)
    def names: List[String] = objs.map(_.id.get)
  }
  lazy val authPermissions: Set[Permission] = (permissions.get ::: roles.permissions).toSet
  lazy val authRoles: Set[String] = roles.names.toSet

  lazy val fancyEmail = AuthUtil.fancyEmail(username.get, email.get)
}

object User extends User with ProtoAuthUserMeta[User] {

  def findByStringId(id: String): Box[User] =
    if (ObjectId.isValid(id)) find(new ObjectId(id))
    else Empty

  def findByEmail(in: String): Box[User] = find(email.name, in)
  def findByUsername(in: String): Box[User] = find(username.name, in)

  /*
	 * MongoAuth vars
	 */
  private lazy val indexUrl = "/"
  private lazy val registerUrl = "/user/register"
  private lazy val loginTokenAfterUrl = MongoAuth.loginTokenAfterUrl.vend

  override def handleLoginToken: Box[LiftResponse] = {
    val resp = S.param("token").flatMap(LoginToken.findByStringId) match {
      case Full(at) if (at.expires.isExpired) ⇒ {
        at.delete_!
        RedirectWithState(indexUrl, RedirectState(() ⇒ { S.error(S ? "liftmodule-monogoauth.simpleUser.handleLoginToken.expiredToken") }))
      }
      case Full(at) ⇒ find(at.userId.get).map(user ⇒ {
        if (user.validate.length == 0) {
          user.verified(true)
          user.save(false)
          logUserIn(user)
          at.delete_!
          RedirectResponse(loginTokenAfterUrl)
        } else {
          at.delete_!
          regUser(user)
          RedirectWithState(registerUrl, RedirectState(() ⇒ { S.notice(S ? "liftmodule-monogoauth.simpleUser.handleLoginToken.completeRegistration") }))
        }
      }).openOr(RedirectWithState(indexUrl, RedirectState(() ⇒ { S.error(S ? "liftmodule-monogoauth.simpleUser.handleLoginToken.userNotFound") })))
      case _ ⇒ RedirectWithState(indexUrl, RedirectState(() ⇒ { S.warning(S ? "liftmodule-monogoauth.simpleUser.handleLoginToken.noToken") }))
    }
    Full(resp)
  }

  // send an email to the user with a link for logging in
  def sendLoginToken(user: User): Unit = {
    val siteName = "WildEarth"
    import net.liftweb.util.Mailer._
    val token = LoginToken.createForUserIdBox(user.id.get)
    val msgTxt =
      """
        |Someone requested a link to change your password on the %s website.
        |
        |If you did not request this, you can safely ignore it. It will expire 48 hours from the time this message was sent.
        |
        |Follow the link below or copy and paste it into your internet browser.
        |
        |%s
        |
        |Thanks,
        |%s
      """.format(siteName, token.map(_.url), "The website").stripMargin
    sendMail(
      From(MongoAuth.systemFancyEmail),
      Subject("%s Password Help".format(siteName)),
      To(user.fancyEmail),
      PlainMailBodyType(msgTxt)
    )
  }

  // used during login process
  object loginCredentials extends SessionVar[LoginCredentials](LoginCredentials(""))
  object regUser extends SessionVar[User](currentUser openOr createRecord)
}

case class LoginCredentials(email: String, isRememberMe: Boolean = false)
