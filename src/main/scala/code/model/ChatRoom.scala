package code.model

import net.liftweb.common.Loggable
import net.liftweb.record.field.StringField
import net.liftweb.record.field.TimeZoneField
import net.liftweb.mongodb.record.MongoRecord
import net.liftweb.mongodb.record.MongoMetaRecord
import net.liftweb.mongodb.record.field.StringPk
import net.liftweb.mongodb.record.field.MongoCaseClassListField
import net.liftweb.mongodb.record.field.ObjectIdRefField
import org.joda.time.DateTime

class ChatRoom extends MongoRecord[ChatRoom] with StringPk[ChatRoom] with Loggable {

  def meta = ChatRoom

  object name extends StringField(this, 64)
  object timezone extends TimeZoneField(this)
  object owner extends ObjectIdRefField(this, User)
  object chatlogs extends MongoCaseClassListField[ChatRoom, ChatLog](this)
  
}

object ChatRoom extends ChatRoom with MongoMetaRecord[ChatRoom]

case class ChatLog(timestamp: DateTime, from: User, message: String)
