package code.lib

import net.liftweb.http.RequestVar
import net.liftweb.common._
import code.model.M

object currentM extends RequestVar[Box[M]](Empty) 
