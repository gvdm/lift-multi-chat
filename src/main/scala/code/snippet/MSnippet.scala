package code.snippet

import net.liftweb.common.Full
import net.liftweb.util.Helpers._
import net.liftweb.http.S

import code.model.M
import code.lib.currentM

class MSnippet(m: M) {
  currentM.set(Full(m))
  println("set current M to "+m.id)
  
  def render = "#m-id *" #> m.id
}