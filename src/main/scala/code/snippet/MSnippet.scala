package code.snippet

import net.liftweb.common.Full
import net.liftweb.util.Helpers._
import net.liftweb.http.S

import code.model.M

class MSnippet(m: M) {
  def render = "#m-id *" #> m.id
}