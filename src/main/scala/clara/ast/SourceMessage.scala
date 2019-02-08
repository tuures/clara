package clara.ast

import clara.util.Message

import ai.x.safe._

case class SourceMessage(pos: Pos, message: String) extends Message {
  def humanFormat = safe"${pos.humanFormat}: $message"
}
