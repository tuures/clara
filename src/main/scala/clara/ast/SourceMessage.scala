package clara.ast

import clara.util.Message

import clara.util.Safe._

case class SourceMessage(pos: Pos, message: String) extends Message {
  def humanFormat = safe"${pos.humanFormat}: $message"
}
