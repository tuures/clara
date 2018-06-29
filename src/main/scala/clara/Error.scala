package clara

import ai.x.safe._

trait Error {
  def humanFormat: String
}

case class GeneralError(message: String) extends Error {
  def humanFormat = message
}

case class SourceError(pos: Pos, message: String) extends Error {
  def humanFormat = safe"${pos.humanFormat}: $message"
}
