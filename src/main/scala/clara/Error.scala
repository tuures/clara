package clara

import ai.x.safe._

case class Error(pos: Pos, message: String) {
  def format = safe"${pos.format}: $message"
}
