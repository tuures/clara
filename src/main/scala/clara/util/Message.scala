package clara.util

trait Message {
  def message: String
  def humanFormat: String
}

case class GeneralMessage(message: String) extends Message {
  def humanFormat = message
}
