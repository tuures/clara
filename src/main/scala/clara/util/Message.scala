package clara.util
// TODO move to clara?

trait Message {
  // TODO: level: info, warning, error
  def message: String
  def humanFormat: String
}

case class GeneralMessage(message: String) extends Message {
  def humanFormat = message
}
