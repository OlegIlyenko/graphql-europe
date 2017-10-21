package views

case class Config(
  gaCode: Option[String],
  smoochApiKey: Option[String],
  graphCoolProjectKey: String,
  canonicalUrl: String,
  cdn: Boolean,
  ticketsUrl: String,
  googleMapsApiKey: String,
  path: String = ""
)
