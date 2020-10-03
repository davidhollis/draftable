import play.api.libs.json.JsonConfiguration
import play.api.libs.json.JsonNaming.SnakeCase

package object models {
  implicit val jsonConfig: JsonConfiguration = JsonConfiguration(SnakeCase)
}
