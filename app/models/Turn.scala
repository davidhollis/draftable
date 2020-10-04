package models

import java.time.ZonedDateTime
import play.api.libs.json._
import play.api.libs.functional.syntax._

case class Turn(
  id: Identifier[Turn],
  name: String,
  expires: Option[ZonedDateTime],
  metadata: Map[String, String],
) extends Identifiable[Turn] {
  def withoutExpiration: Turn = copy(expires = None)
}

object Turn {
  implicit val idPrefix: IdPrefix[Turn] = IdPrefix[Turn]("turn")

  implicit val format: Format[Turn] = (
    (__ \ implicitly[JsonConfiguration].naming("id")).format[Identifier[Turn]] and
      (__ \ implicitly[JsonConfiguration].naming("name")).format[String] and
      (__ \ implicitly[JsonConfiguration].naming("expires")).formatNullable[ZonedDateTime] and
      (__ \ implicitly[JsonConfiguration].naming("metadata")).format[Map[String, String]]
  )(Turn.apply, unlift(Turn.unapply))

}
