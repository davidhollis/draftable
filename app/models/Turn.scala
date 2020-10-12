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

  def isExpired(now: ZonedDateTime): Boolean =
    expires.map(expiry => now.isAfter(expiry)).getOrElse(false)

}

object Turn {
  implicit val idPrefix: IdPrefix[Turn] = IdPrefix[Turn]("turn")

  implicit val format: Format[Turn] = (
    field[Identifier[Turn]]("id") and
      field[String]("name") and
      optionalField[ZonedDateTime]("expires") and
      field[Map[String, String]]("metadata")
  )(Turn.apply, unlift(Turn.unapply))

}
