package models

import play.api.libs.json._
import play.api.libs.functional.syntax._

case class Card(
  id: Identifier[Card],
  name: String,
  colorProfile: ColorProfile,
) extends Identifiable[Card]

object Card {
  implicit val idPrefix: IdPrefix[Card] = IdPrefix[Card]("card")

  implicit val format: Format[Card] = (
    (__ \ implicitly[JsonConfiguration].naming("id")).format[Identifier[Card]] and
      (__ \ implicitly[JsonConfiguration].naming("name")).format[String] and
      (__ \ implicitly[JsonConfiguration].naming("colorProfile")).format[ColorProfile]
  )(Card.apply, unlift(Card.unapply))

}
