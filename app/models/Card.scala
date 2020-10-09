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
    field[Identifier[Card]]("id") and
      field[String]("name") and
      field[ColorProfile]("colorProfile")
  )(Card.apply, unlift(Card.unapply))

}
