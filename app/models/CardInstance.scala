package models

import play.api.libs.json._
import play.api.libs.functional.syntax._

case class CardInstance(
  id: Identifier[CardInstance],
  card: Card,
  attributes: Set[String],
) extends Identifiable[CardInstance] {
  def addAttributes(newAttrs: Set[String]): CardInstance = copy(attributes = attributes | newAttrs)
  def removeAttributes(attrs: Set[String]): CardInstance = copy(attributes = attributes diff attrs)
  def clearAttributes(): CardInstance = copy(attributes = Set.empty)
  def hasAttributes(checkAttrs: Set[String]): Boolean = checkAttrs subsetOf attributes
}

object CardInstance {
  implicit val idPrefix: IdPrefix[CardInstance] = IdPrefix[CardInstance]("card-instance")

  implicit val format: Format[CardInstance] = (
    field[Identifier[CardInstance]]("id") and
      field[Card]("card") and
      field[Set[String]]("attributes")
  )(CardInstance.apply, unlift(CardInstance.unapply))

}
