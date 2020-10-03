package models

import play.api.libs.json._
import play.api.libs.functional.syntax._

case class CardInstance(
  id: Identifier[CardInstance],
  card: Card,
  attributes: Set[String],
) {
  def addAttributes(newAttrs: Set[String]): CardInstance = copy(attributes = attributes | newAttrs)
  def removeAttributes(attrs: Set[String]): CardInstance = copy(attributes = attributes diff attrs)
  def hasAttributes(checkAttrs: Set[String]): Boolean = checkAttrs subsetOf attributes
}

object CardInstance {
  implicit val idPrefix: IdPrefix[CardInstance] = IdPrefix[CardInstance]("card-instance")

  implicit val format: Format[CardInstance] = (
    (__ \ implicitly[JsonConfiguration].naming("id")).format[Identifier[CardInstance]] and
      (__ \ implicitly[JsonConfiguration].naming("card")).format[Card] and
      (__ \ implicitly[JsonConfiguration].naming("attributes")).format[Set[String]]
  )(CardInstance.apply, unlift(CardInstance.unapply))

}
