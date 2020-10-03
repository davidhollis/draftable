package models

import play.api.libs.json._
import play.api.libs.functional.syntax._

case class CardSet(
  id: Identifier[CardSet],
  setType: CardSet.Type,
  name: String,
  cards: Seq[CardInstance],
) extends Identifiable[CardSet] {
  def isEmpty: Boolean = cards.isEmpty

  def find(instanceId: Identifiable[CardInstance]): Option[CardInstance] =
    cards.find(_.id == instanceId.id)

  def findByAttribute(attribute: String): Seq[CardInstance] =
    cards.filter(_.hasAttributes(Set(attribute)))

  def removeCard(instanceId: Identifiable[CardInstance]): CardSet =
    copy(cards = cards.filterNot(_.id == instanceId.id))

  def addAttributes(instanceId: Identifiable[CardInstance], newAttrs: Set[String]): CardSet =
    copy(cards = cards.map { instance =>
      if (instance.id == instanceId.id) instance.addAttributes(newAttrs) else instance
    })

  def removeAttributes(instanceId: Identifiable[CardInstance], attrs: Set[String]): CardSet =
    copy(cards = cards.map { instance =>
      if (instance.id == instanceId.id) instance.removeAttributes(attrs) else instance
    })

  def clearAttributes(instanceId: Identifiable[CardInstance]): CardSet =
    copy(cards = cards.map { instance =>
      if (instance.id == instanceId.id) instance.clearAttributes() else instance
    })

}

object CardSet {
  implicit val idPrefix: IdPrefix[CardSet] = IdPrefix[CardSet]("card-set")

  sealed abstract class Type(override val toString: String)

  object Type {
    object BoosterPack extends Type("booster pack")
    object Stack extends Type("stack")
    object Deck extends Type("deck")
    object Sideboard extends Type("sideboard")
    object Other extends Type("other")

    val all: Set[Type] = Set(BoosterPack, Stack, Deck, Sideboard, Other)

    lazy val byName: Map[String, Type] = all.map(tpe => (tpe.toString -> tpe)).toMap

    implicit val reads: Reads[Type] = Reads {
      case JsString(typeName) => JsSuccess(Type.byName.getOrElse(typeName, Other))
      case _                  => JsSuccess(Other)
    }

    implicit val writes: Writes[Type] = Writes { tpe => JsString(tpe.toString) }
  }

  implicit val format: Format[CardSet] = (
    (__ \ implicitly[JsonConfiguration].naming("id")).format[Identifier[CardSet]] and
      (__ \ implicitly[JsonConfiguration].naming("setType")).format[CardSet.Type] and
      (__ \ implicitly[JsonConfiguration].naming("name")).format[String] and
      (__ \ implicitly[JsonConfiguration].naming("cards")).format[Seq[CardInstance]]
  )(CardSet.apply, unlift(CardSet.unapply))

}
