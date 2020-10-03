package models

import play.api.libs.json._
import play.api.libs.functional.syntax._
import models.Zone.Visibility.Sets
import models.Zone.Visibility.TopCard
import models.Zone.Visibility.Cards

case class CardSet(
  id: Identifier[CardSet],
  setType: CardSet.Type,
  name: String,
  cards: Seq[CardInstance],
) extends Identifiable[CardSet] {
  def isEmpty: Boolean = cards.isEmpty

  def card(card: Identifiable[CardInstance]): Option[CardInstance] =
    cards.find(_.id == card.id)

  def cardsByAttribute(attribute: String): Seq[CardInstance] =
    cards.filter(_.hasAttributes(Set(attribute)))

  def topCard: Option[CardInstance] = cards.headOption

  def popCard: (Option[CardInstance], CardSet) =
    cards match {
      case popped :: rest => (Some(popped), copy(cards = rest))
      case Nil            => (None, this)
    }

  def pushCard(card: CardInstance): CardSet = copy(cards = card +: cards)

  def removeCard(card: Identifiable[CardInstance]): CardSet =
    copy(cards = cards.filterNot(_.id == card.id))

  def modifyCard(
    selectedCard: Identifiable[CardInstance]
  )(
    op: CardInstance => CardInstance
  ): CardSet =
    copy(cards = cards.map { card =>
      if (card.id == selectedCard.id)
        op(card)
      else
        card
    })

  def filteredView(visibility: Zone.Visibility): CardSet =
    visibility match {
      case Sets    => copy(cards = Seq.empty)
      case TopCard => copy(cards = topCard.toSeq)
      case Cards   => this
    }

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
