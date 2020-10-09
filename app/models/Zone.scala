package models

import play.api.libs.json._
import play.api.libs.functional.syntax._

case class Zone(
  id: Identifier[Zone],
  name: String,
  owner: Option[Identifier[Player]],
  sets: Seq[CardSet],
  canSelect: Zone.Target,
  canDrop: Zone.Target,
  visible: Map[Identifier[Player], Zone.Visibility] = Map.empty,
  defaultView: Zone.ViewType = Zone.ViewType.Spread,
) extends Identifiable[Zone] {
  def isOwnedBy(player: Identifiable[Player]): Boolean = owner.map(_ == player.id).getOrElse(false)

  def isEmpty: Boolean = sets.isEmpty

  def cardSet(cardSet: Identifiable[CardSet]): Option[CardSet] = sets.find(_.id == cardSet.id)

  def cardSet(setName: String): Option[CardSet] = sets.find(_.name == setName)

  def nextSet: Option[CardSet] = sets.headOption

  def dequeueSet: (Option[CardSet], Zone) =
    sets match {
      case dequeued :: rest => (Some(dequeued), copy(sets = rest))
      case Nil              => (None, this)
    }

  def enqueueSet(newSet: CardSet): Zone = copy(sets = sets :+ newSet)

  def removeSet(cardSet: Identifiable[CardSet]): Zone =
    copy(sets = sets.filterNot(_.id == cardSet.id))

  def modifySet(selectedCardSet: Identifiable[CardSet])(op: CardSet => CardSet): Zone =
    copy(sets = sets.map { cardSet =>
      if (cardSet.id == selectedCardSet.id)
        op(cardSet)
      else
        cardSet
    })

  def filteredViewFor(player: Identifiable[Player]): Zone =
    visible.get(player.id) match {
      case Some(visibility) => copy(sets = sets.map(_.filteredView(visibility)))
      case None             => copy(sets = Seq.empty)
    }

}

object Zone {
  implicit val idPrefix: IdPrefix[Zone] = IdPrefix[Zone]("zone")

  sealed abstract class Visibility(val name: String, override val toString: String) extends Enum

  object Visibility extends EnumOps[Visibility]("zone visibility") {
    object Sets extends Visibility("sets", "sets only")
    object TopCard extends Visibility("top-card", "sets and the top card of each set")
    object Cards extends Visibility("cards", "sets and cards")

    val all: Set[Visibility] = Set(Sets, TopCard, Cards)
  }

  implicit val readsVisibilityMap: Reads[Map[Identifier[Player], Visibility]] = Reads {
    case JsObject(vmapRaw) => {
      val vmap = vmapRaw.flatMap {
        case (playerIdStr, JsString(visibilityStr)) =>
          Identifier.applyOpt[Player](playerIdStr) zip Visibility.byName.get(visibilityStr)
        case _ => None
      }
      JsSuccess(vmap.toMap)
    }
    case _ => JsSuccess(Map.empty)
  }

  implicit val writesVisibilityMap: Writes[Map[Identifier[Player], Visibility]] = Writes { vmap =>
    JsObject(vmap.map {
      case (playerId, visibility) => (playerId.toString -> JsString(visibility.name))
    })
  }

  sealed abstract class Target(val name: String) extends Enum

  object Target extends EnumOps[Target]("zone selection target") {
    object NoTarget extends Target("nothing")
    object Sets extends Target("sets only")
    object Cards extends Target("cards only")

    val all: Set[Target] = Set(NoTarget, Sets, Cards)

    override val defaultValue = Some(NoTarget)

  }

  sealed abstract class ViewType(val name: String) extends Enum

  object ViewType extends EnumOps[ViewType]("zone view type") {
    object Spread extends ViewType("spread")
    object Stacked extends ViewType("stacked")
    object Columns extends ViewType("columns")

    val all: Set[ViewType] = Set(Spread, Stacked, Columns)

    override val defaultValue = Some(Spread)
  }

  implicit val format: Format[Zone] = (
    field[Identifier[Zone]]("id") and
      field[String]("name") and
      optionalField[Identifier[Player]]("owner") and
      field[Seq[CardSet]]("sets") and
      field[Target]("canSelect") and
      field[Target]("canDrop") and
      field[Map[Identifier[Player], Visibility]]("visible") and
      field[ViewType]("defaultView")
  )(Zone.apply, unlift(Zone.unapply))

}
