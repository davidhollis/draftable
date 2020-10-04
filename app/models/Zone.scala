package models

import play.api.libs.json._
import play.api.libs.functional.syntax._

case class Zone(
  id: Identifier[Zone],
  name: String,
  owner: Option[Identifier[Player]],
  sets: Seq[CardSet],
  visible: Map[Identifier[Player], Zone.Visibility] = Map.empty,
  focused: Boolean = false,
  canPickFrom: Boolean = false,
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

  sealed abstract class Visibility(val name: String, override val toString: String)

  object Visibility {
    object Sets extends Visibility("sets", "sets only")
    object TopCard extends Visibility("top-card", "sets and the top card of each set")
    object Cards extends Visibility("cards", "sets and cards")

    val all: Set[Visibility] = Set(Sets, TopCard, Cards)

    lazy val byName: Map[String, Visibility] = all.map(vis => (vis.name -> vis)).toMap
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

  implicit val format: Format[Zone] = (
    (__ \ implicitly[JsonConfiguration].naming("id")).format[Identifier[Zone]] and
      (__ \ implicitly[JsonConfiguration].naming("name")).format[String] and
      (__ \ implicitly[JsonConfiguration].naming("owner")).formatNullable[Identifier[Player]] and
      (__ \ implicitly[JsonConfiguration].naming("sets")).format[Seq[CardSet]] and
      (__ \ implicitly[JsonConfiguration].naming("visible"))
        .format[Map[Identifier[Player], Visibility]] and
      (__ \ implicitly[JsonConfiguration].naming("focused")).format[Boolean] and
      (__ \ implicitly[JsonConfiguration].naming("canPickFrom")).format[Boolean]
  )(Zone.apply, unlift(Zone.unapply))

}
