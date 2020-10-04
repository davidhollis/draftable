package models

import java.time.Duration
import play.api.libs.json._
import play.api.libs.functional.syntax._
import java.time.ZonedDateTime

case class DraftState(
  id: Identifier[DraftState],
  players: Seq[Player],
  zones: Set[Zone],
  turns: Map[Identifier[Player], Turn],
  status: DraftState.Status,
) extends Identifiable[DraftState] {
  def zone(zone: Identifiable[Zone]): Option[Zone] = zones.find(_.id == zone.id)

  def zone(owner: Identifiable[Player], name: String): Option[Zone] =
    zones.find(z => z.owner == Some(owner) && z.name == name)

  def zonesByName(name: String): Set[Zone] = zones.filter(_.name == name)

  def playerCount: Int = players.size

  def modifyZone(selectedZone: Identifiable[Zone])(op: Zone => Zone): DraftState =
    copy(zones = zones.map { zone =>
      if (zone.id == selectedZone.id)
        op(zone)
      else
        zone
    })

  def filteredViewFor(player: Identifiable[Player]): DraftState =
    copy(
      turns = turns.filter { case (turnPlayer, _) => turnPlayer.id == player.id },
      zones = zones.map(_.filteredViewFor(player)),
    )

  def moveSet(
    set: Identifiable[CardSet],
    player: Identifiable[Player],
    from: Identifiable[Zone],
    to: Identifiable[Zone],
  ): Option[DraftState] =
    for {
      fromZone <- this.zone(from)
      toZone <- this.zone(to)
      if player == Player.System || (fromZone.isOwnedBy(player) && toZone.isOwnedBy(player))
      movingSet <- fromZone.cardSet(set)
    } yield {
      this
        .modifyZone(fromZone)(_.removeSet(movingSet))
        .modifyZone(toZone)(_.enqueueSet(movingSet))
    }

  def moveNextSet(
    player: Identifiable[Player],
    from: Identifiable[Zone],
    to: Identifiable[Zone],
  ): Option[DraftState] =
    for {
      fromZone <- this.zone(from)
      toZone <- this.zone(to)
      if player == Player.System || (fromZone.isOwnedBy(player) && toZone.isOwnedBy(player))
      movingSet <- fromZone.nextSet
    } yield {
      this
        .modifyZone(fromZone)(_.removeSet(movingSet))
        .modifyZone(toZone)(_.enqueueSet(movingSet))
    }

  def removeSet(
    set: Identifiable[CardSet],
    player: Identifiable[Player],
    from: Identifiable[Zone],
  ): Option[DraftState] =
    for {
      fromZone <- this.zone(from)
      if player == Player.System || fromZone.isOwnedBy(player)
    } yield {
      this.modifyZone(fromZone)(_.removeSet(set))
    }

  def moveCard(
    card: Identifiable[CardInstance],
    player: Identifiable[Player],
    from: (Identifiable[Zone], Identifiable[CardSet]),
    to: (Identifiable[Zone], Identifiable[CardSet]),
  ): Option[DraftState] =
    for {
      fromZone <- this.zone(from._1)
      toZone <- this.zone(to._1)
      if player == Player.System || (fromZone.isOwnedBy(player) && toZone.isOwnedBy(player))
      fromSet <- fromZone.cardSet(from._2)
      toSet = to._2
      movingCard <- fromSet.card(card)
    } yield {
      this
        .modifyZone(fromZone)(_.modifySet(fromSet)(_.removeCard(movingCard)))
        .modifyZone(toZone)(_.modifySet(toSet)(_.pushCard(movingCard)))
    }

  def removeCard(
    card: Identifiable[CardInstance],
    player: Identifiable[Player],
    from: (Identifiable[Zone], Identifiable[CardSet]),
  ): Option[DraftState] =
    for {
      fromZone <- this.zone(from._1)
      if player == Player.System || fromZone.isOwnedBy(player)
      fromSet = from._2
    } yield {
      this.modifyZone(fromZone)(_.modifySet(fromSet)(_.removeCard(card)))
    }

  def clearTurnTimeout(player: Identifiable[Player]): Option[DraftState] =
    for {
      currentTurn <- turns.get(player.id)
    } yield copy(turns = turns + (player.id -> currentTurn.withoutExpiration))

  def clearTurns(): DraftState = copy(turns = Map.empty)

  def nextTurn(
    player: Identifiable[Player],
    timeout: Option[Duration],
    name: String,
    kvPairs: (String, String)*
  ): Option[DraftState] =
    for {
      _ <- players.find(_.id == player.id)
    } yield copy(
      turns = turns + (player.id -> Turn(
        Identifier[Turn](),
        name,
        timeout.map(ZonedDateTime.now().plus(_)),
        Map(kvPairs: _*),
      ))
    )

}

object DraftState {
  implicit val idPrefix: IdPrefix[DraftState] = IdPrefix[DraftState]("draft")

  sealed abstract class Status(override val toString: String)

  object Status {
    object WaitingForPlayers extends Status("waiting for players")
    object BuildingCardPool extends Status("building card pool")
    object InProgress extends Status("in progress")
    object Finalized extends Status("finalized")

    val all: Set[Status] = Set(WaitingForPlayers, BuildingCardPool, InProgress, Finalized)

    lazy val byName: Map[String, Status] = all.map(status => (status.toString -> status)).toMap

    implicit val reads: Reads[Status] = Reads {
      case JsString(statusString) =>
        Status.byName.get(statusString) match {
          case Some(status) => JsSuccess(status)
          case None =>
            JsError(s"Invalid status '${statusString}'. Status must be one of ${Status.all}")
        }
      case _ => JsError("Malformed status. Status must be a string.")
    }

    implicit val writes: Writes[Status] = Writes { status => JsString(status.toString) }

  }

  implicit val readsTurnMap: Reads[Map[Identifier[Player], Turn]] = Reads {
    case JsObject(turnMapRaw) => {
      val turnMapOptions = turnMapRaw.map {
        case (playerIdStr, turnObject) =>
          Identifier.applyOpt[Player](playerIdStr) zip turnObject.asOpt[Turn]
      }

      if (turnMapOptions.exists(_.isEmpty))
        JsError("Malformed turn map. The turn map should be an object with player ids for keys and Turn objects for values.")
      else
        JsSuccess(turnMapOptions.flatten.toMap)
    }
    case _ =>
      JsError("Malformed turn map. The turn map should be an object with player ids for keys and Turn objects for values.")
  }

  implicit val writesTurnMap: Writes[Map[Identifier[Player], Turn]] = Writes { turnMap =>
    JsObject(turnMap.map {
      case (playerId, turn) => playerId.toString -> Json.toJson(turn)
    })
  }

  implicit val format: Format[DraftState] = (
    (__ \ implicitly[JsonConfiguration].naming("id")).format[Identifier[DraftState]] and
      (__ \ implicitly[JsonConfiguration].naming("players")).format[Seq[Player]] and
      (__ \ implicitly[JsonConfiguration].naming("zones")).format[Set[Zone]] and
      (__ \ implicitly[JsonConfiguration].naming("turns")).format[Map[Identifier[Player], Turn]] and
      (__ \ implicitly[JsonConfiguration].naming("status")).format[Status]
  )(DraftState.apply, unlift(DraftState.unapply))

}
