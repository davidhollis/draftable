package models

import java.time.{ Clock, Duration, ZonedDateTime }
import play.api.libs.json._
import play.api.libs.functional.syntax._

case class DraftState(
  id: Identifier[DraftState],
  players: Seq[Player],
  zones: Set[Zone],
  turns: Map[Identifier[Player], Turn],
  status: DraftState.Status,
  properties: Map[String, String] = Map.empty,
) extends Identifiable[DraftState] {

  def property(name: String): Option[String] = properties.get(name)

  def setProperty(name: String, value: String): DraftState =
    copy(properties = properties + (name -> value))

  def requireStatus(requiredStatus: DraftState.Status): Option[DraftState] =
    if (status == requiredStatus)
      Some(this)
    else
      None

  def updateStatus(newStatus: DraftState.Status): DraftState = copy(status = newStatus)

  def player(player: Identifiable[Player]): Option[Player] = players.find(_.id == player.id)

  def nextPlayer(from: Identifiable[Player], direction: Int = 1): Option[Player] =
    for {
      playerIndex <- {
        val idx = players.indexWhere(_.id == from.id)
        if (idx >= 0) Some(idx) else None
      }
      nextPlayerIndex = (playerIndex + direction.sign) % players.size
      nextPlayer <- players.lift(nextPlayerIndex)
    } yield nextPlayer

  def addPlayer(player: Player): DraftState = copy(players = players :+ player)

  def modifyPlayer(selectedPlayer: Identifiable[Player])(op: Player => Player): DraftState =
    copy(players = players.map { player =>
      if (player.id == selectedPlayer.id) {
        op(player)
      } else {
        player
      }
    })

  def playerCount: Int = players.size

  def zone(zone: Identifiable[Zone]): Option[Zone] = zones.find(_.id == zone.id)

  def zone(owner: Identifiable[Player], name: String): Option[Zone] =
    zones.find(z => z.owner == Some(owner) && z.name == name)

  def zonesByName(name: String): Set[Zone] = zones.filter(_.name == name)

  def modifyZone(selectedZone: Identifiable[Zone])(op: Zone => Zone): DraftState =
    copy(zones = zones.map { zone =>
      if (zone.id == selectedZone.id)
        op(zone)
      else
        zone
    })

  def cardSet(at: (Identifiable[Zone], Identifiable[CardSet])): Option[CardSet] =
    zone(at._1).flatMap(_.cardSet(at._2))

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

  def clearTurns(): Option[DraftState] = Some(copy(turns = Map.empty))

  def nextTurn(
    player: Identifiable[Player],
    timeout: Option[(Duration, Clock)],
    name: String,
    kvPairs: (String, String)*
  ): Option[DraftState] =
    for {
      _ <- players.find(_.id == player.id)
    } yield copy(
      turns = turns + (player.id -> Turn(
        Identifier[Turn](),
        name,
        timeout.map { case (dur, clock) => ZonedDateTime.now(clock).plus(dur) },
        Map(kvPairs: _*),
      ))
    )

}

object DraftState {
  implicit val idPrefix: IdPrefix[DraftState] = IdPrefix[DraftState]("draft")

  sealed abstract class Status(val name: String) extends Enum

  object Status extends EnumOps[Status]("draft status") {
    object WaitingForPlayers extends Status("waiting for players")
    object BuildingCardPool extends Status("building card pool")
    object InProgress extends Status("in progress")
    object Finalized extends Status("finalized")

    val all: Set[Status] = Set(WaitingForPlayers, BuildingCardPool, InProgress, Finalized)
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
    field[Identifier[DraftState]]("id") and
      field[Seq[Player]]("players") and
      field[Set[Zone]]("zones") and
      field[Map[Identifier[Player], Turn]]("turns") and
      field[Status]("status") and
      field[Map[String, String]]("properties")
  )(DraftState.apply, unlift(DraftState.unapply))

}
