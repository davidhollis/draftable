package services.impl

import com.google.inject.Inject
import java.time.{ Clock, ZonedDateTime }

import models._
import services._
import scala.util.Random

class TraditionalBoosterDraftRules @Inject() (
  packBuilder: PackBuilder,
  pickTimings: PickTimings,
  clock: Clock,
) extends DraftRules {
  import DraftRules._
  import TraditionalBoosterDraftRules._

  val maximumPlayerCount: Int = 8

  def createDraft: DraftState =
    DraftState(
      id = Identifier[DraftState](),
      players = Seq.empty,
      zones = Set.empty,
      turns = Map.empty,
      status = DraftState.Status.WaitingForPlayers,
    )

  def handleEvent(draft: DraftState, event: Event): Option[DraftRules.Update] =
    event match {
      case PlayerRegistered(newPlayer) =>
        draft.player(newPlayer) match {
          case Some(_) => None
          case None    => Some(draft.addPlayer(newPlayer).notify(everyone))
        }
      case PlayerNameChanged(playerId, newName) =>
        for {
          waiting <- draft.requireStatus(DraftState.Status.WaitingForPlayers)
        } yield {
          waiting
            .modifyPlayer(playerId)(_.rename(newName))
            .notify(everyone)
        }
      case ReadyToFire =>
        for {
          waiting <- draft.requireStatus(DraftState.Status.WaitingForPlayers)
          withZones = waiting.copy(zones = buildPlayerZones(waiting.players))
          pack2Zone <- withZones.zonesByName(r2BoosterZoneName).headOption
          pack3Zone <- withZones.zonesByName(r3BoosterZoneName).headOption
        } yield {
          val playerCount = withZones.players.size
          withZones
            .zonesByName(nextPacksZoneName)
            .foldLeft[DraftState] {
              withZones
                .modifyZone(pack2Zone)(_.copy(sets = Seq.fill(playerCount)(packBuilder.buildPack(2))))
                .modifyZone(pack3Zone)(_.copy(sets = Seq.fill(playerCount)(packBuilder.buildPack(3))))
            } { (acc, zone) =>
              acc.modifyZone(zone)(_.enqueueSet(packBuilder.buildPack(1)))
            }
            .setProperty("direction", "left")
            .updateStatus(DraftState.Status.InProgress)
            .notify(everyone)
        }
      case PoolsBuilt => None
      case CardSelected(player, card, (zone, set)) =>
        for {
          _ <- draft.requireStatus(DraftState.Status.InProgress)
          // Ensure that we only mark cards the player had permission to mark
          //  (only cards in their current pack)
          currentPackZone <- draft.zone(player, currentPackZoneName)
          if currentPackZone.id == zone
          currentPackSet <- currentPackZone.nextSet
          if currentPackSet.id == set
          _ <- currentPackSet.card(card)
        } yield {
          draft
            .modifyZone(zone)(
              _.modifySet(set)(currentBooster =>
                currentBooster
                  .modifyAllCards(_.clearAttributes())
                  .modifyCard(card)(_.addAttributes(Set(reservedAttribute)))
              )
            )
            .notify(player)
        }
      case CardActioned(player, card, (zoneId, setId)) =>
        for {
          _ <- draft.requireStatus(DraftState.Status.InProgress)
          zone <- draft.zone(zoneId)
          set <- zone.cardSet(setId)
          result <- set.setType match {
            case CardSet.Type.BoosterPack =>
              pickCardAndPassPack(
                draft = draft,
                player = player,
                card = card,
                from = (zone -> set),
                to = None,
              ).map(_.notify(everyone))
            case CardSet.Type.Deck =>
              privatelyMoveCard(
                draft = draft,
                player = player,
                card = card,
                playerZone = zone,
                fromSet = setId,
                toSetNamed = sideboardSetName,
              ).map(_.notify(player))
            case CardSet.Type.Sideboard =>
              privatelyMoveCard(
                draft = draft,
                player = player,
                card = card,
                playerZone = zone,
                fromSet = setId,
                toSetNamed = deckSetName,
              ).map(_.notify(player))
            case _ => None
          }
        } yield result
      case CardDragged(player, card, (fromZoneId, fromSetId), (toZoneId, toSetId)) =>
        for {
          _ <- draft.requireStatus(DraftState.Status.InProgress)
          fromZone <- draft.zone(fromZoneId)
          fromSet <- fromZone.cardSet(fromSetId)
          toZone <- draft.zone(toZoneId)
          if toZone.owner == Some(player.id) && toZone.canDrop == Zone.Target.Cards
          toSet <- toZone.cardSet(toSetId)
          result <- fromSet.setType match {
            case CardSet.Type.BoosterPack =>
              pickCardAndPassPack(
                draft = draft,
                player = player,
                card = card,
                from = (fromZone -> fromSet),
                to = Some(toZone -> toSet),
              ).map(_.notify(everyone))
            case CardSet.Type.Deck | CardSet.Type.Sideboard =>
              privatelyMoveCard(
                draft = draft,
                player = player,
                card = card,
                from = (fromZoneId -> fromSetId),
                to = (toZoneId -> toSetId),
              ).map(_.notify(player))
            case _ => None
          }
        } yield result
      case Tick =>
        for {
          _ <- draft.requireStatus(DraftState.Status.InProgress)
          // Loop through expired player turns and force picks
          withForcedPicks = {
            draft.turns
              .filter(_._2.isExpired(ZonedDateTime.now(clock)))
              .keys
              .foldLeft[DraftState](draft)(forcePick)
          }
          // If all current and next zones are empty:
          //   If there is a next round, copy its boosters into next zones
          //   If there is no next found, finalize the draft
          // Loop through players:
          //   If that player has an empty current pack and there is a pack in their next zone:
          //     Move the next set from their next pack zone
          //     Set their next turn
        } yield withForcedPicks.notify(everyone)
      case _ => None
    }

  private def buildPlayerZones(players: Seq[Identifiable[Player]]): Set[Zone] = {
    val privateZones = players.flatMap[Zone] { player =>
      Seq(
        Zone(
          id = Identifier[Zone](),
          name = nextPacksZoneName,
          owner = Some(player.id),
          sets = Seq.empty[CardSet],
          canSelect = Zone.Target.NoTarget,
          canDrop = Zone.Target.NoTarget,
          visible = players.map(_.id -> Zone.Visibility.Sets).toMap,
          defaultView = Zone.ViewType.Stacked,
        ),
        Zone(
          id = Identifier[Zone](),
          name = currentPackZoneName,
          owner = Some(player.id),
          sets = Seq.empty[CardSet],
          canSelect = Zone.Target.Cards,
          canDrop = Zone.Target.NoTarget,
          visible =
            players
              .filterNot(_ == player)
              .map(_.id -> Zone.Visibility.Sets)
              .toMap
              + (player.id -> Zone.Visibility.Cards),
          defaultView = Zone.ViewType.Spread,
        ),
        Zone(
          id = Identifier[Zone](),
          name = picksZoneName,
          owner = Some(player.id),
          sets = Seq(
            CardSet(
              id = Identifier[CardSet](),
              setType = CardSet.Type.Deck,
              name = deckSetName,
              cards = Seq.empty[CardInstance],
            ),
            CardSet(
              id = Identifier[CardSet](),
              setType = CardSet.Type.Sideboard,
              name = sideboardSetName,
              cards = Seq.empty[CardInstance],
            ),
          ),
          canSelect = Zone.Target.Cards,
          canDrop = Zone.Target.Cards,
          visible = Map(player.id -> Zone.Visibility.Cards),
          defaultView = Zone.ViewType.Columns,
        ),
      )
    }
    val sharedZones = Set(
      Zone(
        id = Identifier[Zone](),
        name = r2BoosterZoneName,
        owner = None,
        sets = Seq.empty[CardSet],
        canSelect = Zone.Target.NoTarget,
        canDrop = Zone.Target.NoTarget,
        visible = Map.empty,
        defaultView = Zone.ViewType.Stacked,
      ),
      Zone(
        id = Identifier[Zone](),
        name = r3BoosterZoneName,
        owner = None,
        sets = Seq.empty[CardSet],
        canSelect = Zone.Target.NoTarget,
        canDrop = Zone.Target.NoTarget,
        visible = Map.empty,
        defaultView = Zone.ViewType.Stacked,
      ),
    )

    sharedZones ++ privateZones
  }

  private def privatelyMoveCard(
    draft: DraftState,
    player: Identifiable[Player],
    card: Identifiable[CardInstance],
    playerZone: Zone,
    fromSet: Identifiable[CardSet],
    toSetNamed: String,
  ): Option[DraftState] =
    for {
      toSet <- playerZone.cardSet(toSetNamed)
      moved <- draft.moveCard(
        card = card,
        player = player,
        from = (playerZone -> fromSet),
        to = (playerZone -> toSet),
      )
    } yield moved

  private def privatelyMoveCard(
    draft: DraftState,
    player: Identifiable[Player],
    card: Identifiable[CardInstance],
    from: (Identifiable[Zone], Identifiable[CardSet]),
    to: (Identifiable[Zone], Identifiable[CardSet]),
  ): Option[DraftState] =
    for {
      playerZone <- draft.zone(from._1)
      // Verify that the card being is being moved between sets in the player's picksZoneName zone
      if playerZone.owner == Some(
        player.id
      ) && playerZone.name == picksZoneName && from._1.id == to._1.id
      moved <- draft.moveCard(
        card = card,
        player = player,
        from = from,
        to = to,
      )
    } yield moved

  private def pickCardAndPassPack(
    draft: DraftState,
    player: Identifiable[Player],
    card: Identifiable[CardInstance],
    from: (Zone, CardSet),
    to: Option[(Zone, CardSet)],
  ): Option[DraftState] =
    for {
      currentPackZone <- draft.zone(owner = player, name = currentPackZoneName)
      (fromZone, fromSet) = from
      if fromZone.id == currentPackZone.id // Ensure we're picking from the current pack
      (toZone, toSet) <- to orElse (
        for {
          picks <- draft.zone(owner = player, name = picksZoneName)
          deck <- picks.cardSet(deckSetName)
        } yield (picks -> deck)
      )
      if toZone.name == picksZoneName // Ensure we're moving the card into the player's picks
      directionStr <- draft.property("direction")
      nextPlayer <- draft.nextPlayer(
        player,
        direction = directionStr match {
          case "left"  => 1
          case "right" => -1
          case _       => 0
        },
      )
      nextPacksForNextPlayer <- draft.zone(owner = nextPlayer, name = nextPacksZoneName)
      // Move the chosen card from the current pack to the picks zone
      pickMoved <- draft.moveCard(
        card = card,
        player = player,
        from = from,
        to = (toZone -> toSet),
      )
      // Pass the pack
      updatedBooster <- pickMoved.cardSet(from)
      packPassed <- {
        if (updatedBooster.isEmpty)
          pickMoved.removeSet(
            set = updatedBooster,
            player = Player.System,
            from = fromZone,
          )
        else
          pickMoved.moveSet(
            set = updatedBooster,
            player = Player.System,
            from = fromZone,
            to = toZone,
          )
      }
      // Clear the timeout on the current player's turn
      result <- packPassed.clearTurnTimeout(player)
    } yield result

  def forcePick(draft: DraftState, player: Identifiable[Player]): DraftState = {
    val forcePick = for {
      currentPackZone <- draft.zone(owner = player, name = currentPackZoneName)
      if !currentPackZone.isEmpty
      currentBooster <- currentPackZone.nextSet
      pick <- {
        // Get a reserved card, or pick randomly if there is no reserved card
        currentBooster
          .cardsByAttribute(reservedAttribute)
          .headOption
          .orElse {
            val allCards = currentBooster.cards
            if (allCards.isEmpty)
              None
            else
              Some(allCards(Random.nextInt(allCards.length)))
          }
      }
      picked <- pickCardAndPassPack(
        draft = draft,
        player = player,
        card = pick,
        from = (currentPackZone -> currentBooster),
        to = None,
      )
    } yield picked

    val forcePickIfAble = forcePick.getOrElse(draft)

    forcePickIfAble
      .clearTurnTimeout(player)
      .getOrElse(forcePickIfAble)
  }

}

object TraditionalBoosterDraftRules {
  val r2BoosterZoneName: String = "Round 2 Boosters"
  val r3BoosterZoneName: String = "Round 3 Boosters"
  val currentPackZoneName: String = "Current Pack"
  val nextPacksZoneName: String = "Next Packs"
  val picksZoneName: String = "Picks"
  val deckSetName: String = "Deck"
  val sideboardSetName: String = "Sideboard"
  val reservedAttribute: String = "reserved"
}
