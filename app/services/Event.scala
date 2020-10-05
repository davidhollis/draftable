package services

import models._

sealed trait Event

case class PlayerRegistered(
  player: Player
) extends Event

case class PlayerNameChanged(
  playerId: Identifier[Player],
  newName: String,
)

case object ReadyToFire extends Event

case object PoolsBuilt extends Event

case class CardSelected(
  player: Identifier[Player],
  card: Identifier[CardInstance],
  in: (Identifier[Zone], Identifier[CardSet]),
) extends Event

case class CardActioned(
  player: Identifier[Player],
  card: Identifier[CardInstance],
  in: (Identifier[Zone], Identifier[CardSet]),
) extends Event

case class CardDragged(
  player: Identifier[Player],
  card: Identifier[CardInstance],
  from: (Identifier[Zone], Identifier[CardSet]),
  to: (Identifier[Zone], Identifier[CardSet]),
) extends Event

case class SetSelected(
  player: Identifier[Player],
  set: Identifier[CardSet],
  in: Identifier[Zone],
) extends Event

case class SetActioned(
  player: Identifier[Player],
  set: Identifier[CardSet],
  in: Identifier[Zone],
) extends Event

case class SetDragged(
  player: Identifier[Player],
  set: Identifier[CardSet],
  from: Identifier[Zone],
  to: Identifier[Zone],
) extends Event
