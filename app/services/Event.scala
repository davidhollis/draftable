package services

import play.api.libs.json._
import play.api.libs.functional.syntax._

import models._

sealed trait Event

sealed abstract class EventType(val tag: String) {

  def verifyTag: Reads[String] =
    readField[String](EventType.key).filter(_ == tag)

  def applyTag(implicit jsonConfig: JsonConfiguration): OWrites[JsObject] =
    OWrites { _ + (jsonConfig.naming(EventType.key) -> JsString(tag)) }

}

object EventType {
  val key: String = "eventType"
}

case class PlayerRegistered(
  player: Player
) extends Event

object PlayerRegistered extends EventType("player registered") {

  implicit val reads: Reads[PlayerRegistered] = (
    verifyTag.andKeep(
      readField[Player]("player").map(PlayerRegistered.apply)
    )
  )

  implicit val writes: Writes[PlayerRegistered] =
    writeField[Player]("player")
      .contramap(unlift(PlayerRegistered.unapply))
      .transform(applyTag)

}

case class PlayerNameChanged(
  playerId: Identifier[Player],
  newName: String,
) extends Event

object PlayerNameChanged extends EventType("player name changed") {

  implicit val reads: Reads[PlayerNameChanged] = (
    verifyTag.andKeep(
      (
        readField[Identifier[Player]]("playerId") and
          readField[String]("newName")
      )(PlayerNameChanged.apply _)
    )
  )

  implicit val writes: Writes[PlayerNameChanged] = (
    writeField[Identifier[Player]]("playerId") and
      writeField[String]("newName")
  )(unlift(PlayerNameChanged.unapply)).transform(applyTag)

}

case object ReadyToFire extends EventType("ready to fire") with Event {
  implicit val reads: Reads[ReadyToFire.type] = verifyTag.andKeep(Reads.pure(ReadyToFire))

  implicit val writes: Writes[ReadyToFire.type] =
    OWrites[ReadyToFire.type] { _ => JsObject.empty }.transform(applyTag)

}

case object PoolsBuilt extends EventType("pools built") with Event {
  implicit val reads: Reads[PoolsBuilt.type] = verifyTag.andKeep(Reads.pure(PoolsBuilt))

  implicit val writes: Writes[PoolsBuilt.type] =
    OWrites[PoolsBuilt.type] { _ => JsObject.empty }.transform(applyTag)

}

case class CardSelected(
  player: Identifier[Player],
  card: Identifier[CardInstance],
  in: (Identifier[Zone], Identifier[CardSet]),
) extends Event

object CardSelected extends EventType("card selected") {

  implicit val reads: Reads[CardSelected] = (
    verifyTag.andKeep(
      (
        readField[Identifier[Player]]("player") and
          readField[Identifier[CardInstance]]("card") and
          readField[(Identifier[Zone], Identifier[CardSet])]("in")
      )(CardSelected.apply _)
    )
  )

  implicit val writes: Writes[CardSelected] = (
    writeField[Identifier[Player]]("player") and
      writeField[Identifier[CardInstance]]("card") and
      writeField[(Identifier[Zone], Identifier[CardSet])]("in")
  )(unlift(CardSelected.unapply)).transform(applyTag)

}

case class CardActioned(
  player: Identifier[Player],
  card: Identifier[CardInstance],
  in: (Identifier[Zone], Identifier[CardSet]),
) extends Event

object CardActioned extends EventType("card actioned") {

  implicit val reads: Reads[CardActioned] = (
    verifyTag.andKeep(
      (
        readField[Identifier[Player]]("player") and
          readField[Identifier[CardInstance]]("card") and
          readField[(Identifier[Zone], Identifier[CardSet])]("in")
      )(CardActioned.apply _)
    )
  )

  implicit val writes: Writes[CardActioned] = (
    writeField[Identifier[Player]]("player") and
      writeField[Identifier[CardInstance]]("card") and
      writeField[(Identifier[Zone], Identifier[CardSet])]("in")
  )(unlift(CardActioned.unapply)).transform(applyTag)

}

case class CardDragged(
  player: Identifier[Player],
  card: Identifier[CardInstance],
  from: (Identifier[Zone], Identifier[CardSet]),
  to: (Identifier[Zone], Identifier[CardSet]),
) extends Event

object CardDragged extends EventType("card dragged") {

  implicit val reads: Reads[CardDragged] = (
    verifyTag.andKeep(
      (
        readField[Identifier[Player]]("player") and
          readField[Identifier[CardInstance]]("card") and
          readField[(Identifier[Zone], Identifier[CardSet])]("from") and
          readField[(Identifier[Zone], Identifier[CardSet])]("to")
      )(CardDragged.apply _)
    )
  )

  implicit val writes: Writes[CardDragged] = (
    writeField[Identifier[Player]]("player") and
      writeField[Identifier[CardInstance]]("card") and
      writeField[(Identifier[Zone], Identifier[CardSet])]("from") and
      writeField[(Identifier[Zone], Identifier[CardSet])]("to")
  )(unlift(CardDragged.unapply)).transform(applyTag)

}

case class SetSelected(
  player: Identifier[Player],
  set: Identifier[CardSet],
  in: Identifier[Zone],
) extends Event

object SetSelected extends EventType("set selected") {

  implicit val reads: Reads[SetSelected] = (
    verifyTag.andKeep(
      (
        readField[Identifier[Player]]("player") and
          readField[Identifier[CardSet]]("set") and
          readField[Identifier[Zone]]("in")
      )(SetSelected.apply _)
    )
  )

  implicit val writes: Writes[SetSelected] = (
    writeField[Identifier[Player]]("player") and
      writeField[Identifier[CardSet]]("set") and
      writeField[Identifier[Zone]]("in")
  )(unlift(SetSelected.unapply)).transform(applyTag)

}

case class SetActioned(
  player: Identifier[Player],
  set: Identifier[CardSet],
  in: Identifier[Zone],
) extends Event

object SetActioned extends EventType("set actioned") {

  implicit val reads: Reads[SetActioned] = (
    verifyTag.andKeep(
      (
        readField[Identifier[Player]]("player") and
          readField[Identifier[CardSet]]("set") and
          readField[Identifier[Zone]]("in")
      )(SetActioned.apply _)
    )
  )

  implicit val writes: Writes[SetActioned] = (
    writeField[Identifier[Player]]("player") and
      writeField[Identifier[CardSet]]("set") and
      writeField[Identifier[Zone]]("in")
  )(unlift(SetActioned.unapply)).transform(applyTag)

}

case class SetDragged(
  player: Identifier[Player],
  set: Identifier[CardSet],
  from: Identifier[Zone],
  to: Identifier[Zone],
) extends Event

object SetDragged extends EventType("set dragged") {

  implicit val reads: Reads[SetDragged] = (
    verifyTag.andKeep(
      (
        readField[Identifier[Player]]("player") and
          readField[Identifier[CardSet]]("set") and
          readField[Identifier[Zone]]("from") and
          readField[Identifier[Zone]]("to")
      )(SetDragged.apply _)
    )
  )

  implicit val writes: Writes[SetDragged] = (
    writeField[Identifier[Player]]("player") and
      writeField[Identifier[CardSet]]("set") and
      writeField[Identifier[Zone]]("from") and
      writeField[Identifier[Zone]]("to")
  )(unlift(SetDragged.unapply)).transform(applyTag)

}

object Event {

  implicit val reads: Reads[Event] =
    Seq[Reads[Event]](
      PlayerRegistered.reads.widen[Event],
      PlayerNameChanged.reads.widen[Event],
      ReadyToFire.reads.widen[Event],
      PoolsBuilt.reads.widen[Event],
      CardSelected.reads.widen[Event],
      CardActioned.reads.widen[Event],
      CardDragged.reads.widen[Event],
      SetSelected.reads.widen[Event],
      SetActioned.reads.widen[Event],
      SetDragged.reads.widen[Event],
    ).foldRight[Reads[Event]](Reads.failed[Event]("No matching event type found"))(_ orElse _)

  implicit val writes: Writes[Event] = Writes {
    case pr: PlayerRegistered   => PlayerRegistered.writes.writes(pr)
    case pnc: PlayerNameChanged => PlayerNameChanged.writes.writes(pnc)
    case ReadyToFire            => ReadyToFire.writes.writes(ReadyToFire)
    case PoolsBuilt             => PoolsBuilt.writes.writes(PoolsBuilt)
    case cs: CardSelected       => CardSelected.writes.writes(cs)
    case ca: CardActioned       => CardActioned.writes.writes(ca)
    case cd: CardDragged        => CardDragged.writes.writes(cd)
    case ss: SetSelected        => SetSelected.writes.writes(ss)
    case sa: SetActioned        => SetActioned.writes.writes(sa)
    case sd: SetDragged         => SetDragged.writes.writes(sd)
  }

}
