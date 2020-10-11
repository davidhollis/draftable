package services

import play.api.libs.json._
import play.api.libs.functional.syntax._

import models._

trait Message {

  def |->(player: Identifiable[Player]): Seq[Notification] =
    Seq(Notification(player.id, this))

  def |->(players: Seq[Identifiable[Player]]): Seq[Notification] =
    players.map { player => Notification(player.id, this) }

}

sealed abstract class MessageType(val tag: String) {
  def verifyTag: Reads[String] = readField[String](MessageType.key).filter(_ == tag)

  def applyTag(implicit jsonConfig: JsonConfiguration): OWrites[JsObject] =
    OWrites { _ + (jsonConfig.naming(MessageType.key) -> JsString(tag)) }

}

object MessageType {
  val key: String = "messageType"
}

case class Error(message: String) extends Message

object Error extends MessageType("error") {

  implicit val reads: Reads[Error] = (
    verifyTag.andKeep(
      readField[String]("message").map(Error.apply)
    )
  )

  implicit val writes: Writes[Error] =
    writeField[String]("message")
      .contramap(unlift(Error.unapply))
      .transform(applyTag)

}

case class SetLayout(focusZones: Seq[Identifier[Zone]]) extends Message

object SetLayout extends MessageType("set layout") {

  implicit val reads: Reads[SetLayout] = (
    verifyTag.andKeep(
      readField[Seq[Identifier[Zone]]]("focusZones").map(SetLayout.apply)
    )
  )

  implicit val writes: Writes[SetLayout] =
    writeField[Seq[Identifier[Zone]]]("focusZones")
      .contramap(unlift(SetLayout.unapply))
      .transform(applyTag)

}

case class UpdateDraftState(state: DraftState) extends Message

object UpdateDraftState extends MessageType("update draft state") {

  implicit val reads: Reads[UpdateDraftState] = (
    verifyTag.andKeep(
      readField[DraftState]("state").map(UpdateDraftState.apply)
    )
  )

  implicit val writes: Writes[UpdateDraftState] =
    writeField[DraftState]("state")
      .contramap(unlift(UpdateDraftState.unapply))
      .transform(applyTag)

}

object Message {

  implicit val reads: Reads[Message] =
    Seq[Reads[Message]](
      Error.reads.widen[Message],
      SetLayout.reads.widen[Message],
      UpdateDraftState.reads.widen[Message],
    ).foldRight[Reads[Message]](Reads.failed("No matching message type found"))(_ orElse _)

  implicit val writes: Writes[Message] = Writes {
    case e: Error              => Error.writes.writes(e)
    case sl: SetLayout         => SetLayout.writes.writes(sl)
    case uds: UpdateDraftState => UpdateDraftState.writes.writes(uds)
  }

}

case class Notification(player: Identifier[Player], message: Message)
