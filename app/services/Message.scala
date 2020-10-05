package services

import models._

trait Message

case class Error(message: String) extends Message

case class SetLayout(focusZones: Seq[Identifier[Zone]]) extends Message

case class UpdateDraftState(state: DraftState) extends Message

case class Notification(player: Identifier[Player], message: Message)
