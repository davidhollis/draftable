package services

import models.{ DraftState, Identifier, Player }

trait DraftRules {
  val maximumPlayerCount: Int

  def createDraft: DraftState

  def handleEvent(draft: DraftState, event: Event): (DraftState, Seq[Notification])
}

object DraftRules {

  implicit class NotifyPlayer(private val player: Identifier[Player]) extends AnyVal {
    def ~>(message: Message): Notification = Notification(player, message)

    def ~>(draft: DraftState): Notification =
      Notification(player, UpdateDraftState(draft.filteredViewFor(player)))

  }

}
