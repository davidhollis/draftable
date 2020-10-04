package services

import models.{ DraftState, Identifier, Player }

trait DraftRules {
  type Event

  val maximumPlayerCount: Int

  def createDraft: DraftState

  def handleEvent(draft: DraftState, event: Event): (DraftState, Set[Identifier[Player]])
}
