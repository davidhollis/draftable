package services

import models.{ DraftState, Identifiable, Player }

trait DraftRules {
  val maximumPlayerCount: Int

  def createDraft: DraftState

  def handleEvent(draft: DraftState, event: Event): Option[DraftRules.Update]
}

object DraftRules {

  case class Update(newState: DraftState, notifies: Seq[Notification]) {
    def notify(notification: Notification): Update = copy(notifies = notifies :+ notification)
    def notify(notifications: Seq[Notification]): Update = copy(notifies = notifies ++ notifications)
  }

  implicit class DraftStateArrow(private val draft: DraftState) extends AnyVal {

    @inline
    private def filterViews(players: Seq[Identifiable[Player]]): Seq[Notification] =
      players.map { player =>
        Notification(player.id, UpdateDraftState(draft.filteredViewFor(player)))
      }

    @inline
    def notify(player: Identifiable[Player]): Update =
      Update(draft, (this.filterViews(Seq(player))))

    @inline
    def notify(players: Seq[Identifiable[Player]]): Update =
      Update(draft, (this.filterViews(players)))

    @inline
    def notify(players: everyone.type): Update =
      Update(draft, (this.filterViews(draft.players)))

    @inline
    def notify(players: nobody.type): Update = Update(draft, Seq.empty)

    @inline
    def send(notification: Notification): Update =
      Update(draft, Seq(notification))

    @inline
    def send(notifications: Seq[Notification]): Update =
      Update(draft, notifications)

  }

  case object everyone
  case object nobody

}
