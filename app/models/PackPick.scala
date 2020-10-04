package models

import java.time.Duration

case class PackPick(
  pack: Int,
  pick: Int,
  packSize: Int,
) {

  def next: PackPick =
    if (pick == packSize)
      copy(pack = pack + 1, pick = 1)
    else
      copy(pick = pick + 1)

  def timeout(implicit timings: PickTimings): Option[Duration] = timings.timeoutFor(pack, pick)
}

trait PickTimings {
  def timeoutFor(pack: Int, pick: Int): Option[Duration]
}

object PickTimings {

  val noTimeLimit: PickTimings = new PickTimings {
    def timeoutFor(pack: Int, pick: Int): Option[Duration] = None
  }

  val dayPerPick: PickTimings = new PickTimings {
    def timeoutFor(pack: Int, pick: Int): Option[Duration] = Some(Duration.ofDays(1))
  }

  val doublePaper: PickTimings = new PickTimings {

    def timeoutFor(pack: Int, pick: Int): Option[Duration] =
      pick match {
        case 1 | 2             => Some(Duration.ofSeconds(80))
        case 3                 => Some(Duration.ofSeconds(70))
        case 4                 => Some(Duration.ofSeconds(60))
        case 5 | 6             => Some(Duration.ofSeconds(50))
        case 7 | 8             => Some(Duration.ofSeconds(40))
        case 9                 => Some(Duration.ofSeconds(30))
        case 10 | 11           => Some(Duration.ofSeconds(20))
        case 12 | 13 | 14 | 15 => Some(Duration.ofSeconds(10))
        case _                 => None
      }

  }

}
