package models

import play.api.libs.json._
import play.api.libs.functional.syntax._

case class Player(
  id: Identifier[Player],
  name: Option[String],
) extends Identifiable[Player] {
  def rename(newName: String): Player = copy(name = Some(newName))
}

object Player {
  implicit val idPrefix: IdPrefix[Player] = IdPrefix[Player]("player")

  implicit val format: Format[Player] = (
    field[Identifier[Player]]("id") and
      optionalField[String]("name")
  )(Player.apply, unlift(Player.unapply))

  case object System extends Identifiable[Player] {

    val id: Identifier[Player] =
      Identifier.applyOpt[Player]("00000000-0000-0000-0000-000000000000").get

  }

}
