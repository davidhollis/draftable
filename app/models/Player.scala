package models

import play.api.libs.json._
import play.api.libs.functional.syntax._

case class Player(
  id: Identifier[Player],
  name: Option[String],
) extends Identifiable[Player] {
  def rename(newName: String): Player = copy(name = name orElse Some(newName))
}

object Player {
  implicit val idPrefix: IdPrefix[Player] = IdPrefix[Player]("player")

  implicit val format: Format[Player] = (
    (__ \ implicitly[JsonConfiguration].naming("id")).format[Identifier[Player]] and
      (__ \ implicitly[JsonConfiguration].naming("name")).formatNullable[String]
  )(Player.apply, unlift(Player.unapply))

  case object System extends Identifiable[Player] {

    val id: Identifier[Player] =
      Identifier.applyOpt[Player]("00000000-0000-0000-0000-000000000000").get

  }

}
