package models

import play.api.libs.json.{ JsString, JsSuccess, Reads, Writes }

sealed abstract class ColorProfile(override val toString: String)

object ColorProfile {
  object Colorless extends ColorProfile("colorless")
  object White extends ColorProfile("white")
  object Blue extends ColorProfile("blue")
  object Black extends ColorProfile("black")
  object Red extends ColorProfile("red")
  object Green extends ColorProfile("green")
  object Multicolor extends ColorProfile("multicolor")
  object Land extends ColorProfile("land")

  val all: Set[ColorProfile] = Set(Colorless, White, Blue, Black, Red, Green, Multicolor, Land)

  lazy val byName: Map[String, ColorProfile] =
    all.map(profile => (profile.toString -> profile)).toMap

  implicit val reads: Reads[ColorProfile] = Reads {
    case JsString(profileName) => JsSuccess(ColorProfile.byName.getOrElse(profileName, Colorless))
    case _                     => JsSuccess(Colorless)
  }

  implicit val writes: Writes[ColorProfile] = Writes { profile => JsString(profile.toString) }

}
