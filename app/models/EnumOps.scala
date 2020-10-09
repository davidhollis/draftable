package models

import play.api.libs.json._

trait Enum {
  val name: String

  @inline
  override def toString(): String = name

}

abstract class EnumOps[T <: Enum](val typeName: String) {
  val all: Set[T]

  lazy val byName: Map[String, T] = all.map(elem => (elem.name -> elem)).toMap

  val defaultValue: Option[T] = None

  implicit lazy val reads: Reads[T] =
    Reads {
      case JsString(elemName) =>
        byName.get(elemName).orElse(defaultValue) match {
          case Some(elem) => JsSuccess(elem)
          case None       => JsError(s"Invalid $typeName '$elemName'. Should be one of $all.")
        }
      case _ =>
        defaultValue match {
          case Some(elem) => JsSuccess(elem)
          case None       => JsError(s"Malformed $typeName value. Should be a string.")
        }
    }

  implicit lazy val writes: Writes[T] = Writes { elem => JsString(elem.name) }

}
