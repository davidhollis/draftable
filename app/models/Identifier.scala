package models

import java.util.UUID
import scala.util.Try
import scala.util.matching.Regex
import play.api.libs.json.{ JsError, JsString, JsSuccess, Reads, Writes }

abstract class IdPrefix[T] {
  val prefixString: String

  @inline
  override def toString(): String = prefixString

}

object IdPrefix {
  def apply[T](prefix: String): IdPrefix[T] = new IdPrefix[T] { val prefixString: String = prefix }

  def of[T: IdPrefix]: IdPrefix[T] = implicitly[IdPrefix[T]]
}

trait Identifiable[T] {
  val id: Identifier[T]
}

class Identifier[T] private[Identifier] (
  private val uuid: UUID
)(
  implicit
  private val prefix: IdPrefix[T]
) extends Identifiable[T] {
  lazy val id: Identifier[T] = this

  override lazy val toString: String = s"drn:${prefix}:${uuid}"

  override def equals(obj: Any): Boolean =
    obj match {
      case id: Identifier[_] => id.toString == this.toString
      case _                 => false
    }

}

object Identifier {
  val idRegex: Regex = """drn:(.+):([0-9a-f-]+)""".r.anchored
  def apply[T: IdPrefix](): Identifier[T] = new Identifier[T](UUID.randomUUID())

  def applyOpt[T: IdPrefix](idString: String): Option[Identifier[T]] =
    for {
      List(prefixString, uuidString) <- idRegex.unapplySeq(idString)
      if prefixString == IdPrefix.of[T].toString()
      uuid <- Try(UUID.fromString(uuidString)).toOption
    } yield new Identifier[T](uuid)

  def unapply[T](id: Identifier[T]): Option[String] = Some(id.toString)

  implicit def reads[T: IdPrefix]: Reads[Identifier[T]] =
    Reads {
      case JsString(str) =>
        Identifier.applyOpt(str) match {
          case Some(id) => JsSuccess(id)
          case None     => JsError("Malformed identifier string")
        }
      case _ => JsError("Malformed identifier: must be a string")
    }

  implicit def writes[T: IdPrefix]: Writes[Identifier[T]] = Writes { id => JsString(id.toString) }

}
