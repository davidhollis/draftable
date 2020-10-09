import play.api.libs.json._

package object models {
  implicit val jsonConfig: JsonConfiguration = JsonConfiguration(JsonNaming.SnakeCase)

  @inline
  def fieldPath(name: String)(implicit jsonConfig: JsonConfiguration): JsPath =
    (__ \ jsonConfig.naming(name))

  @inline
  def readField[T: Reads](name: String)(implicit jsonConfig: JsonConfiguration): Reads[T] =
    fieldPath(name).read[T]

  @inline
  def writeField[T: Writes](name: String)(implicit jsonConfig: JsonConfiguration): OWrites[T] =
    fieldPath(name).write[T]

  @inline
  def field[T: Format](name: String)(implicit jsonConfig: JsonConfiguration): OFormat[T] =
    fieldPath(name).format[T]

  @inline
  def optionalField[T: Format](
    name: String
  )(
    implicit
    jsonConfig: JsonConfiguration
  ): OFormat[Option[T]] =
    fieldPath(name).formatNullable[T]

}
