package io.joern.solidity2cpg.domain
import io.circe.generic.codec.DerivedAsObjectCodec.deriveCodec
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, DecodingFailure, HCursor, Json, JsonObject}
import io.joern.solidity2cpg.domain.SuryaObject.{ImportDirective, PragmaDirective, SourceUnit}
import org.slf4j.LoggerFactory

/** Manually decodes Surya generated JSON objects to their assigned case classes. For more information see:
  * @see https://circe.github.io/circe/codecs/custom-codecs.html
  * @see https://www.baeldung.com/scala/circe-json
  */
class JsonDecoder {

  private val logger = LoggerFactory.getLogger(classOf[JsonDecoder])

  /** Using the "type" property on the JSON objects, determines which class to decode the object to.
    * @param o
    *   The Circe JsonObject to decode
    * @return
    *   Either Some(SuryaObject) on success or None if the object is not a handled type or some other exception.
    */
  def decodeJsonToSuryaObject(o: JsonObject): Option[SuryaObject] = {
    val maybeType = o("type")
    if (maybeType.isEmpty) {
      logger.warn(s"Given JSON object $o does not contain \"type\" property.")
      return None
    }

    def unboxDecode[T](v: Either[DecodingFailure, T]): Option[T] = {
      v match {
        case Left(e)  => logger.error(s"Unable to decode ${classOf[T]}", e); None
        case Right(v) => Some(v)
      }
    }

    val t = maybeType.get
    if (t.isString) {
      t match {
        case "SourceUnit" => unboxDecode(o.asJson.as[SourceUnit])
        case "PragmaDirective" => unboxDecode(o.asJson.as[PragmaDirective])
        case "ImportDirective" => unboxDecode(o.asJson.as[ImportDirective])
        case _ => logger.warn(s"Unhandled type: $t"); None
      }
    } else {
      None
    }
  }

  implicit val decodeSourceUnit: Decoder[SourceUnit] = (c: HCursor) =>
    for {
      children <- c.downField("children").as[List[Json]]
    } yield {
      SourceUnit(children.map(_.asJsonObject).flatMap(decodeJsonToSuryaObject))
    }

  implicit val decodePragmaDirective: Decoder[PragmaDirective] = (c: HCursor) =>
    for {
      name  <- c.downField("name").as[String]
      value <- c.downField("value").as[String]
    } yield {
      PragmaDirective(name, value)
    }

  implicit val decodeImportDirective: Decoder[ImportDirective] = (c: HCursor) =>
    for {
      path <- c.downField("path").as[String]
    } yield {
      ImportDirective(path)
    }
}
