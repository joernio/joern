package io.joern.solidity2cpg.domain
import io.joern.solidity2cpg.domain.SuryaObject._
import org.slf4j.LoggerFactory
import spray.json.{DefaultJsonProtocol, JsNull, JsValue, JsonFormat}

/** Manually decodes Surya generated JSON objects to their assigned case classes. For more information see:
  * @see
  *   https://github.com/spray/spray-json
  */
object SuryaJsonProtocol extends DefaultJsonProtocol {
  private val logger = LoggerFactory.getLogger(SuryaJsonProtocol.getClass)

  /** Given a base AST node, will decide which class it belongs to and decode it. If the type is not supported a
    * [[BaseASTNode]] containing the type will be returned.
    */
  implicit object BaseASTNodeJsonFormat extends JsonFormat[BaseASTNode] with DefaultJsonProtocol {
    def write(c: BaseASTNode): JsValue = JsNull

    /** Parses the JSON value and determines which [[BaseASTNode]] class this is. Whenever a class references nested
      * [[BaseASTNode]]s then it needs it's own JSON format e.g. [[SourceUnitJsonFormat]] or
      * [[ContractDefinitionJsonFormat]].
      * @param json
      *   the JSON value to parse.
      * @return
      *   a specific [[SuryaObject]] case class or a [[BaseASTNode]] if the class is unhandled.
      */
    def read(json: JsValue): BaseASTNode = {
      val fields = json.asJsObject("BaseASTNode object expected").fields
      val typ    = fields("type").convertTo[String]
      typ match {
        case "PragmaDirective" =>
          PragmaDirective(fields("name").convertTo[String], fields("value").convertTo[String])
        case "ImportDirective" =>
          ImportDirective(fields("path").convertTo[String])
        case "ContractDefinition" =>
          ContractDefinitionJsonFormat.read(json)
        case _ =>
          logger.warn(s"Unhandled type '$typ' parsed from JSON AST.");
          new BaseASTNode(`type` = fields("type").convertTo[String])
      }
    }
  }

  implicit object SourceUnitJsonFormat extends JsonFormat[SourceUnit] with DefaultJsonProtocol {

    def write(c: SourceUnit): JsValue = JsNull

    def read(json: JsValue): SourceUnit = {
      val fields = json.asJsObject("Unable to decode JSON as SourceUnit").fields
      if (fields("type").convertTo[String] != "SourceUnit") {
        throw new RuntimeException("SourceUnit object expected")
      } else {
        SourceUnit(children = fields("children").convertTo[Map[String, BaseASTNode]])
      }
    }
  }

  implicit object ContractDefinitionJsonFormat extends JsonFormat[ContractDefinition] with DefaultJsonProtocol {

    def write(c: ContractDefinition): JsValue = JsNull

    def read(json: JsValue): ContractDefinition = {
      val fields = json.asJsObject("Unable to decode JSON as ContractDefinition").fields
      if (fields("type").convertTo[String] != "ContractDefinition") {
        throw new RuntimeException("ContractDefinition object expected")
      } else {
        ContractDefinition(
          fields("name").convertTo[String],
          fields("baseContracts").convertTo[Map[String, BaseASTNode]],
          fields("subNodes").convertTo[Map[String, BaseASTNode]],
          fields("kind").convertTo[String]
        )
      }
    }
  }
}
