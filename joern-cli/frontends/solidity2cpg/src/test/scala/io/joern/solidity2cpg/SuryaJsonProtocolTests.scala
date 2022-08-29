package io.joern.solidity2cpg

import io.shiftleft.utils.ProjectRoot
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.slf4j.LoggerFactory
import spray.json._
import spray.json.{DefaultJsonProtocol, JsBoolean, JsNull, JsString, JsValue, JsonFormat, JsArray}
import scala.util.{Failure, Success, Try, Using}

class SuryaJsonProtocolTests extends AnyWordSpec with Matchers with BeforeAndAfterAll {

  private val logger = LoggerFactory.getLogger(classOf[SuryaJsonProtocolTests])

  val sampleAstFile: String =
    ProjectRoot.relativise("joern-cli/frontends/solidity2cpg/src/test/resources/sample_contract_ast.json")
  private val code: String = Using.resource(scala.io.Source.fromFile(sampleAstFile)) { source =>
    source.getLines() mkString "\n"
  }

  "should be able to parse a solidity file to a json format file" in {
    val sampleSolFile: String =
      ProjectRoot.relativise("joern-cli/frontends/solidity2cpg/src/test/resources/Reentrancy.sol")
    val json = new Solidity2Cpg().getSourcesFromDir(sampleSolFile)

//    println(json._1)
//    println(json._2)
  }

// "should be able to decode a string JSON input to a root SourceUnit" in {
//   import io.joern.solidity2cpg.domain.SuryaJsonProtocol._
//   import io.joern.solidity2cpg.domain.SuryaObject._
//  val astRoot = Try(code.parseJson.convertTo[SourceUnit]) match {
//    case Failure(e)          => fail(s"Unable to convert JSON to SourceUnit due to an exception", e)
//    case Success(sourceUnit) => sourceUnit
//  }
// }

  // TODO: Write tests to spot check that important things are here in the AST ROOT
  "should be able to decode a string JSON input to a root PragmaDirective" in {
    import io.joern.solidity2cpg.domain.SuryaJsonProtocol._
    import io.joern.solidity2cpg.domain.SuryaObject._
    val newRoot = code.parseJson.convertTo[JsObject].fields("children").convertTo[JsArray];
    var counter = 0;

    while (counter < newRoot.elements.size) {
      try {
        val newPrag = newRoot.elements(counter);
        val astRoot = Try(newPrag.convertTo[PragmaDirective]) match {
          case Failure(e)               => fail(s"Unable to convert JSON to PragmaDirective due to an exception", e)
          case Success(pragmaDirective) => pragmaDirective
        }
        counter += 1
      } catch {
        case _ => counter += 1
      }
    }
  }

  "should be able to decode a string JSON input to a root ImportDirective" in {
    import io.joern.solidity2cpg.domain.SuryaJsonProtocol._
    import io.joern.solidity2cpg.domain.SuryaObject._
    val newRoot = code.parseJson.convertTo[JsObject].fields("children").convertTo[JsArray];
    var counter = 0;

    while (counter < newRoot.elements.size) {
      try {
        val newImport = newRoot.elements(counter);
        val astRoot = Try(newImport.convertTo[ImportDirective]) match {
          case Failure(e) => fail(s"Unable to convert JSON to ImportDirective due to an exception", e)
          case Success(pragmaImportDirective) => pragmaImportDirective
        }
        counter += 1
      } catch {
        case _ => counter += 1
      }
    }
  }

  "should be able to decode a string JSON input to a root InheritanceSpecifier" in {
    import io.joern.solidity2cpg.domain.SuryaJsonProtocol._
    import io.joern.solidity2cpg.domain.SuryaObject._
    val oldRoot = code.parseJson.convertTo[JsObject].fields("children").convertTo[JsArray];
    val newRoot = oldRoot.elements(3).convertTo[JsObject].fields("baseContracts").convertTo[JsArray]
    var counter = 0
    while (counter < newRoot.elements.size) {
      try {
        val newInheritanceSpecifier = newRoot.elements(counter);
        val astRoot = Try(newInheritanceSpecifier.convertTo[InheritanceSpecifier]) match {
          case Failure(e) => fail(s"Unable to convert JSON to ImportDirective due to an exception", e)
          case Success(inheritanceSpecifier) => inheritanceSpecifier
        }
        counter += 1
      } catch {
        case _ => counter += 1
      }
    }
  }

  "should be able to decode a string JSON input to a root ModifierDefinition" in {
    import io.joern.solidity2cpg.domain.SuryaJsonProtocol._
    import io.joern.solidity2cpg.domain.SuryaObject._
    val oldRoot = code.parseJson.convertTo[JsObject].fields("children").convertTo[JsArray];
    val newRoot = oldRoot.elements(3).convertTo[JsObject].fields("subNodes").convertTo[JsArray]
    var counter = 0
    while (counter < newRoot.elements.size) {
      try {
        val newModifierDefinition = newRoot.elements(counter);
        val astRoot = Try(newModifierDefinition.convertTo[ModifierDefinition]) match {
          case Failure(e) => fail(s"Unable to convert JSON to ModifierDefinition due to an exception", e)
          case Success(modifierDefinition) => modifierDefinition
        }
        counter += 1
      } catch {
        case _ => counter += 1
      }
    }
  }

// "should be able to decode a string JSON input to a root VariableDeclaration" in {
//   import io.joern.solidity2cpg.domain.SuryaJsonProtocol._
//   import io.joern.solidity2cpg.domain.SuryaObject._
//   val oldRoot =  code.parseJson.convertTo[JsObject].fields("children").convertTo[JsArray]
//   val newRoot = oldRoot.elements(3).convertTo[JsObject].fields("subNodes").convertTo[JsArray]
//   var counter = 0
//   while (counter < newRoot.elements.size) {
//     try {
//       var counter2 = 0;
//       val newVariable = newRoot.elements(counter).convertTo[JsObject].fields("parameters").convertTo[JsArray];
//       while (counter2 < newVariable.elements.size) {
//         val astRoot = Try(newVariable.elements(counter2).convertTo[VariableDeclaration]) match {
//           case Failure(e)          => fail(s"Unable to convert JSON to VariableDeclaration due to an exception", e)
//           case Success(variableDeclaration) => variableDeclaration
//         }
//         counter2 += 1
//       }
//       counter += 1
//     }
//
//   }
// }

}
