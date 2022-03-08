package io.joern.solidity2cpg

import io.shiftleft.utils.ProjectRoot
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.slf4j.LoggerFactory
import spray.json._

import scala.util.{Failure, Success, Try, Using}

class SuryaJsonProtocolTests extends AnyWordSpec with Matchers with BeforeAndAfterAll {

  private val logger = LoggerFactory.getLogger(classOf[SuryaJsonProtocolTests])

  val sampleAstFile: String =
    ProjectRoot.relativise("joern-cli/frontends/solidity2cpg/src/test/resources/sample_contract_ast.json")
  private val code: String = Using.resource(scala.io.Source.fromFile(sampleAstFile)) { source =>
    source.getLines() mkString "\n"
  }

  "should be able to decode a string JSON input to a root SourceUnit" in {
    import io.joern.solidity2cpg.domain.SuryaJsonProtocol._
    import io.joern.solidity2cpg.domain.SuryaObject._
    val astRoot = Try(code.parseJson.convertTo[SourceUnit]) match {
      case Failure(e)          => fail(s"Unable to convert JSON to SourceUnit due to an exception", e)
      case Success(sourceUnit) => sourceUnit
    }
    // TODO: Write tests to spot check that important things are here in the AST ROOT
    println(astRoot)
  }

}
