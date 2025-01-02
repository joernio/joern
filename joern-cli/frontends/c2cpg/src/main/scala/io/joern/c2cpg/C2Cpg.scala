package io.joern.c2cpg

import io.joern.c2cpg.passes.{AstCreationPass, PreprocessorPass, TypeDeclNodePass}
import io.joern.c2cpg.passes.FunctionDeclNodePass
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.Languages
import io.joern.x2cpg.passes.frontend.{MetaDataPass, TypeNodePass}
import io.joern.x2cpg.X2Cpg.withNewEmptyCpg
import io.joern.x2cpg.X2CpgFrontend
import io.joern.x2cpg.utils.Report
import org.slf4j.LoggerFactory

import java.util.regex.Pattern
import scala.util.control.NonFatal
import scala.util.Try
import scala.util.matching.Regex

class C2Cpg extends X2CpgFrontend[Config] {

  private val logger = LoggerFactory.getLogger(classOf[C2Cpg])

  def createCpg(config: Config): Try[Cpg] = {
    withNewEmptyCpg(config.outputPath, config) { (cpg, config) =>
      val report = new Report()
      new MetaDataPass(cpg, Languages.NEWC, config.inputPath).createAndApply()
      val astCreationPass = new AstCreationPass(cpg, config, report)
      astCreationPass.createAndApply()
      new FunctionDeclNodePass(cpg, astCreationPass.unhandledMethodDeclarations())(config.schemaValidation)
        .createAndApply()
      TypeNodePass.withRegisteredTypes(astCreationPass.typesSeen(), cpg).createAndApply()
      new TypeDeclNodePass(cpg)(config.schemaValidation).createAndApply()
      report.print()
    }
  }

  def printIfDefsOnly(config: Config): Unit = {
    try {
      val stmts = new PreprocessorPass(config).run().mkString(",")
      println(stmts)
    } catch {
      case NonFatal(ex) =>
        logger.error("Failed to print preprocessor statements.", ex)
        throw ex
    }
  }

}

object C2Cpg {

  private val EscapedFileSeparator = Pattern.quote(java.io.File.separator)

  val DefaultIgnoredFolders: List[Regex] = List(
    s"(.*[$EscapedFileSeparator])?\\..*".r,
    s"(.*[$EscapedFileSeparator])?tests?[$EscapedFileSeparator].*".r,
    s"(.*[$EscapedFileSeparator])?CMakeFiles[$EscapedFileSeparator].*".r
  )

}
