package io.joern.c2cpg.parser

import org.eclipse.cdt.core.dom.ast.{
  IASTPreprocessorFunctionStyleMacroDefinition,
  IASTPreprocessorIfStatement,
  IASTPreprocessorIfdefStatement,
  IASTPreprocessorStatement,
  IASTTranslationUnit
}
import org.slf4j.LoggerFactory

trait PreprocessorStatementsLogger {

  this: CdtParser =>

  private val logger = LoggerFactory.getLogger(classOf[PreprocessorStatementsLogger])

  protected def logPreprocessorStatements(translationUnit: IASTTranslationUnit): Unit = {
    preprocessorStatements(translationUnit).foreach(logPreprocessorStatement)
  }

  private def logPreprocessorStatement(node: IASTPreprocessorStatement): Unit = {
    val text = s"""Preprocessor statement '${node.getClass.getSimpleName}' found!
                  |  Code: '${node.getRawSignature}'
                  |  File: '${node.getFileLocation.getFileName}'
                  |  Line: ${node.getFileLocation.getStartingLineNumber}""".stripMargin
    val additionalInfo = node match {
      case s: IASTPreprocessorFunctionStyleMacroDefinition =>
        s"""
           |  Parameter: ${s.getParameters.map(_.getRawSignature).mkString(", ")}
           |  Expansion: ${s.getExpansion}""".stripMargin
      case s: IASTPreprocessorIfStatement =>
        s"""
           |  Defined: ${s.taken()}""".stripMargin
      case s: IASTPreprocessorIfdefStatement =>
        s"""
           |  Defined: ${s.taken()}""".stripMargin
      case _ => ""
    }
    logger.info(s"$text$additionalInfo")
  }

  protected def preprocessorStatements(translationUnit: IASTTranslationUnit): Iterable[IASTPreprocessorStatement] = {
    translationUnit.getAllPreprocessorStatements
  }

}
