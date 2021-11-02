package io.shiftleft.c2cpg.parser

import org.eclipse.cdt.core.dom.ast.IASTProblem
import org.slf4j.LoggerFactory

trait ParseProblemsLogger {

  this: CdtParser =>

  private val logger = LoggerFactory.getLogger(classOf[ParseProblemsLogger])

  private def logProblemNode(node: IASTProblem): Unit = {
    val text = s"""Parse problem '${node.getClass.getSimpleName}' occurred!
                  |  Code: '${node.getRawSignature}'
                  |  File: '${node.getFileLocation.getFileName}'
                  |  Line: ${node.getFileLocation.getStartingLineNumber}
                  |  """.stripMargin
    logger.info(text)
  }

  protected def logProblems(problems: List[IASTProblem]): Unit = {
    problems.foreach(logProblemNode)
  }

}
