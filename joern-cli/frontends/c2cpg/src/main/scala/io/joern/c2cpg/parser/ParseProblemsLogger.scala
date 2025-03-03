package io.joern.c2cpg.parser

import org.eclipse.cdt.core.dom.ast.IASTProblem
import org.slf4j.LoggerFactory

trait ParseProblemsLogger {

  this: CdtParser =>

  private val logger = LoggerFactory.getLogger(classOf[ParseProblemsLogger])

  protected def logProblems(problems: List[IASTProblem]): Unit = {
    problems.foreach(logProblemNode)
  }

  private def logProblemNode(node: IASTProblem): Unit = {
    val text = s"""Parse problem '${node.getClass.getSimpleName}' occurred!
                  |  Code: '${node.getRawSignature}'
                  |  File: '${node.getFileLocation.getFileName}'
                  |  Line: ${node.getFileLocation.getStartingLineNumber}
                  |  """.stripMargin
    logger.info(text)
  }

  /** The exception message might be null for parse failures due to ambiguous nodes that can't be resolved successfully.
    * We extract better log messages for this case here.
    */
  protected def extractParseException(exception: Throwable): String = Option(exception.getMessage) match {
    case Some(message) => message
    case None =>
      exception.getStackTrace
        .collectFirst {
          case stackTraceElement: StackTraceElement if stackTraceElement.getClassName.endsWith("ASTAmbiguousNode") =>
            "Could not resolve ambiguous node!"
        }
        .getOrElse(exception.getStackTrace.mkString(System.lineSeparator()))
  }

}
