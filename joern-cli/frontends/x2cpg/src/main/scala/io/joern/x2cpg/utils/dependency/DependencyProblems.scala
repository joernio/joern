package io.joern.x2cpg.utils.dependency

import io.joern.x2cpg.report.{Category, ProblemSink}
import org.slf4j.Logger

private[dependency] object DependencyProblems {

  /** Logs the problem on `logger` and records it as a [[Category.DependencyResolution]] warning. */
  def warn(problems: ProblemSink, logger: Logger, message: String, cause: Option[Throwable] = None): Unit =
    problems.warn(Category.DependencyResolution, message, cause = cause, logger = logger)
}
