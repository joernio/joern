package io.joern.ghidra2cpg.utils

import ghidra.util.{DefaultErrorLogger, Msg}
import org.apache.logging.log4j.{LogManager, Logger}

/** Ghidra logs through its own `ghidra.util.Msg` facility (not slf4j), whose default logger prints everything from INFO
  * upwards to stdout/stderr. That buries the actual output under hundreds of lines of progress messages (SSL
  * initialization, loader details, analysis timing tables, ...).
  *
  * This logger keeps warnings and errors on the console like the default one, but forwards the chatty levels to log4j
  * instead (info is demoted to debug). Those are silent with the default WARN root level and can be enabled when
  * needed: in tests via `src/test/resources/log4j2-test.xml`, for the CLI via the `SL_LOGGING_LEVEL` env var.
  *
  * Additionally, warnings from ghidra's decompiler are demoted to debug as well: it emits one warning per function it
  * cannot fully decompile (e.g. calls into EXTERNAL blocks), which is expected for typical binaries and otherwise
  * floods the output.
  */
private class WarnAndUpErrorLogger extends DefaultErrorLogger {

  // Same originator resolution as ghidra's own Log4jErrorLogger
  private def loggerFor(originator: Any): Logger = originator match {
    case null            => LogManager.getLogger("(null)")
    case clazz: Class[?] => LogManager.getLogger(clazz)
    case other           => LogManager.getLogger(other.getClass)
  }

  private def isDecompilerDiagnostics(originator: Any): Boolean = {
    val clazz = originator match {
      case clazz: Class[?] => clazz
      case null            => return false
      case other           => other.getClass
    }
    clazz.getPackageName == "ghidra.app.decompiler"
  }

  override def trace(originator: Any, message: Any): Unit =
    loggerFor(originator).trace(message)

  override def trace(originator: Any, message: Any, throwable: Throwable): Unit =
    loggerFor(originator).trace(message, throwable)

  override def debug(originator: Any, message: Any): Unit =
    loggerFor(originator).debug(message)

  override def debug(originator: Any, message: Any, throwable: Throwable): Unit =
    loggerFor(originator).debug(message, throwable)

  override def info(originator: Any, message: Any): Unit =
    loggerFor(originator).debug(message)

  override def info(originator: Any, message: Any, throwable: Throwable): Unit =
    loggerFor(originator).debug(message, throwable)

  override def warn(originator: Any, message: Any): Unit =
    if (isDecompilerDiagnostics(originator)) loggerFor(originator).debug(message)
    else super.warn(originator, message)

  override def warn(originator: Any, message: Any, throwable: Throwable): Unit =
    if (isDecompilerDiagnostics(originator)) loggerFor(originator).debug(message, throwable)
    else super.warn(originator, message, throwable)
}

object WarnAndUpErrorLogger {

  /** Replaces Ghidra's global error logger with one that only prints warnings and errors. */
  def install(): Unit = Msg.setErrorLogger(new WarnAndUpErrorLogger)
}
