package io.joern.rubysrc2cpg.astcreation

import io.joern.rubysrc2cpg.parser.{RubyLexer, RubyParser}
import io.shiftleft.utils.IOUtils
import org.antlr.v4.runtime.*
import org.antlr.v4.runtime.atn.ATN
import org.antlr.v4.runtime.dfa.DFA
import org.slf4j.LoggerFactory

import java.nio.file.Paths
import scala.collection.mutable.ArrayBuffer
import scala.util.Try

/** A consumable wrapper for the RubyParser class used to parse the given file and be disposed thereafter. This includes
  * a "hacky" recovery of the parser when unsupported constructs are encountered by simply not parsing those lines.
  * @param filename
  *   the file path to the file to be parsed.
  */
class AntlrParser(filename: String) {

  private val logger      = LoggerFactory.getLogger(getClass)
  private val sourceLines = IOUtils.readLinesInFile(Paths.get(filename)).to(ArrayBuffer)
  var parser: RubyParser  = generateParser()

  private def generateParser(): RubyParser = {
    val inputString = sourceLines.mkString(System.lineSeparator)
    val charStream  = CharStreams.fromString(inputString)
    val lexer       = new RubyLexer(charStream)
    val tokenStream = new CommonTokenStream(lexer)
    val parser      = new RubyParser(tokenStream)
    parser.removeErrorListeners()
    parser.addErrorListener(ErroneousLineListener.INSTANCE)
    parser
  }

  def parse(): Try[RubyParser.ProgramContext] = {
    var result    = Try(parser.program())
    var lastError = ErroneousLineListener.INSTANCE.getLastErrorAndReset
    while (lastError != -1) {
      logger.warn(s"Erroneous line reported at $lastError, removing and re-parsing")
      sourceLines.remove(lastError - 1)
      sourceLines.insert(lastError - 1, "")
      parser = generateParser()
      result = Try(parser.program())
      lastError = ErroneousLineListener.INSTANCE.getLastErrorAndReset
    }
    result
  }

}

/** A re-usable parser object that clears the ANTLR DFA-cache if it determines that the memory usage is becoming large.
  * Once this parser is closed, the whole cache is evicted.
  *
  * This is done in this way since clearing the cache after each file is inefficient, since the cache must be re-built
  * every time, but the cache can become unnecessarily large at times. The cache also does not evict itself at the end
  * of parsing.
  *
  * @param clearLimit
  *   the percentage of used heap to clear the DFA-cache on.
  */
class ResourceManagedParser(clearLimit: Double) extends AutoCloseable {

  private val logger                                 = LoggerFactory.getLogger(getClass)
  private val runtime                                = Runtime.getRuntime
  private var maybeDecisionToDFA: Option[Array[DFA]] = None
  private var maybeAtn: Option[ATN]                  = None

  def parse(filename: String): Try[RubyParser.ProgramContext] = {
    val antlrParser = AntlrParser(filename)
    val interp      = antlrParser.parser.getInterpreter
    // We need to grab a live instance in order to get the static variables as they are protected from static access
    maybeDecisionToDFA = Option(interp.decisionToDFA)
    maybeAtn = Option(interp.atn)
    val usedMemory = runtime.freeMemory.toDouble / runtime.totalMemory.toDouble
    if (usedMemory >= clearLimit) {
      logger.info(s"Runtime memory consumption at $usedMemory, clearing ANTLR DFA cache")
      clearDFA()
    }
    antlrParser.parse()
  }

  /** Clears the shared DFA cache.
    */
  private def clearDFA(): Unit = if (maybeDecisionToDFA.isDefined && maybeAtn.isDefined) {
    val decisionToDFA = maybeDecisionToDFA.get
    val atn           = maybeAtn.get
    for (d <- decisionToDFA.indices) {
      decisionToDFA(d) = new DFA(atn.getDecisionState(d), d)
    }
  }

  override def close(): Unit = clearDFA()
}

private class ErroneousLineListener extends BaseErrorListener {

  private val logger         = LoggerFactory.getLogger(getClass)
  private var lastError: Int = -1

  def getLastErrorAndReset: Int = {
    val tmp = lastError
    lastError = -1
    tmp
  }

  override def syntaxError(
    recognizer: Recognizer[_, _],
    offendingSymbol: Any,
    line: Int,
    charPositionInLine: Int,
    msg: String,
    e: RecognitionException
  ): Unit = {
    val tokenString = offendingSymbol match
      case x: CommonToken => x.getText
      case x              => x.getClass

    lastError = line
    logger.warn(s"$msg. Offending symbol '$tokenString' at $line:$charPositionInLine.")
  }

}

object ErroneousLineListener {
  val INSTANCE: ErroneousLineListener = new ErroneousLineListener()
}
