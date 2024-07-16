package io.joern.rubysrc2cpg.parser

import better.files.File
import org.antlr.v4.runtime.*
import org.antlr.v4.runtime.atn.{ATN, ATNConfigSet}
import org.antlr.v4.runtime.dfa.DFA
import org.slf4j.LoggerFactory

import java.io.File.separator
import java.util
import scala.collection.mutable.ListBuffer
import scala.util.Try

/** A consumable wrapper for the RubyParser class used to parse the given file and be disposed thereafter.
  * @param inputDir
  *   the directory of the target to parse.
  * @param filename
  *   the file path to the file to be parsed.
  */
class AntlrParser(inputDir: File, filename: String) {

  private val charStream = CharStreams.fromFileName(filename)
  private val lexer      = new RubyLexer(charStream)

  private val tokenStream = new CommonTokenStream(RubyLexerPostProcessor(lexer))
  val parser: RubyParser  = new RubyParser(tokenStream)

  def parse(): (Try[RubyParser.ProgramContext], List[String]) = {
    val errors = ListBuffer[String]()
    parser.removeErrorListeners()
    parser.addErrorListener(new ANTLRErrorListener {
      override def syntaxError(
        recognizer: Recognizer[?, ?],
        offendingSymbol: Any,
        line: Int,
        charPositionInLine: Int,
        msg: String,
        e: RecognitionException
      ): Unit = {
        val errorMessage =
          s"Syntax error on ${filename.stripPrefix(s"${inputDir.pathAsString}$separator")}:$line:$charPositionInLine"
        errors.append(errorMessage)
      }

      override def reportAmbiguity(
        recognizer: Parser,
        dfa: DFA,
        startIndex: Int,
        stopIndex: Int,
        exact: Boolean,
        ambigAlts: util.BitSet,
        configs: ATNConfigSet
      ): Unit = {}

      override def reportAttemptingFullContext(
        recognizer: Parser,
        dfa: DFA,
        startIndex: Int,
        stopIndex: Int,
        conflictingAlts: util.BitSet,
        configs: ATNConfigSet
      ): Unit = {}

      override def reportContextSensitivity(
        recognizer: Parser,
        dfa: DFA,
        startIndex: Int,
        stopIndex: Int,
        prediction: Int,
        configs: ATNConfigSet
      ): Unit = {}
    })
    (Try(parser.program()), errors.toList)
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
class ResourceManagedParser(clearLimit: Double, debug: Boolean = false) extends AutoCloseable {

  private val logger                                 = LoggerFactory.getLogger(getClass)
  private val runtime                                = Runtime.getRuntime
  private var maybeDecisionToDFA: Option[Array[DFA]] = None
  private var maybeAtn: Option[ATN]                  = None

  def parse(inputFile: File, filename: String): Try[RubyParser.ProgramContext] = {
    val inputDir    = if inputFile.isDirectory then inputFile else inputFile.parent
    val antlrParser = AntlrParser(inputDir, filename)
    antlrParser.parser.setTrace(debug) // enables printing of ANTLR parse tree
    val interp = antlrParser.parser.getInterpreter
    // We need to grab a live instance in order to get the static variables as they are protected from static access
    maybeDecisionToDFA = Option(interp.decisionToDFA)
    maybeAtn = Option(interp.atn)
    val usedMemory = runtime.freeMemory.toDouble / runtime.totalMemory.toDouble
    if (usedMemory >= clearLimit) {
      logger.debug(s"Runtime memory consumption at $usedMemory, clearing ANTLR DFA cache")
      clearDFA()
    }

    val (programCtx, errors) = antlrParser.parse()
    errors.foreach(logger.warn)
    programCtx
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
