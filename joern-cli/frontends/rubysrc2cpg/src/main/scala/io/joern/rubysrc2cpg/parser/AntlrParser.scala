package io.joern.rubysrc2cpg.parser

import better.files.File
import org.antlr.v4.runtime.*
import org.antlr.v4.runtime.atn.{ATN, ATNConfigSet, ProfilingATNSimulator}
import org.antlr.v4.runtime.dfa.DFA
import org.slf4j.LoggerFactory
import java.io.FileWriter
import java.io.File.separator
import java.util
import scala.collection.mutable.ListBuffer
import scala.util.{Try, Using}

/** Summarizes the result of parsing a file.
  */
case class ParserResult(program: Try[RubyParser.ProgramContext], errors: List[String], warnings: List[String])

/** A consumable wrapper for the RubyParser class used to parse the given file and be disposed thereafter.
  * @param inputDir
  *   the directory of the target to parse.
  * @param filename
  *   the file path to the file to be parsed.
  * @param withDebugging
  *   if set, will enable the ANTLR debugger, i.e, printing of the parse tree.
  * @param withProfiler
  *   if set, will enable a profiler and dump logs for each parsed file alongside it.
  */
class AntlrParser(inputDir: File, filename: String, withDebugging: Boolean = false, withProfiler: Boolean = false) {

  private val charStream = CharStreams.fromFileName(filename)
  private val lexer      = new RubyLexer(charStream)

  private val tokenStream                                = new CommonTokenStream(RubyLexerPostProcessor(lexer))
  val parser: RubyParser                                 = new RubyParser(tokenStream)
  private var profilerOpt: Option[ProfilingATNSimulator] = None

  parser.setTrace(withDebugging)
  if (withProfiler) {
    val profiler = new ProfilingATNSimulator(parser)
    parser.setInterpreter(profiler)
    parser.setProfile(true)
    profilerOpt = Option(profiler)
  }

  def parse(): ParserResult = {
    val errors   = ListBuffer[String]()
    val warnings = ListBuffer[String]()
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
      ): Unit = {
        val atn            = recognizer.getATN
        val decisionNumber = dfa.decision
        val ruleIndex      = atn.decisionToState.get(decisionNumber).ruleIndex
        val ruleName       = recognizer.getRuleNames()(ruleIndex)

        val startToken = recognizer.getTokenStream.get(startIndex)
        val stopToken  = recognizer.getTokenStream.get(stopIndex)

        warnings.append(
          s"Parser ambiguity detected for rule '$ruleName' (decision ${dfa.decision}) from token '${startToken.getText}' [startIndex=$startIndex] to '${stopToken.getText}' [stopIndex=$stopIndex], alternatives: ${ambigAlts.toString}"
        )
      }

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

    val program = Try {
      val program = parser.program()

      // If profiling is enabled, read metrics and write accompanying file
      profilerOpt.foreach { profiler =>
        val logFilename = filename.replaceAll("\\.[^.]+$", "") + ".log"
        val atn         = parser.getATN
        Using.resource(FileWriter(logFilename)) { logFile =>
          logFile.write("Profiling information for file: " + filename + "\n\n")

          var totalTimeInPrediction = 0L
          var totalLookaheadOps     = 0L

          profiler.getDecisionInfo.foreach { decision =>
            val decisionNumber = decision.decision
            val ruleIndex      = atn.decisionToState.get(decisionNumber).ruleIndex
            val ruleName       = parser.getRuleNames()(ruleIndex)

            logFile.write(s"Decision $decisionNumber ($ruleName):\n")
            logFile.write(s"  Invocations: ${decision.invocations}\n")
            logFile.write(s"  Time (ns): ${decision.timeInPrediction}\n")
            logFile.write(s"  SLL lookahead operations: ${decision.SLL_TotalLook}\n")
            logFile.write(s"  LL lookahead operations: ${decision.LL_TotalLook}\n")

            totalTimeInPrediction += decision.timeInPrediction
            totalLookaheadOps += decision.SLL_TotalLook + decision.LL_TotalLook
          }
          logFile.write(s"Total time in prediction: $totalTimeInPrediction ns\n")
          logFile.write(s"Total lookahead operations: $totalLookaheadOps\n")
        }
      }

      program
    }
    ParserResult(program, errors.toList, warnings.toList)
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
class ResourceManagedParser(clearLimit: Double, debug: Boolean = false, profiling: Boolean = false)
    extends AutoCloseable {

  private val logger                                 = LoggerFactory.getLogger(getClass)
  private val runtime                                = Runtime.getRuntime
  private var maybeDecisionToDFA: Option[Array[DFA]] = None
  private var maybeAtn: Option[ATN]                  = None

  def parse(inputFile: File, filename: String): Try[RubyParser.ProgramContext] = {
    val inputDir    = if inputFile.isDirectory then inputFile else inputFile.parent
    val antlrParser = AntlrParser(inputDir, filename, debug, profiling)
    val interp      = antlrParser.parser.getInterpreter
    // We need to grab a live instance in order to get the static variables as they are protected from static access
    maybeDecisionToDFA = Option(interp.decisionToDFA)
    maybeAtn = Option(interp.atn)
    val usedMemory = runtime.freeMemory.toDouble / runtime.totalMemory.toDouble
    if (usedMemory >= clearLimit && !profiling) { // Profiler may need the DFA for approximating time used at decisions
      logger.debug(s"Runtime memory consumption at $usedMemory, clearing ANTLR DFA cache")
      clearDFA()
    }

    val ParserResult(programCtx, errors, warnings) = antlrParser.parse()
    errors.foreach(logger.warn)
    if (profiling) warnings.foreach(logger.warn)
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
