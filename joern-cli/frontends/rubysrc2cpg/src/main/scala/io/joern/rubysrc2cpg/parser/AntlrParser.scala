package io.joern.rubysrc2cpg.parser

import better.files.File
import better.files.File.OpenOptions
import org.antlr.v4.runtime.*
import org.antlr.v4.runtime.atn.{ATN, ATNConfigSet, ProfilingATNSimulator}
import org.antlr.v4.runtime.dfa.DFA
import org.slf4j.LoggerFactory

import java.io.FileWriter
import java.io.File.separator
import java.nio.file.{Files, Path, StandardOpenOption}
import java.util
import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}
import scala.collection.concurrent.TrieMap
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.{Try, Using}
import flatgraph.help.Table
import flatgraph.help.Table.AvailableWidthProvider
import io.shiftleft.semanticcpg

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
  * @param maybeParserProfiler
  *   the parser profiler used to capture profiling information.
  */
class AntlrParser(
  inputDir: File,
  filename: String,
  withDebugging: Boolean = false,
  maybeParserProfiler: Option[ParserProfiler] = None
) {

  val parser: RubyParser = {
    val charStream  = CharStreams.fromFileName(filename)
    val lexer       = new RubyLexer(charStream)
    val tokenStream = new CommonTokenStream(RubyLexerPostProcessor(lexer))
    new RubyParser(tokenStream)
  }
  private var profilerOpt: Option[ProfilingATNSimulator] = None

  parser.setTrace(withDebugging)
  if (maybeParserProfiler.isDefined) {
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
      val parseStart = System.nanoTime()
      val program    = parser.program()
      val parseTime  = System.nanoTime() - parseStart
      maybeParserProfiler.foreach(_.captureParseTime(filename, parseTime))
      // If profiling is enabled, read metrics and write accompanying file
      profilerOpt.foreach(profiler =>
        maybeParserProfiler.foreach(_.captureProfilerLogs(parser, inputDir.pathAsString, filename, profiler))
      )

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
  private val profiler: Option[ParserProfiler]       = if profiling then Option(ParserProfiler()) else None

  def parse(inputFile: File, filename: String): Try[RubyParser.ProgramContext] = {
    val inputDir    = if inputFile.isDirectory then inputFile else inputFile.parent
    val antlrParser = AntlrParser(inputDir, filename, debug, profiler)
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

class ParserProfiler {

  private val logger                    = LoggerFactory.getLogger(getClass)
  private val ruleCost                  = TrieMap.empty[String, RuleTimeCost]
  private val fileCost                  = TrieMap.empty[String, Long]
  private var projectRoot: Option[Path] = None

  // Note: This is in a shutdown hook to guarantee output is dumped, however it is optional and can readily be
  // replaced or dumped at the end of successful parse runs only.
  sys.addShutdownHook {
    dumpSummary()
  }

  /** An object to aggregate the performance cost of a rule.
    * @param predictionTime
    *   the total time in prediction (ns).
    * @param lookaheads
    *   total lookahead operations.
    */
  private case class RuleTimeCost(predictionTime: Long, lookaheads: Long) {
    def +(o: RuleTimeCost): RuleTimeCost =
      this.copy(this.predictionTime + o.predictionTime, this.lookaheads + o.lookaheads)
  }

  def captureParseTime(filename: String, nanoTime: Long): Unit =
    fileCost.put(filename, nanoTime)

  def captureProfilerLogs(
    parser: RubyParser,
    inputDir: String,
    filename: String,
    profiler: ProfilingATNSimulator
  ): Unit = {
    // Set project root
    if projectRoot.isEmpty then projectRoot = Option(Path.of(inputDir))

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

        val ruleTimeCost = RuleTimeCost(decision.timeInPrediction, decision.SLL_TotalLook + decision.LL_TotalLook)
        totalTimeInPrediction += ruleTimeCost.predictionTime
        totalLookaheadOps += ruleTimeCost.lookaheads

        ruleCost.updateWith(ruleName) {
          case Some(x) => Option(x + ruleTimeCost)
          case None    => Option(ruleTimeCost)
        }
      }
      logFile.write(s"Total time in prediction: $totalTimeInPrediction ns\n")
      logFile.write(s"Total lookahead operations: $totalLookaheadOps\n")
    }
  }

  private def dumpSummary(): Unit = {
    projectRoot match {
      case Some(root) =>
        val conversionFactor                               = 1 / 1e6
        val timeUnit                                       = "ms"
        val summaryPath                                    = root.resolve("antlr_summary.log")
        implicit val widthProvider: AvailableWidthProvider = semanticcpg.defaultAvailableWidthProvider
        val totalParseTime                                 = fileCost.values.sum
        val avgParseTime                                   = totalParseTime / fileCost.size.toDouble
        val mostExpensiveFileStr = fileCost.toList.sortBy(_._2).reverse.headOption.map { case (name, time) =>
          f"Most Expensive File: ${root.relativize(Path.of(name))} (${time * conversionFactor}%.2f $timeUnit)"
        }

        val columnNames = Seq("Rule Name", s"Prediction Time ($timeUnit)", "Total Lookaheads")
        val rulesByTimeTable = Table(
          columnNames = columnNames,
          rows = ruleCost.toList.sortBy { case (_, timeCost) => timeCost.predictionTime }.reverse.take(10).map {
            case (ruleName, timeCost) =>
              Seq(ruleName, f"${timeCost.predictionTime * conversionFactor}%.2f", timeCost.lookaheads.toString)
          }
        )
        val rulesByLookaheadTable = Table(
          columnNames = columnNames,
          rows = ruleCost.toList.sortBy { case (_, timeCost) => timeCost.lookaheads }.reverse.take(10).map {
            case (ruleName, timeCost) =>
              Seq(ruleName, f"${timeCost.predictionTime * conversionFactor}%.2f", timeCost.lookaheads.toString)
          }
        )

        if (Files.exists(summaryPath.getParent)) {
          Files.writeString(
            summaryPath,
            f"""Summary for project at '${root.getFileName}'
               |Total Parsed Files: ${fileCost.size}
               |Total Parse Time (CPU): ${totalParseTime * conversionFactor}%.2f ($timeUnit)
               |Avg. Parse Time Per File: ${avgParseTime * conversionFactor}%.2f ($timeUnit)
               |${mostExpensiveFileStr.getOrElse("")}
               |
               |Most Expensive Rules By Time in Prediction
               |==========================================
               |${rulesByTimeTable.render}
               |
               |Most Expensive Rules By Total SLL & LL Lookaheads
               |=================================================
               |${rulesByLookaheadTable.render}
               |""".stripMargin,
            StandardOpenOption.TRUNCATE_EXISTING,
            StandardOpenOption.CREATE
          )
        } else {
          logger.warn(s"${summaryPath.getParent} does not exist. Skipping profile summary dump.")
        }
      case None => logger.warn("At least one file must be parsed for profiling information to be dumped")
    }
  }

}
