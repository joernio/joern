package io.joern.x2cpg.report

import flatgraph.help.Table
import flatgraph.help.Table.ConstantWidth
import io.joern.x2cpg.X2CpgConfig
import io.joern.x2cpg.utils.TimeUtils
import org.slf4j.{Logger, LoggerFactory}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.ConcurrentLinkedQueue
import scala.collection.concurrent.TrieMap
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Success, Try}

enum Severity {
  case Warning, Error
}

/** What kind of non-fatal problem was encountered. The names are part of the JSON report format that downstream tools
  * rely on and must therefore stay stable.
  */
enum Category {

  /** A source file could not be parsed, or only with problems, e.g., by the AST generator. */
  case FileParse

  /** A parsed source file could not be turned into a CPG. */
  case FileProcessing

  /** Dependencies of the analyzed project could not be resolved or fetched. */
  case DependencyResolution

  /** An external tool (AST generator, compiler, package manager, ...) failed. */
  case ExternalTool
  case Other
}

/** The exception that caused a [[Problem]]. Stack traces are only logged.
  *
  * @param className
  *   the fully qualified class name of the exception.
  * @param message
  *   the message of the exception, if it has one.
  */
final case class ExceptionInfo(className: String, message: Option[String] = None)

object ExceptionInfo {
  def of(throwable: Throwable): ExceptionInfo =
    ExceptionInfo(throwable.getClass.getName, Option(throwable.getMessage).filter(_.nonEmpty))
}

/** A non-fatal problem of a frontend run.
  *
  * @param message
  *   what went wrong. It does not repeat `file`, which is reported (and logged) next to it.
  * @param file
  *   the affected file, relative to the input path where possible.
  * @param exception
  *   the cause, if any.
  */
final case class Problem(
  severity: Severity,
  category: Category,
  message: String,
  file: Option[String] = None,
  exception: Option[ExceptionInfo] = None
)

/** Collects [[Problem]]s. Utilities that run without access to a full [[ScanReport]] (e.g., dependency resolution)
  * accept this narrower interface.
  */
trait ProblemSink {

  /** Only records the problem. Use this if the problem is already logged elsewhere. */
  def record(problem: Problem): Unit

  /** Logs the problem on the given logger, prefixed with the file if there is one, and records it. */
  final def problem(
    severity: Severity,
    category: Category,
    message: String,
    file: Option[String] = None,
    cause: Option[Throwable] = None,
    logger: Logger = ProblemSink.defaultLogger
  ): Unit = {
    val logText = file.map(affected => s"$affected: $message").getOrElse(message)
    severity match {
      case Severity.Warning => logger.warn(logText, cause.orNull)
      case Severity.Error   => logger.error(logText, cause.orNull)
    }
    record(Problem(severity, category, message, file, cause.map(ExceptionInfo.of)))
  }

  final def warn(
    category: Category,
    message: String,
    file: Option[String] = None,
    cause: Option[Throwable] = None,
    logger: Logger = ProblemSink.defaultLogger
  ): Unit = problem(Severity.Warning, category, message, file, cause, logger)

  final def error(
    category: Category,
    message: String,
    file: Option[String] = None,
    cause: Option[Throwable] = None,
    logger: Logger = ProblemSink.defaultLogger
  ): Unit = problem(Severity.Error, category, message, file, cause, logger)
}

object ProblemSink {

  private val defaultLogger: Logger = LoggerFactory.getLogger(classOf[ProblemSink])

  /** Does not keep anything. Problems logged through [[ProblemSink.problem]] still show up in the log. */
  val LogOnly: ProblemSink = new ProblemSink {
    override def record(problem: Problem): Unit = {}
  }
}

/** How a frontend reports the outcome of a run. The table is always logged. */
sealed trait ReportMode

object ReportMode {

  /** Only log the report. */
  case object Log extends ReportMode

  /** Additionally write the report as JSON. If no path is given, it is written next to the CPG. */
  final case class Json(path: Option[String] = None) extends ReportMode
}

enum RunStatus {
  case Completed, Failed
}

/** Result of processing one file. `loc` is unknown if the file could not even be read. */
final case class FileEntry(
  path: String,
  loc: Option[Int],
  parsed: Boolean,
  cpgGenerated: Boolean,
  durationNanos: Long
) {
  private def toRow: Seq[String] = {
    val lines = loc.map(_.toString).getOrElse("-")
    val dur   = if (durationNanos == 0) "-" else TimeUtils.pretty(durationNanos)
    Seq(lines, if (parsed) "yes" else "no", if (cpgGenerated) "yes" else "no", dur)
  }

  private[report] def tableRow(index: Int): Seq[String] = s"$index" +: path +: toRow
}

object ScanReport {

  private val logger = LoggerFactory.getLogger(classOf[ScanReport])

  /** @param cpgPath
    *   where the CPG was written, if it was persisted at all.
    */
  final case class Metadata(
    frontend: String,
    joernVersion: String,
    inputPath: String,
    cpgPath: Option[String],
    status: RunStatus
  )

  /** `<cpg>.report.json`. Without a CPG output path the frontend writes to `cpg.bin`, so the report goes there. */
  def defaultJsonPath(outputPath: String): Path = {
    val cpgPath = Paths.get(if (outputPath.isEmpty) "cpg.bin" else outputPath).toAbsolutePath.normalize()
    cpgPath.resolveSibling(s"${cpgPath.getFileName}.report.json")
  }

  private val ProblemOrdering: Ordering[Problem] =
    Ordering.by(problem =>
      (
        problem.file.getOrElse(""),
        problem.category.ordinal,
        problem.severity.ordinal,
        problem.message,
        problem.exception.map(_.className).getOrElse(""),
        problem.exception.flatMap(_.message).getOrElse("")
      )
    )
}

/** Collects what happened during one run of a frontend: the outcome per file and all non-fatal [[Problem]]s.
  *
  * Thread safe. One instance belongs to exactly one run; `X2CpgFrontend.run` creates it and calls [[finish]] at the
  * end, which logs the report and, if requested via [[ReportMode.Json]], writes it to disk.
  */
class ScanReport extends ProblemSink {
  import ScanReport.*

  private val files    = TrieMap.empty[String, FileEntry]
  private val problems = new ConcurrentLinkedQueue[Problem]()

  override def record(problem: Problem): Unit = problems.add(problem)

  def addReportInfo(
    fileName: String,
    loc: Int,
    parsed: Boolean = false,
    cpgGen: Boolean = false,
    duration: Long = 0
  ): Unit = files(fileName) = FileEntry(fileName, Option.when(loc >= 0)(loc), parsed, cpgGen, duration)

  def updateReport(fileName: String, cpg: Boolean, duration: Long): Unit =
    files.updateWith(fileName) {
      case Some(existing) => Some(existing.copy(cpgGenerated = cpg, durationNanos = duration))
      // files whose parse result could not be read never got an addReportInfo call; still record them
      case None => Some(FileEntry(fileName, loc = None, parsed = false, cpgGenerated = cpg, durationNanos = duration))
    }

  /** All files, sorted by path.
    *
    * Besides the files that were registered, this contains the files that a [[Category.FileParse]] or
    * [[Category.FileProcessing]] problem is about, if they were not registered: frontends need not keep track of the
    * files they fail on. A file that could not be parsed was not parsed, a file that could not be processed was. A file
    * that could not be processed did not get a CPG, even if its frontend registered that it did, e.g., because it
    * recovered from the failure with an empty AST.
    */
  def fileEntries: Seq[FileEntry] = {
    // files is a concurrent TrieMap: entries are sorted by their unique path, so encounter order is irrelevant.
    val registered     = files.values.toSeq // scalafix:ok UnorderedIteration
    val fileProblems   = allProblems.collect { case Problem(_, category, _, Some(file), _) => file -> category }
    val processingFail = fileProblems.collect { case (file, Category.FileProcessing) => file }.toSet
    val parseFail      = fileProblems.collect { case (file, Category.FileParse) => file }.toSet
    val known          = registered.map(_.path).toSet
    val unregistered   = (processingFail ++ parseFail).diff(known).toSeq.sorted.map { file =>
      FileEntry(file, None, parsed = !parseFail.contains(file), cpgGenerated = false, 0)
    }
    val corrected = registered.map { entry =>
      if (processingFail.contains(entry.path)) entry.copy(cpgGenerated = false) else entry
    }
    (corrected ++ unregistered).sortBy(_.path)
  }

  /** All problems in a deterministic order, independent of the order in which threads reported them. */
  def allProblems: Seq[Problem] = problems.asScala.toSeq.sorted(ProblemOrdering)

  private def problemSummary(allProblems: Seq[Problem]): Option[String] = {
    Option
      .when(allProblems.nonEmpty) {
        val errors     = allProblems.count(_.severity == Severity.Error)
        val warnings   = allProblems.count(_.severity == Severity.Warning)
        val byCategory = allProblems
          .groupBy(_.category)
          .toSeq
          .sortBy(_._1.ordinal)
          .map { case (category, inCategory) => s"  $category: ${inCategory.size}" }
        s"Problems: $errors error(s), $warnings warning(s)" +: byCategory
      }
      .map(_.mkString(System.lineSeparator()))
  }

  private def table(entries: Seq[FileEntry]): String = {
    val rows  = entries.zipWithIndex.map { case (entry, index) => entry.tableRow(index + 1) }
    val total = Seq(
      "Total",
      "",
      s"${entries.flatMap(_.loc).sum}",
      s"${entries.count(_.parsed)}/${entries.size}",
      s"${entries.count(_.cpgGenerated)}/${entries.size}",
      ""
    )
    Table(Seq("#", "File", "LOC", "Parsed", "Got a CPG", "Duration"), rows :+ total).render(using
      ConstantWidth(Int.MaxValue)
    )
  }

  /** Logs the file table and the problem summary. Logs nothing if there is nothing to report. */
  def print(): Unit = {
    val entries = fileEntries
    val parts   = Option.when(entries.nonEmpty)(table(entries)).toSeq ++ problemSummary(allProblems)
    if (parts.nonEmpty) {
      logger.info(s"Report:${System.lineSeparator()}${parts.mkString(System.lineSeparator())}")
    }
  }

  def toJson(metadata: Metadata): ujson.Obj = {
    val entries  = fileEntries
    val problems = allProblems
    val summary  = ujson.Obj(
      "files"        -> entries.size,
      "parsed"       -> entries.count(_.parsed),
      "cpgGenerated" -> entries.count(_.cpgGenerated),
      // ujson would render a Long as a string
      "loc"      -> ujson.Num(entries.flatMap(_.loc).map(_.toDouble).sum),
      "errors"   -> problems.count(_.severity == Severity.Error),
      "warnings" -> problems.count(_.severity == Severity.Warning)
    )
    ujson.Obj.from(
      Seq[(String, ujson.Value)](
        "schemaVersion" -> 1,
        "frontend"      -> metadata.frontend,
        "joernVersion"  -> metadata.joernVersion,
        "inputPath"     -> metadata.inputPath
      ) ++
        metadata.cpgPath.map(path => "cpgPath" -> ujson.Str(path)) ++
        Seq[(String, ujson.Value)](
          "status"   -> metadata.status.toString,
          "summary"  -> summary,
          "problems" -> ujson.Arr.from(problems.map(problemJson)),
          "files"    -> ujson.Arr.from(entries.map(fileJson))
        )
    )
  }

  /** Fields that are not available are left out. */
  private def problemJson(problem: Problem): ujson.Obj = ujson.Obj.from(
    Seq[(String, ujson.Value)]("severity" -> problem.severity.toString, "category" -> problem.category.toString) ++
      problem.file.map(file => "file" -> ujson.Str(file)) ++
      Seq[(String, ujson.Value)]("message" -> problem.message) ++
      problem.exception.map(exception => "exception" -> exceptionJson(exception))
  )

  private def exceptionJson(exception: ExceptionInfo): ujson.Obj = ujson.Obj.from(
    Seq[(String, ujson.Value)]("type" -> exception.className) ++
      exception.message.map(message => "message" -> ujson.Str(message))
  )

  /** Fields that are not available (LOC if the file was not readable, a duration that was not measured) are left out.
    */
  private def fileJson(entry: FileEntry): ujson.Obj = ujson.Obj.from(
    Seq[(String, ujson.Value)]("path" -> entry.path) ++
      entry.loc.map(loc => "loc" -> ujson.Num(loc)) ++
      Seq[(String, ujson.Value)]("parsed" -> entry.parsed, "cpgGenerated" -> entry.cpgGenerated) ++
      Option.when(entry.durationNanos > 0)("durationMs" -> ujson.Num(math.round(entry.durationNanos / 1e3) / 1e3))
  )

  /** Writes the report as JSON, replacing an existing file. */
  def writeJson(path: Path, metadata: Metadata): Unit = {
    val target = path.toAbsolutePath.normalize()
    Files.createDirectories(target.getParent)
    Files.writeString(target, ujson.write(toJson(metadata), indent = 2), StandardCharsets.UTF_8)
  }

  /** Reports the outcome of the run: logs the table and writes the JSON file if requested by the configuration. Never
    * throws; failing to write the report must not turn a successful run into a failed one.
    */
  def finish(frontendName: String, joernVersion: String, config: X2CpgConfig[?], status: RunStatus): Unit = {
    print()
    config.reportMode match {
      case ReportMode.Log                =>
      case ReportMode.Json(explicitPath) =>
        val path     = explicitPath.map(Paths.get(_)).getOrElse(defaultJsonPath(config.outputPath))
        val cpgPath  = Option.when(config.outputPath.nonEmpty)(config.outputPath)
        val metadata = Metadata(frontendName, joernVersion, config.inputPath, cpgPath, status)
        Try(writeJson(path, metadata)) match {
          case Success(_)         => logger.info(s"Wrote the scan report to '${path.toAbsolutePath.normalize()}'")
          case Failure(exception) => logger.warn(s"Failed to write the scan report to '$path'", exception)
        }
    }
  }

}
