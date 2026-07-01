package io.joern.javasrc2cpg.util

import java.nio.file.Path
import scala.util.matching.Regex

/** Filters delombok stderr to drop noisy false-positive `cannot find symbol` diagnostics that reference peer package
  * roots. Peer-root errors are false positives for a given delombok invocation because Lombok's AST-manipulation pass
  * only runs on files given as inputs — peer sources reached via `--sourcepath` are compiled but not lomboked. Anything
  * genuinely wrong in a peer root will surface when that peer is itself the input root in a different invocation.
  *
  * Fail-open: any stderr line that doesn't parse as a recognised diagnostic record is kept verbatim.
  */
object DelombokStderrFilter {

  /** Index that maps top-level Java FQNs (and inner-class prefixes) to the absolute package roots they belong to, plus
    * the ordered list of absolute roots so file paths can be mapped to a root.
    *
    * @param fqnToRoots
    *   top-level class FQN (dotted) to the set of absolute package roots declaring that FQN. Multiple roots per FQN
    *   are possible when peer source trees declare the same class name — callers must treat this as ambiguous.
    * @param absoluteRoots
    *   absolute, normalised package roots in original ordering.
    */
  case class FqnIndex(fqnToRoots: Map[String, Set[Path]], absoluteRoots: Seq[Path]) {
    private val rootsByDepth: Seq[Path] = absoluteRoots.sortBy(-_.getNameCount)

    /** Longest dotted-prefix match on the given FQN. So `com.example.UserRecord.InnerClass` matches an entry
      * `com.example.UserRecord`. Returns the empty set if no match.
      */
    def rootsFor(fqn: String): Set[Path] = {
      val prefixes = Iterator
        .iterate(fqn.replace('$', '.')) { candidate =>
          val dot = candidate.lastIndexOf('.')
          if (dot < 0) "" else candidate.substring(0, dot)
        }
        .takeWhile(_.nonEmpty)
      prefixes.flatMap(fqnToRoots.get).nextOption().getOrElse(Set.empty)
    }

    /** Which package root (if any) contains this absolute file path. Returns the longest matching root. */
    def rootForFile(absPath: Path): Option[Path] = {
      val normalised = absPath.toAbsolutePath.normalize()
      rootsByDepth.find(normalised.startsWith)
    }
  }

  object FqnIndex {

    /** Build an index from the SourceParser file infos and the discovered package roots.
      *
      *   - For each file, compute the top-level FQN as `packageName + "." + <file stem>`.
      *   - Each file is assigned to the (unique) package root whose relative path is a prefix of the file's relative
      *     path. The dot-root matches everything.
      *   - Absolute-root list is `packageRoots.map(inputPath.resolve(_).toAbsolutePath.normalize)`.
      */
    def build(inputPath: Path, fileInfo: List[SourceParser.FileInfo], packageRoots: List[Path]): FqnIndex = {
      val dotPath           = Path.of(".")
      val absoluteInputPath = inputPath.toAbsolutePath.normalize()
      val absoluteRoots     = packageRoots.map(root => absoluteInputPath.resolve(root.toString).normalize())

      // For lookup: pair each relative root with its absolute root, sort by descending name count so that the
      // most specific (longest) root wins.
      val rootsByNameCount =
        packageRoots.zip(absoluteRoots).sortBy { case (rel, _) => -rel.getNameCount }

      val fqnToRoots: Map[String, Set[Path]] = fileInfo.iterator.flatMap { info =>
        val stem = {
          val fileName = info.relativePath.getFileName.toString
          if (fileName.endsWith(".java")) fileName.dropRight(".java".length) else fileName
        }
        val fqn = info.packageName match {
          case Some(pkg) if pkg.nonEmpty => s"$pkg.$stem"
          case _                         => stem
        }
        val matchingRoot = rootsByNameCount.collectFirst {
          case (rel, abs) if rel == dotPath                    => abs
          case (rel, abs) if info.relativePath.startsWith(rel) => abs
        }
        matchingRoot.map(root => fqn -> root)
      }.toList.groupMap(_._1)(_._2).view.mapValues(_.toSet).toMap

      FqnIndex(fqnToRoots, absoluteRoots)
    }
  }

  /** Regex for the header line of a `cannot find symbol` diagnostic record: `<absolute file path>:<line>: error:
    * <msg>`. Matches POSIX absolute paths (`/…`) as well as Windows drive-letter paths (`C:\…` / `C:/…`).
    */
  private val HeaderRegex: Regex =
    raw"^((?:/|[A-Za-z]:[\\/])[^:]+):(\d+):\s*error:\s*(.*)$$".r

  /** `location:` line, class/interface/enum form. */
  private val LocationClassRegex: Regex = raw"^\s*location:\s*(?:class|interface|enum)\s+([A-Za-z0-9_.$$]+)\s*$$".r

  /** `location:` line, `variable NAME of type FQN` form. */
  private val LocationVariableRegex: Regex =
    raw"^\s*location:\s*variable\s+\S+\s+of\s+type\s+([A-Za-z0-9_.$$]+)\s*$$".r

  /** `symbol:` line marker. */
  private val SymbolRegex: Regex = raw"^\s*symbol:\s*.*$$".r

  /** A parsed `cannot find symbol` record.
    *
    * @param locationFqn
    *   dotted FQN as extracted from the `location:` line. `$` (from inner-class names in javac output) is preserved
    *   as-is here; normalisation to dots happens at lookup time in [[FqnIndex.rootsFor]].
    */
  private case class Record(lines: Seq[String], file: Path, locationFqn: Option[String])

  /** Result of streaming stderr into records and unclassified passthrough lines, preserving their original order.
    */
  private sealed trait Element
  private case class RecordElement(record: Record) extends Element
  private case class PassthroughLine(line: String) extends Element

  /** Filter the given stderr lines: drop `cannot find symbol` records that reference peer roots; keep everything else
    * verbatim. Order is preserved.
    *
    * @param peerRoots
    *   absolute, normalised peer package roots (callers must pre-normalise; matched by exact equality).
    */
  def filter(peerRoots: Set[Path], index: FqnIndex, stderrLines: Seq[String]): Seq[String] = {
    val elements = splitIntoElements(stderrLines)

    elements.flatMap {
      case PassthroughLine(line) => Seq(line)
      case RecordElement(record) =>
        val fileRoot      = index.rootForFile(record.file)
        val locationRoots = record.locationFqn.map(index.rootsFor).getOrElse(Set.empty)
        val fileInPeer    = fileRoot.exists(peerRoots.contains)
        // Drop only when every possible originating root is a peer — if the FQN could also
        // resolve to the current root, keep the record (fail-open, may log a false positive).
        val locInPeer     = locationRoots.nonEmpty && locationRoots.forall(peerRoots.contains)
        if (fileInPeer || locInPeer)
          Seq.empty
        else
          record.lines
    }
  }

  /** Stream-parse stderr lines into a mixed list of records (5-line `cannot find symbol` diagnostics) and passthrough
    * lines (anything we didn't recognise).
    */
  private def splitIntoElements(lines: Seq[String]): Seq[Element] = {
    val indexed = lines.toIndexedSeq
    val out     = collection.mutable.ArrayBuffer.empty[Element]
    var i       = 0
    while (i < indexed.length) {
      tryParseRecord(indexed, i) match {
        case Some(record) =>
          out += RecordElement(record)
          i += record.lines.length
        case None =>
          out += PassthroughLine(indexed(i))
          i += 1
      }
    }
    out.toSeq
  }

  /** Try to parse the 5-line `cannot find symbol` diagnostic starting at `start`. Returns `None` if any of the required
    * lines is missing or doesn't match the expected shape (header / symbol / location). Snippet and caret lines
    * (indices +1 and +2) are consumed but not inspected.
    */
  private def tryParseRecord(lines: IndexedSeq[String], start: Int): Option[Record] = {
    for {
      header <- lines.lift(start).flatMap(HeaderRegex.findFirstMatchIn)
      if header.group(3).contains("cannot find symbol")
      _          <- lines.lift(start + 1) // snippet
      _          <- lines.lift(start + 2) // caret
      symbolLine <- lines.lift(start + 3)
      if SymbolRegex.matches(symbolLine)
      locationLine <- lines.lift(start + 4)
      locationFqn <- LocationClassRegex
        .findFirstMatchIn(locationLine)
        .map(_.group(1))
        .orElse(LocationVariableRegex.findFirstMatchIn(locationLine).map(_.group(1)))
    } yield Record(
      lines = lines.slice(start, start + 5),
      file = Path.of(header.group(1)),
      locationFqn = Some(locationFqn)
    )
  }
}
