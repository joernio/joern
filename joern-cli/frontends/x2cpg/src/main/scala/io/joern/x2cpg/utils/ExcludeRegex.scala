package io.joern.x2cpg.utils

import io.joern.x2cpg.SourceFiles

import java.nio.file.Paths
import scala.util.matching.Regex

/** The single definition of how the `--exclude-regex` option is matched.
  *
  * A path is excluded if the regex matches the '''whole''' path relative to the input directory, i.e., the semantics of
  * [[scala.util.matching.Regex.matches]]. Relative paths use the separator of the operating system and have no leading
  * separator. Matching is case-sensitive. Directories are matched as well as files; a matching directory is excluded
  * with everything below it.
  *
  * Regexes starting with `.*` match the same paths when applied to an absolute path. Everything else, e.g.,
  * `src/gen/.*` or `(?!.*keep).*`, requires the relative path.
  */
object ExcludeRegex {

  /** @return
    *   true if `regex` fully matches the path of `path` relative to `inputPath`. The empty regex, i.e., the default,
    *   never excludes anything.
    */
  def matchesRelative(regex: Regex, path: String, inputPath: String): Boolean =
    nonEmpty(regex) && regex.matches(SourceFiles.toRelativePath(path, inputPath))

  /** Matches `regex` against the absolute, normalized path of `path`.
    *
    * This is deprecated: use [[matchesRelative]]. It only exists so that callers can warn if it is the only matcher
    * that matches.
    *
    * Regexes with a negative lookaround, e.g., `(?!src/keep/).*\.js`, are never matched against the absolute path: they
    * are satisfied by nearly every absolute path and would exclude everything instead of just the paths that the regex
    * is meant to select. They are only meaningful relative to the input directory.
    */
  def matchesLegacyAbsolute(regex: Regex, path: String): Boolean =
    nonEmpty(regex) && !hasNegativeLookaround(regex) &&
      regex.matches(Paths.get(path).toAbsolutePath.normalize().toString)

  private def nonEmpty(regex: Regex): Boolean = regex.pattern.pattern().nonEmpty

  private def hasNegativeLookaround(regex: Regex): Boolean = {
    val pattern = regex.pattern.pattern()
    pattern.contains("(?!") || pattern.contains("(?<!")
  }

}
