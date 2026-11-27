package io.joern.x2cpg.utils

import io.joern.x2cpg.SourceFiles

import java.nio.file.{Path, Paths}
import java.util.regex.Pattern
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
    *   true if `regex` fully matches the path of `path` relative to `inputPath`, or one of the directories between
    *   `inputPath` and `path`. The latter is how a matching directory excludes everything below it, also for callers
    *   that only see the files. The empty regex, i.e., the default, never excludes anything.
    *
    * A `path` that is not below `inputPath` cannot be made relative, see [[SourceFiles.toRelativePath]]. It is matched
    * as a whole, without its parents: they are not between `inputPath` and `path`.
    */
  def matchesRelative(regex: Regex, path: String, inputPath: String): Boolean = {
    nonEmpty(regex) && {
      val relativeOrOutside = Paths.get(SourceFiles.toRelativePath(path, inputPath))
      if (relativeOrOutside.isAbsolute) {
        regex.matches(relativeOrOutside.toString)
      } else {
        Iterator
          .iterate[Path](relativeOrOutside)(_.getParent)
          .takeWhile(_ != null)
          .exists(part => regex.matches(part.toString))
      }
    }
  }

  /** Matches `regex` against the absolute, normalized path of `path`.
    *
    * This is deprecated: use [[matchesRelative]]. It only exists so that callers can warn if it is the only matcher
    * that matches.
    *
    * A regex with a negative lookaround, e.g., `(?!src/keep/).*\.js`, is only matched against the absolute path if it
    * starts with something that pins it to an absolute path: `/`, a drive letter, or `.*`. Otherwise, it is meant for
    * the relative path: the lookaround is evaluated at the start of the absolute path, where it is trivially true, and
    * the regex would exclude the very files it is meant to keep.
    */
  def matchesLegacyAbsolute(regex: Regex, path: String): Boolean =
    nonEmpty(regex) && (!hasNegativeLookaround(regex) || startsWithAbsolutePrefix(regex)) &&
      regex.matches(Paths.get(path).toAbsolutePath.normalize().toString)

  private def nonEmpty(regex: Regex): Boolean = regex.pattern.pattern().nonEmpty

  private def hasNegativeLookaround(regex: Regex): Boolean = {
    val pattern = regex.pattern.pattern()
    pattern.contains("(?!") || pattern.contains("(?<!")
  }

  private val AbsolutePrefix = Pattern.compile("""\^?(?:\\Q)?(?:/|[A-Za-z]:|\.\*)""")

  private def startsWithAbsolutePrefix(regex: Regex): Boolean =
    AbsolutePrefix.matcher(regex.pattern.pattern()).lookingAt()

}
