package io.shiftleft.semanticcpg.language.types.structure

import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.semanticcpg.language.*
import org.apache.commons.io.FilenameUtils

/** A config file entry
  */
class ConfigFileTraversal(val traversal: Iterator[nodes.ConfigFile]) extends AnyVal {

  /** Only files that end with `str` (ignoring casing, because that's how Mac/Windows work) */
  def suffix(str: String): Iterator[nodes.ConfigFile] = {
    val lower = str.toLowerCase
    traversal.filter(_.name.toLowerCase.endsWith(lower))
  }

  /** Only files that end with one of the values in `strings` (ignoring casing, because that's how Mac/Windows work) */
  def suffix(strings: String*): Iterator[nodes.ConfigFile] = {
    val lower = strings.map(_.toLowerCase)
    traversal.filter { configFile =>
      val name = configFile.name.toLowerCase
      lower.exists(name.endsWith)
    }
  }

  /** Only config files where the path ends with the given (file/directory) name (e.g. `foo/bar` matches `baz\foo\bar`).
    *
    * Normalizes to UNIX paths and ignores casing (because Windows/Mac do that). Use slashes when checking for directory
    * names
    */
  def fileName(name: String): Iterator[nodes.ConfigFile] = {
    val lower = name.toLowerCase
    traversal.filter { cfgFile =>
      val norm = FilenameUtils.separatorsToUnix(cfgFile.name.toLowerCase)
      norm == lower || norm.endsWith(s"/${lower}")
    }
  }

  /** Map to (filename, content) pairs
    */
  def nameContentPairs: Iterator[(String, String)] = {
    traversal.map(configFile => (configFile.name, configFile.content))
  }

}
