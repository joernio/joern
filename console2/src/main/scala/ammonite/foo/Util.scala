/** originally from amm/util/src/main/scala/ammonite/util/Util.scala */

package ammonite.foo

import java.security.MessageDigest

object Util{
  val newLine = System.lineSeparator()
  val javaPrefixes = Set("java", "jdk", "javax")
  def lookupWhiteList(whitelist: Set[Seq[String]], tokens: Seq[String]): Boolean = {
    if (whitelist.isEmpty) true
    else {
      tokens.foreach(s => assert(!s.contains('/'), s))
      javaPrefixes.contains(tokens.head) || whitelist(tokens)
    }
  }

  def encodeScalaSourcePath(path: Seq[Name]) = path.map(_.backticked).mkString(".")
}
