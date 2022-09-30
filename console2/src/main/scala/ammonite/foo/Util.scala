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

  /**
    * Information about where a particular block of code came from; [[path]]
    * is optional because some code snippets are synthetic, which means any
    * filename is entirely synthetic and $file imports do not work in them.
    * However, there are many snippets of code, e.g. repl commands and such,
    * which have a "fake" [[path]] because we want to allow $file imports
    * relative to some path or working-directory
    */
  case class CodeSource(wrapperName: Name,
                        flexiblePkgName: Seq[Name],
                        pkgRoot: Seq[Name],
                        path: Option[os.Path]){
    // All code Ammonite compiles must be rooted in some package within
    // the `ammonite` top-level package
    assert(pkgRoot.head == Name("ammonite"))
    def pkgName = pkgRoot ++ flexiblePkgName
    def fullName = pkgName :+ wrapperName

    def fileName = path.fold(filePathPrefix.last + ".sc")(_.last)
    def jvmPathPrefix = Util.encodeJvmPath(fullName)
    def filePathPrefix = Util.encodeFilePath(fullName)
    def printablePath = path match{
      case Some(x) => x.toString
      case None => "(synthetic)/" + filePathPrefix.mkString("/") + ".sc"
    }
  }

  def encodeFilePath(path: Seq[Name]) = path.map(_.encoded)
  def encodeScalaSourcePath(path: Seq[Name]) = path.map(_.backticked).mkString(".")
  def encodeJvmPath(path: Seq[Name]) = path.map(_.encoded).mkString(".")
}
