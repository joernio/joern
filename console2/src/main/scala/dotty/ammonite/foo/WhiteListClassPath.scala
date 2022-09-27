package dotty.ammonite.foo

import ammonite.foo.Util

import dotty.tools.dotc.classpath.{ClassPathEntries, PackageName}
import dotty.tools.io.ClassPath

class WhiteListClasspath(aggregates: Seq[ClassPath], whitelist: Set[Seq[String]])
  extends dotty.tools.dotc.classpath.AggregateClassPath(aggregates) {
  override def findClassFile(name: String) = {
    val tokens = name.split('.')
    if (Util.lookupWhiteList(whitelist, tokens.init ++ Seq(tokens.last + ".class"))) {
      super.findClassFile(name)
    }
    else None
  }

  override def list(inPackage: PackageName) = {
    val superList = super.list(inPackage)
    ClassPathEntries(
      superList.packages.filter{ p => Util.lookupWhiteList(whitelist, p.name.split('.')) },
      superList.classesAndSources.filter{ t =>
        Util.lookupWhiteList(whitelist, inPackage.dottedString.split('.') ++ Seq(t.name + ".class"))
      }
    )
  }

  override def toString: String =
    s"WhiteListClasspath($aggregates, ${whitelist.size} white-listed elements)"
}
