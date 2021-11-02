package io.shiftleft.c2cpg.datastructures

import io.shiftleft.c2cpg.parser.FileDefaults
import io.shiftleft.passes.DiffGraph
import io.shiftleft.x2cpg.Ast

import java.util.concurrent.ConcurrentHashMap
import scala.collection.mutable

class Global {

  val usedTypes: ConcurrentHashMap[String, Boolean] =
    new ConcurrentHashMap()

  val file2LinesCache: ConcurrentHashMap[String, Seq[Int]] =
    new ConcurrentHashMap()

}

object Global {

  private val headerAstCache: mutable.HashMap[String, mutable.HashSet[(Integer, Integer)]] =
    mutable.HashMap.empty

  def headerFiles: Set[String] = headerAstCache.keySet.toSet

  def shouldBeCleared(): Boolean = {
    if (headerAstCache.nonEmpty) {
      headerAstCache.clear()
      true
    } else {
      false
    }
  }

  def getAstsFromAstCache(diffGraph: DiffGraph.Builder,
                          filename: String,
                          fromFilename: String,
                          linenumber: Option[Integer],
                          columnnumber: Option[Integer],
                          astCreatorFunction: => Seq[Ast]): Seq[Ast] = Global.synchronized {
    if (FileDefaults
          .isHeaderFile(filename) && filename != fromFilename && linenumber.isDefined && columnnumber.isDefined) {
      if (!headerAstCache.contains(filename)) {
        headerAstCache.put(filename, mutable.HashSet((linenumber.get, columnnumber.get)))
        astCreatorFunction.foreach(Ast.storeInDiffGraph(_, diffGraph))
      } else {
        if (!headerAstCache(filename).contains((linenumber.get, columnnumber.get))) {
          headerAstCache(filename).add((linenumber.get, columnnumber.get))
          astCreatorFunction.foreach(Ast.storeInDiffGraph(_, diffGraph))
        }
      }
      Seq.empty
    } else { astCreatorFunction }
  }

}
