package io.joern.c2cpg.datastructures

import io.joern.c2cpg.parser.FileDefaults
import overflowdb.BatchedUpdate.DiffGraphBuilder
import io.joern.x2cpg.Ast
import io.joern.x2cpg.datastructures.Global

import java.util.concurrent.ConcurrentHashMap
import scala.collection.mutable

class CGlobal extends Global {

  val file2OffsetTable: ConcurrentHashMap[String, Array[Int]] =
    new ConcurrentHashMap()

}

object CGlobal {

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

  def getAstsFromAstCache(
    diffGraph: DiffGraphBuilder,
    filename: String,
    fromFilename: String,
    linenumber: Option[Integer],
    columnnumber: Option[Integer],
    astCreatorFunction: => Seq[Ast]
  ): Seq[Ast] = {
    val (callCreatorFunc, addDirectlyToDiff) =
      CGlobal.synchronized {
        if (
          FileDefaults
            .isHeaderFile(filename) && filename != fromFilename && linenumber.isDefined && columnnumber.isDefined
        ) {
          if (!headerAstCache.contains(filename)) {
            headerAstCache.put(filename, mutable.HashSet((linenumber.get, columnnumber.get)))
            (true, true)
          } else {
            if (!headerAstCache(filename).contains((linenumber.get, columnnumber.get))) {
              headerAstCache(filename).add((linenumber.get, columnnumber.get))
              (true, true)
            } else {
              (false, false)
            }
          }
        } else {
          (true, false)
        }
      }

    if (callCreatorFunc) {
      val asts = astCreatorFunction
      if (addDirectlyToDiff) {
        asts.foreach(Ast.storeInDiffGraph(_, diffGraph))
        Seq.empty
      } else {
        asts
      }
    } else {
      Seq.empty
    }
  }

}
