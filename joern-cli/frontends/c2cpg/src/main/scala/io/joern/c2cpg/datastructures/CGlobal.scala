package io.joern.c2cpg.datastructures

import io.joern.c2cpg.astcreation.Defines
import io.joern.c2cpg.parser.FileDefaults
import overflowdb.BatchedUpdate.DiffGraphBuilder
import io.joern.x2cpg.Ast
import io.joern.x2cpg.datastructures.Global

import scala.collection.mutable
import scala.jdk.CollectionConverters._

object CGlobal extends Global {

  private val headerAstCache: mutable.HashMap[String, mutable.HashSet[(Int, Int)]] =
    mutable.HashMap.empty

  val headerFileFullNameToPostfix: mutable.HashMap[String, Int] =
    mutable.HashMap.empty

  def headerFiles: Set[String] = headerAstCache.keySet.toSet

  def typesSeen(): List[String] = {
    val types = usedTypes.keys().asScala.filterNot(_ == Defines.anyTypeName).toList
    usedTypes.clear()
    types
  }

  def shouldBeCleared(): Boolean = {
    if (headerAstCache.nonEmpty) {
      headerAstCache.clear()
      headerFileFullNameToPostfix.clear()
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
