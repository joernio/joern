package io.joern.c2cpg.astcreation

import io.joern.x2cpg.datastructures.Global
import java.util.concurrent.ConcurrentHashMap

import scala.jdk.CollectionConverters.*

class CGlobal extends Global {

  import io.joern.c2cpg.passes.FunctionDeclNodePass.MethodInfo

  val methodDeclarations: ConcurrentHashMap[String, MethodInfo] = new ConcurrentHashMap()
  val methodDefinitions: ConcurrentHashMap[String, Boolean]     = new ConcurrentHashMap()

  // 1) not in this map at all -> not included from any source file; we will parse it based on the header files extension
  // 2) in this map with value HeaderFileParserLanguage.C.ordinal -> included from at least one C source file
  // 3) in this map with value HeaderFileParserLanguage.Cpp.ordinal -> included from at least one C++ source file
  // 4) in this map with value HeaderFileParserLanguage.Both.ordinal -> included from at least one C and one C++ source file, so we will have to parse it twice
  val headerIncludes: ConcurrentHashMap[String, Int] = new ConcurrentHashMap()

  def typesSeen(): List[String] = {
    usedTypes.keys().asScala.toList
  }

  def unhandledMethodDeclarations(): Map[String, MethodInfo] = {
    methodDeclarations.asScala.toMap -- methodDefinitions.asScala.keys
  }

}
