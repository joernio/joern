package io.joern.gosrc2cpg.datastructures

import io.joern.gosrc2cpg.astcreation.Defines
import io.joern.x2cpg.datastructures.Global

import java.util.Map
import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters.EnumerationHasAsScala

object GoGlobal extends Global {

  val methodFullNameReturnTypeMap: ConcurrentHashMap[String, (String, String)] = new ConcurrentHashMap()
  val structTypeMemberTypeMapping: ConcurrentHashMap[String, String]           = new ConcurrentHashMap()

  def recordStructTypeMemberType(memberFullName: String, memberType: String): Unit = {
    structTypeMemberTypeMapping.putIfAbsent(memberFullName, memberType)
  }

  def recordFullNameToReturnType(methodFullName: String, returnType: String, signature: String): Unit = {
    methodFullNameReturnTypeMap.putIfAbsent(methodFullName, (returnType, signature))
  }

  def typesSeen(): List[String] = {
    val types = usedTypes.keys().asScala.filterNot(_ == Defines.anyTypeName).toList
    usedTypes.clear()
    types
  }

}
