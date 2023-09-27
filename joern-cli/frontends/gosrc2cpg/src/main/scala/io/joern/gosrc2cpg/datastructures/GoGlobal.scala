package io.joern.gosrc2cpg.datastructures

import io.joern.gosrc2cpg.astcreation.Defines
import io.joern.x2cpg.datastructures.Global

import java.util.Map
import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters.EnumerationHasAsScala

object GoGlobal extends Global {

  /** This map will only contain the mapping for those packages whose package name is different from the enclosing
    * folder name
    *
    * e.g
    *
    * module namespace = joern.io/sample
    *
    * folder path = <project_root>/lib
    *
    * package name = fpkg
    *
    * In above sample as the package name `fpkg` is different from `lib` this one will be cached in the map
    */
  val aliasToNameSpaceMapping: ConcurrentHashMap[String, String]               = new ConcurrentHashMap()
  val methodFullNameReturnTypeMap: ConcurrentHashMap[String, (String, String)] = new ConcurrentHashMap()
  val structTypeMemberTypeMapping: ConcurrentHashMap[String, String]           = new ConcurrentHashMap()

  def recordAliasToNamespaceMapping(alias: String, namespace: String): Unit = {
    aliasToNameSpaceMapping.putIfAbsent(alias, namespace)
  }

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
