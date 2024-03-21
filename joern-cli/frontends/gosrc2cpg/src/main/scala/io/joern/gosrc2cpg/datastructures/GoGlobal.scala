package io.joern.gosrc2cpg.datastructures

import dotty.tools.dotc.util.Signatures.Signature
import io.joern.x2cpg.Ast

import java.util.concurrent.{ConcurrentHashMap, ConcurrentSkipListSet}
class GoGlobal {

  var processingDependencies = false

  /** NOTE: This set is getting cleared inside {@link io.joern.gosrc2cpg.passes.MethodAndTypeCacheBuilderPass#process}
    * to release the memory once done with the use.
    */
  val sourcePackageSet: java.util.Set[String] = new ConcurrentSkipListSet[String]()

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
  val aliasToNameSpaceMapping: ConcurrentHashMap[String, String] = new ConcurrentHashMap()

  /** This map will record the Type FullName of Struct Type defined for Lambda Expression along with return type
    * fullname against the lambda signature.
    *
    * This will help map the Lambda TypeFullName with the respective Struct Type as supper Type
    */
  val lambdaSignatureToLambdaTypeMap: ConcurrentHashMap[String, java.util.Set[LambdaTypeInfo]] = new ConcurrentHashMap()

  val pkgLevelVarAndConstantAstMap: ConcurrentHashMap[String, Set[(Ast, String)]] = new ConcurrentHashMap()

  // Mapping method fullname to its return type and signature, lambda expression return type also getting recorded under this map
  val methodFullNameReturnTypeMap: ConcurrentHashMap[String, (String, String)] = new ConcurrentHashMap()

  /** Mapping fully qualified name of the member variable of a struct type to it's type It will also maintain the type
    * mapping for package level global variables. e.g.
    *
    * module namespace = joern.io/sample
    *
    * package sample
    *
    * type Person struct{ Age int}
    *
    * var ( HostURL = "http://api.sample.com" )
    *
    * It will map
    *
    * `joern.io/sample.Person.Age` - `int`
    *
    * `joern.io/sample.HostURL` - `string`
    */
  val structTypeMemberTypeMapping: ConcurrentHashMap[String, String] = new ConcurrentHashMap()

  def recordAliasToNamespaceMapping(alias: String, namespace: String): String = {
    aliasToNameSpaceMapping.putIfAbsent(alias, namespace)
  }

  def recordStructTypeMemberType(memberFullName: String, memberType: String): Unit = {
    structTypeMemberTypeMapping.putIfAbsent(memberFullName, memberType)
  }

  def recordFullNameToReturnType(methodFullName: String, returnType: String, signature: String): Unit = {
    methodFullNameReturnTypeMap.putIfAbsent(methodFullName, (returnType, signature))
  }

  def recordPkgLevelVarAndConstantAst(pkg: String, ast: Ast, filePath: String): Unit = synchronized {
    Option(pkgLevelVarAndConstantAstMap.get(pkg)) match {
      case Some(existingList) =>
        val t = (ast, filePath)
        pkgLevelVarAndConstantAstMap.put(pkg, existingList + t)
      case None => pkgLevelVarAndConstantAstMap.put(pkg, Set((ast, filePath)))
    }
  }

  def recordForThisLamdbdaSignature(signature: String): Unit = synchronized {
    Option(lambdaSignatureToLambdaTypeMap.get(signature)) match {
      case None => lambdaSignatureToLambdaTypeMap.put(signature, new ConcurrentSkipListSet())
      case _    =>
    }
  }

  def recordLambdaSigntureToLambdaType(signature: String, lambdaTypeInfo: LambdaTypeInfo): Unit = {
    Option(lambdaSignatureToLambdaTypeMap.get(signature)) match {
      case Some(existingList) =>
        existingList.add(lambdaTypeInfo)
      case _ =>
    }
  }
}

case class LambdaTypeInfo(lambdaStructTypeFullName: String, returnTypeFullname: String)
    extends Comparable[LambdaTypeInfo] {
  override def compareTo(that: LambdaTypeInfo): Int = {
    val lambdaStructTypeFullNameComparison = this.lambdaStructTypeFullName.compareTo(that.lambdaStructTypeFullName)
    if (lambdaStructTypeFullNameComparison != 0) {
      lambdaStructTypeFullNameComparison
    } else {
      this.returnTypeFullname.compareTo(that.returnTypeFullname)
    }
  }
}
