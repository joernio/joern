package io.joern.gosrc2cpg.datastructures

import io.joern.x2cpg.Ast

import java.util.concurrent.ConcurrentHashMap

class GoGlobal {

  var processingDependencies = false

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

  val lambdaSignatureToLambdaTypeMap: ConcurrentHashMap[String, Set[(String, String)]] = new ConcurrentHashMap()

  val pkgLevelVarAndConstantAstMap: ConcurrentHashMap[String, Set[(Ast, String)]] = new ConcurrentHashMap()

  // Mapping method fullname to its return type and signature
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

  def recordPkgLevelVarAndConstantAst(pkg: String, ast: Ast, filePath: String): Unit = {
    synchronized {
      Option(pkgLevelVarAndConstantAstMap.get(pkg)) match {
        case Some(existingList) =>
          val t = (ast, filePath)
          pkgLevelVarAndConstantAstMap.put(pkg, existingList + t)
        case None => pkgLevelVarAndConstantAstMap.put(pkg, Set((ast, filePath)))
      }
    }
  }

  def recordLambdaSigntureToLambdaType(
    signature: String,
    lambdaStructTypeFullName: String,
    returnTypeFullname: String
  ): Unit = {
    synchronized {
      Option(lambdaSignatureToLambdaTypeMap.get(signature)) match {
        case Some(existingList) =>
          val t = (lambdaStructTypeFullName, returnTypeFullname)
          lambdaSignatureToLambdaTypeMap.put(signature, existingList + t)
        case None => lambdaSignatureToLambdaTypeMap.put(signature, Set((lambdaStructTypeFullName, returnTypeFullname)))
      }
    }
  }

}
