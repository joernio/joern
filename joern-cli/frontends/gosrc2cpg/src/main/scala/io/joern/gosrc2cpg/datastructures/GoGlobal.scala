package io.joern.gosrc2cpg.datastructures

import io.joern.x2cpg.Ast
import org.slf4j.LoggerFactory

import java.util.concurrent.{ConcurrentHashMap, ConcurrentSkipListSet}
class GoGlobal {
  private val logger             = LoggerFactory.getLogger(getClass)
  var mainModule: Option[String] = None
  var processingDependencies     = false

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

  val nameSpaceMetaDataMap: ConcurrentHashMap[String, NameSpaceMetaData] = new ConcurrentHashMap()

  def recordAliasToNamespaceMapping(alias: String, namespace: String): Unit = synchronized {
    val existingVal = aliasToNameSpaceMapping.putIfAbsent(alias, namespace)
    // NOTE: !namespace.startsWith(mainModule.get) this check will not add the mapping for main source code imports.
    // This will make sure to add the entry in CacheBuilder, which in turn creates the required Package level TypeDecl AST structure as well.
    if (existingVal == null) {
      recordForThisNamespaceThroughImports(namespace)
    } else if (existingVal != namespace) {
      // TODO: This might need better way of recording the information.
      logger.warn(s"more than one namespaces are found for given alias `$alias` -> `$existingVal` and `$namespace`")
    }
  }

  def recordForThisNamespaceThroughImports(namespace: String): Unit = {
    if (mainModule == None || (mainModule != None && !namespace.startsWith(mainModule.get)))
      recordForThisNamespace(namespace)
  }

  def recordForThisNamespace(namespace: String): Boolean = {
    val existing = nameSpaceMetaDataMap.putIfAbsent(namespace, NameSpaceMetaData())
    existing == null
  }

  def getMethodMetadata(namespace: String, methodName: String): Option[MethodCacheMetaData] = {
    Option(nameSpaceMetaDataMap.get(namespace)) match {
      case Some(existingNamespace) =>
        Option(existingNamespace.methodMetaMap.get(methodName))
      case _ =>
        None
    }
  }

  def recordMethodMetadata(namespace: String, methodName: String, methodMetaData: MethodCacheMetaData): Unit = {
    Option(nameSpaceMetaDataMap.get(namespace)) match {
      case Some(existingNamespace) =>
        existingNamespace.methodMetaMap.put(methodName, methodMetaData)
      case _ =>
        // handling for types and lambda functions defined inside methods. Wrapping method becomes the part of their namespace.
        val (wrappingNamespace, membertoken) = splitNamespaceFromMember(namespace)
        // now check if this namespace is present in the map. If yes then make the new entry for this sub namespace
        if (nameSpaceMetaDataMap.containsKey(wrappingNamespace) && checkForDependencyFlags(membertoken)) {
          recordForThisNamespace(namespace)
          recordMethodMetadata(namespace, methodName, methodMetaData)
        }
    }
  }

  def getStructTypeMemberType(namespace: String, memberName: String): Option[String] = {
    Option(nameSpaceMetaDataMap.get(namespace)) match {
      case Some(existingNamespace) =>
        Option(existingNamespace.structTypeMembers.get(memberName))
      case _ =>
        None
    }
  }
  def recordStructTypeMemberTypeInfo(namespace: String, memberName: String, memberType: String): Unit = {
    Option(nameSpaceMetaDataMap.get(namespace)) match {
      case Some(existingNamespace) =>
        existingNamespace.structTypeMembers.put(memberName, memberType)
      case _ =>
        val (wrappingNamespace, membertoken) = splitNamespaceFromMember(namespace)
        if (nameSpaceMetaDataMap.containsKey(wrappingNamespace) && checkForDependencyFlags(membertoken)) {
          recordForThisNamespace(namespace)
          recordStructTypeMemberTypeInfo(namespace, memberName, memberType)
        }
    }
  }

  def recordPkgLevelVarAndConstantAst(pkg: String, ast: Ast, filePath: String): Unit = synchronized {
    Option(pkgLevelVarAndConstantAstMap.get(pkg)) match {
      case Some(existingList) =>
        val t = (ast, filePath)
        pkgLevelVarAndConstantAstMap.put(pkg, existingList + t)
      case None => pkgLevelVarAndConstantAstMap.put(pkg, Set((ast, filePath)))
    }
  }

  def recordForThisLamdbdaSignature(signature: String): Unit = {
    lambdaSignatureToLambdaTypeMap.putIfAbsent(signature, new ConcurrentSkipListSet())
  }

  def recordLambdaSigntureToLambdaType(signature: String, lambdaTypeInfo: LambdaTypeInfo): Unit = {
    Option(lambdaSignatureToLambdaTypeMap.get(signature)) match {
      case Some(existingList) =>
        existingList.add(lambdaTypeInfo)
      case _ =>
    }
  }

  def splitNamespaceFromMember(fullName: String): (String, String) = {
    if (fullName.contains('.')) {
      val lastDotIndex  = fullName.lastIndexOf('.')
      val nameSpaceName = fullName.substring(0, lastDotIndex)
      val memberName    = fullName.substring(lastDotIndex + 1)
      (nameSpaceName, memberName)
    } else {
      (fullName, "")
    }
  }

  /** While processing the dependencies code ignoring package level global variables, constants, types, and functions
    * starting with lower case letter as those are only accessible within package. So those will not be referred from
    * main source code.
    *
    * @param name
    * @return
    */
  def checkForDependencyFlags(name: String): Boolean = {
    !processingDependencies || processingDependencies && name.headOption.exists(_.isUpper)
  }
}

case class NameSpaceMetaData(
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
  structTypeMembers: ConcurrentHashMap[String, String] = new ConcurrentHashMap(),
  // Mapping method fullname to its return type and signature, lambda expression return type also getting recorded under this map
  methodMetaMap: ConcurrentHashMap[String, MethodCacheMetaData] = ConcurrentHashMap()
)

case class MethodCacheMetaData(returnType: String, signature: String)

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
