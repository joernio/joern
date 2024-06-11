package io.joern.rubysrc2cpg.datastructures

import better.files.File
import io.joern.rubysrc2cpg.passes.GlobalTypes
import io.joern.rubysrc2cpg.passes.GlobalTypes.kernelPrefix
import io.joern.x2cpg.Defines
import io.joern.rubysrc2cpg.passes.Defines as RDefines
import io.joern.x2cpg.datastructures.*
import io.shiftleft.codepropertygraph.generated.NodeTypes
import io.shiftleft.codepropertygraph.generated.nodes.{DeclarationNew, NewLocal, NewMethodParameterIn}

import java.io.File as JFile
import scala.collection.mutable
import scala.reflect.ClassTag
import scala.util.Try

class RubyScope(summary: RubyProgramSummary, projectRoot: Option[String])
    extends Scope[String, DeclarationNew, TypedScopeElement]
    with TypedScope[RubyMethod, RubyField, RubyType](summary) {

  private val builtinMethods = GlobalTypes.kernelFunctions
    .map(m => RubyMethod(m, List.empty, Defines.Any, Some(GlobalTypes.kernelPrefix)))
    .toList

  override val typesInScope: mutable.Set[RubyType] =
    mutable.Set(RubyType(GlobalTypes.kernelPrefix, builtinMethods, List.empty))

  // Add some built-in methods that are significant
  // TODO: Perhaps create an offline pre-built list of methods
  typesInScope.addAll(
    Seq(
      RubyType(
        s"$kernelPrefix.Array",
        List(RubyMethod("[]", List.empty, s"$kernelPrefix.Array", Option(s"$kernelPrefix.Array"))),
        List.empty
      ),
      RubyType(
        s"$kernelPrefix.Hash",
        List(RubyMethod("[]", List.empty, s"$kernelPrefix.Hash", Option(s"$kernelPrefix.Hash"))),
        List.empty
      )
    )
  )

  override val membersInScope: mutable.Set[MemberLike] = mutable.Set(builtinMethods*)

  /** @return
    *   using the stack, will initialize a new module scope object.
    */
  def newProgramScope: Option[ProgramScope] = surroundingScopeFullName.map(ProgramScope.apply)

  /** @return
    *   true if the top of the stack is the program/module.
    */
  def isSurroundedByProgramScope: Boolean = {
    stack
      .take(2)
      .filterNot {
        case ScopeElement(BlockScope(_), _) => true
        case _                              => false
      }
      .headOption match {
      case Some(ScopeElement(ProgramScope(_), _)) => true
      case _                                      => false
    }
  }

  def pushField(field: FieldDecl): Unit = {
    popScope().foreach {
      case TypeScope(fullName, fields) =>
        pushNewScope(TypeScope(fullName, fields :+ field))
      case x =>
        pushField(field)
        pushNewScope(x)
    }
  }

  def getFieldsInScope: List[FieldDecl] =
    stack.collect { case ScopeElement(TypeScope(_, fields), _) => fields }.flatten

  def findFieldInScope(fieldName: String): Option[FieldDecl] = {
    getFieldsInScope.find(_.name == fieldName)
  }

  override def pushNewScope(scopeNode: TypedScopeElement): Unit = {
    // Use the summary to determine if there is a constructor present
    val mappedScopeNode = scopeNode match {
      case n: NamespaceLikeScope =>
        typesInScope.addAll(summary.typesUnderNamespace(n.fullName))
        n
      case n: ProgramScope =>
        typesInScope.addAll(summary.typesUnderNamespace(n.fullName))
        n
      case TypeScope(name, _) =>
        typesInScope.addAll(summary.matchingTypes(name))
        scopeNode
      case _ => scopeNode
    }

    super.pushNewScope(mappedScopeNode)
  }

  def addRequire(
    projectRoot: String,
    currentFilePath: String,
    requiredPath: String,
    isRelative: Boolean,
    isWildCard: Boolean = false
  ): Unit = {
    val path = requiredPath.stripSuffix(":<global>") // Sometimes the require call provides a processed path
    // We assume the project root is the sole LOAD_PATH of the project sources
    // NB: Tracking whatever has been added to $LOADER is dynamic and requires post-processing step!
    val resolvedPath =
      if (isRelative) {
        Try((File(currentFilePath).parent / path).pathAsString).toOption
          .map(_.stripPrefix(s"$projectRoot${JFile.separator}"))
          .getOrElse(path)
      } else {
        path
      }

    val pathsToImport =
      if (isWildCard) {
        val dir = File(projectRoot) / resolvedPath
        if (dir.isDirectory)
          dir.list
            .map(
              _.pathAsString.stripPrefix(s"$projectRoot${JFile.separator}").stripSuffix(".rb").replaceAll("\\\\", "/")
            )
            .toList
        else Nil
      } else {
        resolvedPath :: Nil
      }

    pathsToImport.foreach { pathName =>
      // Pull in type / module defs
      summary.pathToType.getOrElse(pathName, Set()) match {
        case x if x.nonEmpty =>
          x.foreach { ty => addImportedTypeOrModule(ty.name) }
          addImportedFunctions(pathName)
        case _ =>
          addRequireGem(path)
      }
    }
  }

  def addImportedFunctions(importName: String): Unit = {
    val matchingTypes = summary.namespaceToType.values.flatten.filter(x =>
      x.name.startsWith(importName) && x.name.endsWith(RDefines.Program)
    )
    typesInScope.addAll(matchingTypes)
  }

  def addInclude(typeOrModule: String): Unit = {
    addImportedMember(typeOrModule)
  }

  def addRequireGem(gemName: String): Unit = {
    val matchingTypes = summary.namespaceToType.values.flatten.filter(_.name.startsWith(gemName))
    typesInScope.addAll(matchingTypes)
  }

  /** @return
    *   the full name of the surrounding scope.
    */
  def surroundingScopeFullName: Option[String] = stack.collectFirst {
    case ScopeElement(x: NamespaceLikeScope, _) => x.fullName
    case ScopeElement(x: TypeLikeScope, _)      => x.fullName
    case ScopeElement(x: MethodLikeScope, _)    => x.fullName
  }

  /** Locates a position in the stack matching a partial function, modifies it and emits a result
    * @param pf
    *   Tests ScopeElements of the stack. If they match, return the new value and the result to emi
    * @return
    *   the emitted result if the position was found and modifies
    */
  def updateSurrounding[T](
    pf: PartialFunction[
      ScopeElement[String, DeclarationNew, TypedScopeElement],
      (ScopeElement[String, DeclarationNew, TypedScopeElement], T)
    ]
  ): Option[T] = {
    stack.zipWithIndex
      .collectFirst { case (pf(elem, res), i) =>
        (elem, res, i)
      }
      .map { case (elem, res, i) =>
        stack = stack.updated(i, elem)
        res
      }
  }

  /** Get the name of the implicit or explict proc param and mark the method scope as using the proc param
    */
  def useProcParam: Option[String] = updateSurrounding {
    case ScopeElement(MethodScope(fullName, param, _), variables) =>
      (ScopeElement(MethodScope(fullName, param, true), variables), param.fold(x => x, x => x))
  }

  /** Get the name of the implicit or explict proc param */
  def anonProcParam: Option[String] = stack.collectFirst { case ScopeElement(MethodScope(_, Left(param), true), _) =>
    param
  }

  /** Set the name of explict proc param */
  def setProcParam(param: String): Unit = updateSurrounding {
    case ScopeElement(MethodScope(fullName, _, _), variables) =>
      (ScopeElement(MethodScope(fullName, Right(param)), variables), ())
  }

  def surroundingTypeFullName: Option[String] = stack.collectFirst { case ScopeElement(x: TypeLikeScope, _) =>
    x.fullName
  }

  /** @return
    *   the corresponding node label according to the scope element.
    */
  def surroundingAstLabel: Option[String] = stack.collectFirst {
    case ScopeElement(_: NamespaceLikeScope, _) => NodeTypes.NAMESPACE_BLOCK
    case ScopeElement(_: ProgramScope, _)       => NodeTypes.METHOD
    case ScopeElement(_: TypeLikeScope, _)      => NodeTypes.TYPE_DECL
    case ScopeElement(_: MethodLikeScope, _)    => NodeTypes.METHOD
  }

  def surrounding[T <: TypedScopeElement](implicit tag: ClassTag[T]): Option[T] = stack.collectFirst {
    case ScopeElement(elem: T, _) => elem
  }

  /** @return
    *   true if one should still generate a default constructor for the enclosing type decl.
    */
  def shouldGenerateDefaultConstructor: Boolean = stack
    .collectFirst {
      case ScopeElement(_: ModuleScope, _)   => false
      case ScopeElement(x: TypeLikeScope, _) => !typesInScope.find(_.name == x.fullName).exists(_.hasConstructor)
      case _                                 => false
    }
    .getOrElse(false)

  /** When a singleton class is introduced into the scope, the base variable will now have the singleton's functionality
    * mixed in. This method finds base variable and appends the singleton type.
    *
    * @param singletonClassName
    *   the singleton type full name.
    * @param variableName
    *   the base variable
    */
  def pushSingletonClassDeclaration(singletonClassName: String, variableName: String): Unit = {
    lookupVariable(variableName).foreach {
      case local: NewLocal =>
        local.possibleTypes(local.possibleTypes :+ singletonClassName)
      case param: NewMethodParameterIn => param.possibleTypes(param.possibleTypes :+ singletonClassName)
      case _                           =>
    }
  }

  override def typeForMethod(m: RubyMethod): Option[RubyType] = {
    typesInScope.find(t => Option(t.name) == m.baseTypeFullName).orElse { super.typeForMethod(m) }
  }

  override def tryResolveTypeReference(typeName: String): Option[RubyType] = {
    val normalizedTypeName = typeName.replaceAll("::", ".")

    /** Given a typeName, attempts to resolve full name using internal types currently in scope
      * @param typeName
      *   the shorthand name
      * @return
      *   the type meta-data if found
      */
    def tryResolveInternalTypeReference(typeName: String): Option[RubyType] = {
      typesInScope.collectFirst {
        case typ if !typ.isInstanceOf[RubyStubbedType] && typ.name.split("[.]").endsWith(typeName.split("[.]")) => typ
      }
    }

    /** Given a typeName, attempts to resolve full name using stubbed types currently in scope
      * @param typeName
      *   the shorthand name
      * @return
      *   the type meta-data if found
      */
    def tryResolveStubbedTypeReference(typeName: String): Option[RubyType] = {
      typesInScope.collectFirst {
        case typ if typ.isInstanceOf[RubyStubbedType] && typ.name.split("[.]").endsWith(typeName.split("[.]")) => typ
      }
    }

    // TODO: While we find better ways to understand how the implicit class loading works,
    //  we can approximate that all types are in scope in the mean time.
    tryResolveInternalTypeReference(typeName)
      .orElse(tryResolveStubbedTypeReference(typeName))
      .orElse {
        super.tryResolveTypeReference(normalizedTypeName) match {
          case None if GlobalTypes.kernelFunctions.contains(normalizedTypeName) =>
            Option(RubyType(s"${GlobalTypes.kernelPrefix}.$normalizedTypeName", List.empty, List.empty))
          case None if GlobalTypes.bundledClasses.contains(normalizedTypeName) =>
            Option(RubyType(s"<${GlobalTypes.builtinPrefix}.$normalizedTypeName>", List.empty, List.empty))
          case None =>
            None
          case x => x
        }
      }
  }

}
