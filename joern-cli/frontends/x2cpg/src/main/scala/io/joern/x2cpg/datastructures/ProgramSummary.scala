package io.joern.x2cpg.datastructures

import scala.collection.mutable

/** A heirarchical data-structure that stores the result of types and their respective members. These types can be
  * sourced from pre-parsing the application, or pre-computed stubs of common libraries.
  *
  * The utility of this object is in assisting resolving shorthand types during AST creation.
  *
  * @tparam M
  *   the method/function meta data class.
  * @tparam F
  *   the field/object property meta data class.
  * @tparam T
  *   the type/class meta data class.
  */
trait ProgramSummary[M <: MethodLike, F <: FieldLike, T <: TypeLike[M, F]] {

  /** A mapping between a namespace/directory and the containing types.
    */
  protected val namespaceToType: Map[String, Set[T]]

  /** For the given namespace, returns the declared types.
    */
  def typesUnderNamespace(namespace: String): Set[T] = namespaceToType.getOrElse(namespace, Set.empty)

  /** For a type, will search for the associated namespace.
    */
  def namespaceFor(clazz: T): Option[String] = namespaceToType.find { case (_, v) => v.contains(clazz) }.map(_._1)

  /** @param typeName
    *   the type name or full name. Can be partially qualified.
    * @return
    *   the set of matching types' meta data.
    */
  def matchingTypes(typeName: String): List[T] = {
    namespaceToType.values.flatten.filter(_.name.endsWith(typeName)).toList
  }

}

/** Extends the capability of the scope object to track types in scope as provide type resolution.
  *
  * @tparam M
  *   the method/function meta data class.
  * @tparam F
  *   the field/object property meta data class.
  * @tparam T
  *   the type/class meta data class.
  * @tparam S
  *   the scope type.
  */
trait TypedScope[M <: MethodLike, F <: FieldLike, T <: TypeLike[M, F], S <: TypedScopeElement](
  summary: ProgramSummary[M, F, T]
) { this: Scope[_, _, S] =>

  /** Tracks the types that are visible to this scope.
    */
  protected val typesInScope = mutable.Set.empty[T]

  /** Tracks any types or modules imported under alternative names to their type full names.
    */
  protected val aliasedTypes = mutable.HashMap.empty[String, String]

  /** Given a type name or alias, attempts to resolve its full name using the types currently in scope.
    *
    * @param typeName
    *   the shorthand name.
    * @return
    *   the type meta-data if found.
    */
  def tryResolveTypeReference(typeName: String): Option[T] = {
    typesInScope
      .collectFirst {
        case typ if typ.name.endsWith(typeName)                                           => typ
        case typ if aliasedTypes.contains(typeName) && typ.name == aliasedTypes(typeName) => typ
      }
      .flatMap(typ => summary.namespaceFor(typ).map(namespace => typ))
  }

  /** Given the type full name and call name, will attempt to find the matching entry.
    *
    * @param typeFullName
    *   the base type full name.
    * @param callName
    *   the call name.
    * @param argTypes
    *   the observed argument types. Only relevant for languages that implement overloading.
    * @return
    *   the method full name if found.
    */
  def tryResolveMethodInvocation(typeFullName: String, callName: String, argTypes: List[String]): Option[M] = {
    tryResolveTypeReference(typeFullName).flatMap { t =>
      t.methods.find { m => m.name == callName && isOverloadedBy(m, argTypes) }
    }
  }

  /** Determines if, by observing the given argument types, that the method's signature is a plausible match to the
    * observed arguments.
    *
    * The default implementation only considers that the same number of arguments are added and does not account for
    * variadic arguments nor polymorphism.
    *
    * @param method
    *   the method meta data.
    * @param argTypes
    *   the observed arguments from the call-site.
    * @return
    *   true if the method could be overloaded by a call with these argument types.
    */
  protected def isOverloadedBy(method: M, argTypes: List[String]): Boolean = {
    method.parameterTypes.size == argTypes.size
  }

  /** Appends known types imported into the scope.
    * @param namespace
    *   the fully qualified imported namespace.
    */
  def addImportedNamespace(namespace: String): Unit = {
    val knownTypesFromNamespace = summary.typesUnderNamespace(namespace)
    typesInScope.addAll(knownTypesFromNamespace)
  }

  /** Appends known types imported into the scope.
    * @param typeOrModule
    *   the type name or full name.
    */
  def addImportedTypeOrModule(typeOrModule: String): Unit = {
    val matchingTypes = summary.matchingTypes(typeOrModule)
    typesInScope.addAll(matchingTypes)
  }

}

/** An implementation of combining the typed scoping structures to manage the available type information at namespace
  * levels.
  *
  * @param summary
  *   the program summary.
  */
class DefaultTypedScope[I, V, S <: TypedScopeElement, M <: MethodLike, F <: FieldLike, T <: TypeLike[M, F]](
  summary: ProgramSummary[M, F, T]
) extends Scope[I, V, S]
    with TypedScope[M, F, T, S](summary) {

  /** Pops the scope, adding types from the scope if necessary.
    */
  override def pushNewScope(scopeNode: S): Unit = {
    scopeNode match
      case n: NamespaceTSE => typesInScope.addAll(summary.typesUnderNamespace(n.fullName))
      case _               =>
    super.pushNewScope(scopeNode)
  }

  /** Pops the scope, removing types from the scope if necessary.
    */
  override def popScope(): Option[S] = {
    super.popScope() match
      case Some(n: NamespaceTSE) =>
        summary.typesUnderNamespace(n.fullName).foreach(typesInScope.remove)
        Some(n)
      case x => x
  }

}

/*
  Traits related to scoping classes
 */

/** A scope element designed for the TypedScope.
  */
trait TypedScopeElement

/** A namespace scope to synchronise types entering and exiting scopes.
  */
trait NamespaceTSE extends TypedScopeElement {

  /** @return
    *   the namespace full name.
    */
  def fullName: String
}

/*
  Traits related to meta-data classes
 */

/** A type declaration or module. Holds methods and field entities.
  *
  * @tparam M
  *   the method/function meta data class.
  * @tparam F
  *   the field/object property meta data class.
  */
trait TypeLike[M <: MethodLike, F <: FieldLike] {

  /** @return
    *   the type full name.
    */
  def name: String

  /** @return
    *   the methods declared directly under the type declaration.
    */
  def methods: List[M]

  /** @return
    *   the fields/properties declared directly under the type declaration.
    */
  def fields: List[F]

}

/** A member that behaves like a field/property/module variabe.
  */
trait FieldLike {

  /** @return
    *   the name of the field.
    */
  def name: String

  /** @return
    *   the type declared (not necessarily resolved)
    */
  def typeName: String
}

/** A function or procedure.
  */
trait MethodLike {

  /** @return
    *   the name of the method.
    */
  def name: String

  /** Stores a tuple of the parameter name and type name.
    *
    * @return
    *   the names and type names of the parameters.
    */
  def parameterTypes: List[(String, String)]

  /** Stores the return type name.
    *
    * @return
    *   the return type name.
    */
  def returnType: String

}
