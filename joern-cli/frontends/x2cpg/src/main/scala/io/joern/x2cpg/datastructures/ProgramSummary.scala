package io.joern.x2cpg.datastructures

import io.shiftleft.codepropertygraph.generated.nodes.DeclarationNew

import scala.collection.immutable.Map
import scala.collection.mutable

/** A hierarchical data-structure that stores the result of types and their respective members. These types can be
  * sourced from pre-parsing the application, or pre-computed stubs of common libraries.
  *
  * The utility of this object is in assisting resolving shorthand types during AST creation.
  *
  * @tparam T
  *   the type/class meta data class.
  */
trait ProgramSummary[T <: TypeLike[_, _]] {

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

object ProgramSummary {

  /** Combines two namespace-to-type maps.
    */
  def combine[T <: TypeLike[_, _]](a: Map[String, Set[T]], b: Map[String, Set[T]]): Map[String, Set[T]] = {
    val accumulator = mutable.HashMap.from(a)

    b.keySet.foreach(k =>
      accumulator.updateWith(k) {
        case Some(existing) => Option(a.getOrElse(k, Set.empty) ++ b.getOrElse(k, Set.empty) ++ existing)
        case None           => Option(a.getOrElse(k, Set.empty) ++ b.getOrElse(k, Set.empty))
      }
    )
    accumulator.toMap
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
  */
trait TypedScope[M <: MethodLike, F <: FieldLike, T <: TypeLike[M, F]](summary: ProgramSummary[T]) {
  this: Scope[_, _, TypedScopeElement] =>

  /** Tracks the types that are visible to this scope.
    */
  protected val typesInScope = mutable.Set.empty[T]

  /** Tracks the members visible to this scope. In languages like JavaScript or Python, where members can be directly
    * imported and accessed without an explicit base, they are kept here.
    */
  protected val membersInScope = mutable.Set.empty[MemberLike]

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
    // TODO: Handle partially quaified names
    typesInScope
      .collectFirst {
        case typ if typ.name.split("[.]").lastOption == typeName.split("[.]").lastOption  => typ
        case typ if aliasedTypes.contains(typeName) && typ.name == aliasedTypes(typeName) => typ
      }
  }

  /** Given the type full name and call name, will attempt to find the matching entry.
    *
    * @param typeFullName
    *   the base type full name. If none, will refer to loosely imported member or functions.
    * @param callName
    *   the call name.
    * @param argTypes
    *   the observed argument types. Only relevant for languages that implement overloading.
    * @return
    *   the method meta data if found.
    */
  def tryResolveMethodInvocation(
    callName: String,
    argTypes: List[String],
    typeFullName: Option[String] = None
  ): Option[M] = typeFullName match {
    case None =>
      // TODO: The typesInScope part is to imprecisely solve the unimplemented polymorphism limitation
      (membersInScope ++ typesInScope.flatMap(_.methods))
        .collectFirst { case m: MethodLike if m.name == callName => m.asInstanceOf[M] }
    case Some(tfn) =>
      val methodsWithEqualArgs = tryResolveTypeReference(tfn).flatMap { t =>
        Option(
          t.methods.filter(m => m.name == callName && m.parameterTypes.filterNot(_._1 == "this").size == argTypes.size)
        )
      }

      methodsWithEqualArgs
        .getOrElse(List.empty[M])
        .find(isOverloadedBy(_, argTypes)) match {
        case Some(m) => Option(m)
        case None    => methodsWithEqualArgs.getOrElse(List.empty[M]).headOption
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

  /** Given the type full name and field name, will attempt to find the matching entry.
    * @param typeFullName
    *   the base type full name. If none, will refer to loosely imported member or functions.
    * @param fieldName
    *   the field/object property/module variable name.
    * @return
    *   the field/object property/module variable's meta data.
    */
  def tryResolveFieldAccess(fieldName: String, typeFullName: Option[String] = None): Option[F] = typeFullName match {
    case None => membersInScope.collectFirst { case f: FieldLike if f.name == fieldName => f.asInstanceOf[F] }
    case Some(tfn) =>
      tryResolveTypeReference(tfn).flatMap { t =>
        t.fields.find { f => f.name == fieldName }
      }
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

  /** Appends known members to the scope.
    * @param typeOrModule
    *   the type name or full name.
    * @param memberNames
    *   the names of the members, or, if empty, imports all members from the type.
    */
  def addImportedMember(typeOrModule: String, memberNames: String*): Unit = {
    val matchingTypes   = summary.matchingTypes(typeOrModule)
    val matchingMembers = matchingTypes.flatMap(t => t.fields ++ t.methods)
    memberNames match {
      case Nil => membersInScope.addAll(matchingMembers)
      case names =>
        val nameSet         = names.toSet // Cast to set for O(1) membership query
        val filteredMembers = matchingMembers.filter(member => nameSet.contains(member.name))
        membersInScope.addAll(filteredMembers)
    }
  }

  /** Given a method, will attempt to find the associated type with preference to the types in scope.
    * @param m
    *   the method meta data.
    * @return
    *   the type meta data, if found.
    */
  def typeForMethod(m: M): Option[T] = {
    typesInScope.find(t => t.methods.contains(m))
  }

}

/** An implementation of combining the typed scoping structures to manage the available type information at namespace
  * levels.
  *
  * @tparam M
  *   the method/function meta data class.
  * @tparam F
  *   the field/object property meta data class.
  * @tparam T
  *   the type/class meta data class.
  * @param summary
  *   the program summary.
  */
class DefaultTypedScope[M <: MethodLike, F <: FieldLike, T <: TypeLike[M, F]](summary: ProgramSummary[T])
    extends Scope[String, DeclarationNew, TypedScopeElement]
    with TypedScope[M, F, T](summary) {

  /** Pops the scope, adding types from the scope if necessary.
    */
  override def pushNewScope(scopeNode: TypedScopeElement): Unit = {
    scopeNode match {
      case n: NamespaceLikeScope => typesInScope.addAll(summary.typesUnderNamespace(n.fullName))
      case _                     =>
    }
    super.pushNewScope(scopeNode)
  }

  /** Pops the scope, removing types from the scope if necessary.
    */
  override def popScope(): Option[TypedScopeElement] = {
    super.popScope().map {
      case n: NamespaceLikeScope =>
        summary.typesUnderNamespace(n.fullName).foreach(typesInScope.remove)
        n
      case x => x
    }
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
trait NamespaceLikeScope extends TypedScopeElement {

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

/** An entity that is a member to some type or module.
  */
trait MemberLike {

  /** @return
    *   the name of the member.
    */
  def name: String
}

/** A member that behaves like a field/property/module variabe.
  */
trait FieldLike extends MemberLike {

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
trait MethodLike extends MemberLike {

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
