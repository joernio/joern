package io.joern.x2cpg.datastructures

import io.shiftleft.codepropertygraph.generated.nodes.DeclarationNew

import scala.annotation.targetName
import scala.collection.mutable
import scala.reflect.ClassTag

/** A hierarchical data-structure that stores the result of types and their respective members. These types can be
  * sourced from pre-parsing the application, or pre-computed stubs of common libraries.
  *
  * The utility of this object is in assisting resolving shorthand types during AST creation.
  *
  * @tparam T
  *   the type/class meta data class.
  * @tparam M
  *   the function/method meta data class.
  * @tparam F
  *   the field/property/member meta data class.
  */
trait ProgramSummary[T <: TypeLike[M, F], M <: MethodLike, F <: FieldLike] {

  /** A mapping between a namespace/directory and the containing types.
    */
  protected val namespaceToType: mutable.Map[String, mutable.Set[T]]

  /** For the given namespace, returns the declared types.
    */
  def typesUnderNamespace(namespace: String): Set[T] = namespaceToType.getOrElse(namespace, Set.empty).toSet

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

  /** Absorbs the given program summary information into this program summary.
    * @param o
    *   the program summary to absorb.
    * @return
    *   this program summary.
    */
  def absorb(o: ProgramSummary[T, M, F]): ProgramSummary[T, M, F] = {
    ProgramSummary.merge(this.namespaceToType, o.namespaceToType)
    this
  }

}

object ProgramSummary {

  def merge[T <: TypeLike[M, F], M <: MethodLike, F <: FieldLike](
    a: mutable.Map[String, mutable.Set[T]],
    b: mutable.Map[String, mutable.Set[T]]
  ): mutable.Map[String, mutable.Set[T]] = {

    def dedupTypesInPlace(m: mutable.Map[String, mutable.Set[T]]): Unit = {
      val newMap = m
        .map { case (namespace, ts) => namespace -> ts.groupBy(_.name) }
        .map { case (namespace, typMap) =>
          val dedupedTypes = mutable.Set.from(
            typMap
              .map { case (name, ts) => name -> ts.reduce((u, v) => (u + v).asInstanceOf[T]) }
              .values
              .toSet
          )
          m.put(namespace, dedupedTypes)
          namespace -> dedupedTypes
        }
        .toMap
      assert(m.flatMap { case (name, ts) => ts.groupBy(_.name).map(_._2.size) }.forall(_ == 1))
    }

    // Handle duplicate types sharing the same namespace. This can be introduced from serialized type stubs.
    dedupTypesInPlace(a)
    dedupTypesInPlace(b)

    b.foreach { case (namespace, bts) =>
      a.updateWith(namespace) {
        case Some(ats: mutable.Set[T]) =>
          // Assert that we can simply reduce the grouped values to a simple key-value mapping for fast look-ups
          assert(ats.groupBy(_.name).values.forall(_.sizeIs == 1))
          val atsMap = ats.groupBy(_.name).map { case (name, ts) => name -> ts.head }

          bts.foreach { bt =>
            atsMap.get(bt.name) match {
              case Some(at) =>
                ats.remove(at)
                ats.add((at + bt).asInstanceOf[T])
              case None =>
                ats.add(bt)
            }
          }
          Option(ats)
        case None => b.get(namespace)
      }
    }
    a
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
trait TypedScope[M <: MethodLike, F <: FieldLike, T <: TypeLike[M, F]](summary: ProgramSummary[T, M, F]) {
  this: Scope[?, ?, TypedScopeElement] =>

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
    typesInScope
      .collectFirst {
        // Handle partially qualified names
        case typ if typ.name.split("[.]").endsWith(typeName.split("[.]"))                 => typ
        case typ if aliasedTypes.contains(typeName) && typ.name == aliasedTypes(typeName) => typ
      }
  }

  protected def matchingM(callName: String)(implicit tag: ClassTag[M]): PartialFunction[MemberLike, M] = {
    case m: M if m.name == callName => m
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
  def tryResolveMethodInvocation(callName: String, argTypes: List[String] = Nil, typeFullName: Option[String] = None)(
    implicit tag: ClassTag[M]
  ): Option[M] = typeFullName match {
    case None =>
      // TODO: The typesInScope part is to imprecisely solve the unimplemented polymorphism limitation
      membersInScope.collectFirst(matchingM(callName)).orElse {
        typesInScope.flatMap(_.methods).collectFirst(matchingM(callName))
      }
    case Some(tfn) =>
      tryResolveTypeReference(tfn).flatMap { t =>
        t.methods.find(m => m.name == callName)
      }
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

trait OverloadableScope[M <: OverloadableMethod] {
  this: TypedScope[M, ?, ?] =>
  override def tryResolveMethodInvocation(
    callName: String,
    argTypes: List[String],
    typeFullName: Option[String] = None
  )(implicit tag: ClassTag[M]): Option[M] = typeFullName match {
    case None =>
      // TODO: The typesInScope part is to imprecisely solve the unimplemented polymorphism limitation
      membersInScope.collectFirst(matchingM(callName)).orElse {
        typesInScope.flatMap(_.methods).collectFirst(matchingM(callName))
      }
    case Some(tfn) =>
      val methodsWithEqualArgs = tryResolveTypeReference(tfn).flatMap { t =>
        // TODO: Investigate using `isOverloadedBy` here
        Option(
          t.methods.filter(m => m.name == callName && m.parameterTypes.filterNot(_._1 == "this").size == argTypes.size)
        )
      }

      methodsWithEqualArgs
        .getOrElse(List.empty[M])
        .find(isOverloadedBy(_, argTypes))
        .orElse(methodsWithEqualArgs.getOrElse(List.empty[M]).headOption)
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
class DefaultTypedScope[M <: MethodLike, F <: FieldLike, T <: TypeLike[M, F]](summary: ProgramSummary[T, M, F])
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

  /** Adds the contents of the two types and produces a new type.
    * @param o
    *   the other type-like.
    * @return
    *   a type-like that is the combination of the two, with precedence to colliding contents to this type (LHS).
    */
  @targetName("add")
  def +(o: TypeLike[M, F]): TypeLike[M, F]

  /** Helper method for creating the sum of two type-like's methods, while preferring this types' methods on collisions.
    * @param o
    *   the other type-like.
    * @return
    *   the combination of the two type-like's methods.
    */
  protected def mergeMethods(o: TypeLike[M, ?]): List[M] = {
    val methodNames = methods.map(_.name).toSet
    methods ++ o.methods.filterNot(m => methodNames.contains(m.name))
  }

  /** Helper method for creating the sum of two type-like's fields, while preferring this types' fields on collisions.
    *
    * @param o
    *   the other type-like.
    * @return
    *   the combination of the two type-like's fields.
    */
  protected def mergeFields(o: TypeLike[?, F]): List[F] = {
    val fieldNames = fields.map(_.name).toSet
    fields ++ o.fields.filterNot(f => fieldNames.contains(f.name))
  }

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

  /** Stores the return type name.
    *
    * @return
    *   the return type name.
    */
  def returnType: String

}

trait OverloadableMethod extends MethodLike {

  /** Stores a tuple of the parameter name and type name.
    *
    * @return
    *   the names and type names of the parameters.
    */
  def parameterTypes: List[(String, String)]
}

trait StubbedType[M <: MethodLike, F <: FieldLike] extends TypeLike[M, F]
