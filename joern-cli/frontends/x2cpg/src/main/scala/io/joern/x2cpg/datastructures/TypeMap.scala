package io.joern.x2cpg.datastructures

/** A heirarchical data-structure that stores the result of types and their respective members. These types can be
  * sourced from pre-parsing the application, or pre-computed stubs of common libraries.
  *
  * The utility of this object is in assisting resolving shorthand types during AST creation.
  */
trait TypeMap[T <: TypeLike, M <: MethodLike] {

  type NamespaceToTypeMap = Map[String, Set[T]]

  /**
    * A mapping between a namespace/directory and the containing types.
    */
  protected val namespaceToType: NamespaceToTypeMap

  /** For the given namespace, returns the declared types.
    */
  def typesUnderNamespace(namespace: String): Set[T] = namespaceToType.getOrElse(namespace, Set.empty)

  /** For a type, will search for the associated namespace.
    */
  def namespaceFor(clazz: T): Option[String] = namespaceToType.find { case (_, v) => v.contains(clazz) }.map(_._1)



}

/**
  * The stubbed type declarations.
  */
trait TypeLike {
  def fullName: String
}

/**
  * A member that behaves like a field/property.
  */
trait FieldLike {
    /**
      * @return the name of the field.
      */
    def name: String

    /**
      * 
      * @return the type declared (not necessarily resolved)
      */
    def typeName: String
}

/**
  * A function or procedure.
  */
trait MethodLike {

}