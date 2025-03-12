package io.joern.x2cpg

object Defines {

  // Represents an unresolved type, or indicates that one cannot guarantee a single type
  // for the given node.
  val Any = "ANY"

  // The following two defines should be used for type and method full names to
  // indicate unresolved static type information. Using them enables
  // the closed source backend to apply policies in a less strict fashion.
  // The most notable case is the METHOD_FULL_NAME property of a CALL node.
  // As example consider a call to a method `foo(someArg)` which cannot be
  // resolved. The METHOD_FULL_NAME should be given as
  // "<unresolvedNamespace>.foo:<unresolvedSignature>(1)". If the namespace is known
  // the METHOD_FULL_NAME should be given as
  // "some.namespace.foo:<unresolvedSignature>(1)". Thereby the number in parenthesis
  // is the number of call arguments.
  // Note that this schema and thus the defines only makes sense for statically
  // typed languages with a package/namespace structure like Java, CSharp, etc..
  val UnresolvedNamespace = "<unresolvedNamespace>"
  val UnresolvedSignature = "<unresolvedSignature>"

  // Name of the synthetic, static method that contains the initialization of member variables.
  val StaticInitMethodName = "<clinit>"

  // Name of the constructor.
  val ConstructorMethodName = "<init>"

  // In some languages like Javascript dynamic calls do not provide any statically known
  // method/function interface information. In those cases please use this value.
  val DynamicCallUnknownFullName = "<unknownFullName>"

  // Anonymous functions, lambdas, and closures, follow the naming scheme of $LambdaPrefix$int
  val ClosurePrefix = "<lambda>"

  val LeftAngularBracket = "<"
  val Unknown            = "<unknown>"

  // Used for field access calls in the lowering of pattern extractors where the field name
  // may not be known. As an example in javasrc2cpg, the assignment for `o instanceof Foo(Bar b))` could
  // be lowered to `Bar b = (Bar) (((Foo) o).<unknownField>)`
  val UnknownField = "<unknownField>"
}
