package io.joern.rubysrc2cpg.datastructures

import io.joern.x2cpg.datastructures.{FieldLike, MethodLike, ProgramSummary, TypeLike}

class RubyProgramSummary(initialMap: Map[String, Set[RubyType]]) extends ProgramSummary[RubyType] {

  override val namespaceToType: Map[String, Set[RubyType]] = initialMap

}

case class RubyMethod(name: String, parameterTypes: List[(String, String)], returnType: String) extends MethodLike

case class RubyField(name: String, typeName: String) extends FieldLike

case class RubyType(name: String, methods: List[RubyMethod], fields: List[RubyField])
    extends TypeLike[RubyMethod, RubyField]
