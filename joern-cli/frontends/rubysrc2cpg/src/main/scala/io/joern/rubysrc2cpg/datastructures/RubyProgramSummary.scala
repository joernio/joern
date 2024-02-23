package io.joern.rubysrc2cpg.datastructures

import io.joern.x2cpg.datastructures.{FieldLike, MethodLike, ProgramSummary, TypeLike}
import io.joern.x2cpg.Defines as XDefines
import scala.annotation.targetName

class RubyProgramSummary(initialMap: Map[String, Set[RubyType]] = Map.empty) extends ProgramSummary[RubyType] {

  override val namespaceToType: Map[String, Set[RubyType]] = initialMap

  @targetName("add")
  def ++(other: RubyProgramSummary): RubyProgramSummary = {
    RubyProgramSummary(ProgramSummary.combine(this.namespaceToType, other.namespaceToType))
  }

}

case class RubyMethod(name: String, parameterTypes: List[(String, String)], returnType: String) extends MethodLike

case class RubyField(name: String, typeName: String) extends FieldLike

case class RubyType(name: String, methods: List[RubyMethod], fields: List[RubyField])
    extends TypeLike[RubyMethod, RubyField] {
  def hasConstructor: Boolean = {
    methods.exists(_.name == XDefines.ConstructorMethodName)
  }
}
