package io.joern.rubysrc2cpg.datastructures

import io.joern.x2cpg.Defines as XDefines
import io.joern.x2cpg.datastructures.{FieldLike, MethodLike, ProgramSummary, TypeLike}

import scala.annotation.targetName
import upickle.default.*

class RubyProgramSummary(
  initialNamespaceMap: Map[String, Set[RubyType]] = Map.empty,
  initialPathMap: Map[String, Set[RubyType]] = Map.empty
) extends ProgramSummary[RubyType] {

  override val namespaceToType: Map[String, Set[RubyType]] = initialNamespaceMap
  val pathToType: Map[String, Set[RubyType]]               = initialPathMap

  @targetName("add")
  def ++(other: RubyProgramSummary): RubyProgramSummary = {
    RubyProgramSummary(
      ProgramSummary.combine(this.namespaceToType, other.namespaceToType),
      ProgramSummary.combine(this.pathToType, other.pathToType)
    )
  }

}


case class RubyMethod(
  name: String,
  parameterTypes: List[(String, String)],
  returnType: String,
  baseTypeFullName: Option[String]
) extends MethodLike

object RubyMethod {
  implicit val rubyMethodRwJson: ReadWriter[RubyMethod] = readwriter[ujson.Value].bimap[RubyMethod](
    x => ujson.Obj(
      "name" -> x.name,
    ),
    json => RubyMethod(json("name").str, List.empty, "", Option.empty)
  )
}

case class RubyField(name: String, typeName: String) extends FieldLike derives ReadWriter

case class RubyType(name: String, methods: List[RubyMethod], fields: List[RubyField])
    extends TypeLike[RubyMethod, RubyField] derives ReadWriter {

  @targetName("add")
  override def +(o: TypeLike[RubyMethod, RubyField]): TypeLike[RubyMethod, RubyField] = {
    this.copy(methods = mergeMethods(o), fields = mergeFields(o))
  }

  def hasConstructor: Boolean = {
    methods.exists(_.name == XDefines.ConstructorMethodName)
  }
}
