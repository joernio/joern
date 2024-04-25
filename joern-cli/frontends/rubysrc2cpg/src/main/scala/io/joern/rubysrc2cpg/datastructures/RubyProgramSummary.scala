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
    x => ujson.Obj("name" -> x.name),
    json =>
      RubyMethod(
        name = json("name").str,
        parameterTypes = List.empty,
        returnType = XDefines.Any,
        baseTypeFullName = Option(json("name").str.split("\\.").dropRight(1).mkString("."))
      )
  )
}

case class RubyField(name: String, typeName: String) extends FieldLike derives ReadWriter

case class RubyType(name: String, methods: List[RubyMethod], fields: List[RubyField])
    extends TypeLike[RubyMethod, RubyField] {

  @targetName("add")
  override def +(o: TypeLike[RubyMethod, RubyField]): TypeLike[RubyMethod, RubyField] = {
    this.copy(methods = mergeMethods(o), fields = mergeFields(o))
  }

  def hasConstructor: Boolean = {
    methods.exists(_.name == XDefines.ConstructorMethodName)
  }
}

object RubyType {
  implicit val rubyTypeRw: ReadWriter[RubyType] = readwriter[ujson.Value].bimap[RubyType](
    x =>
      ujson.Obj(
        "name" -> x.name,
        "methods" -> x.methods.map { method =>
          ujson.Obj("name" -> method.name)
        },
        "fields" -> x.fields.map { field => write[RubyField](field) }
      ),
    json =>
      RubyType(
        name = json("name").str,
        methods = json.obj.get("methods") match {
          case Some(jsonMethods) =>
            val methodsMap = read[collection.mutable.Map[String, RubyMethod]](jsonMethods)
            methodsMap.map { case (name, func) =>
              val splitName        = name.split("\\.")
              val baseTypeFullName = splitName.dropRight(1).mkString(".")

              func.copy(name = name, baseTypeFullName = Option(baseTypeFullName))
            }.toList
          case None => Nil
        },
        fields = json.obj.get("fields").map(read[List[RubyField]](_)).getOrElse(Nil)
      )
  )
}
