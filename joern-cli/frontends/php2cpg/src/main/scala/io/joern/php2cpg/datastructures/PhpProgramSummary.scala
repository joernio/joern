package io.joern.php2cpg.datastructures

import io.joern.x2cpg.datastructures.{FieldLike, MethodLike, ProgramSummary, TypeLike}

import scala.annotation.targetName
import scala.collection.mutable

type NamespaceToTypeMap = mutable.Map[String, mutable.Set[PhpType]]

class PhpProgramSummary(
  override val namespaceToType: NamespaceToTypeMap = mutable.Map.empty[String, mutable.Set[PhpType]]
) extends ProgramSummary[PhpType, PhpMethod, PhpField] {

  @targetName("appendAll")
  def ++=(other: PhpProgramSummary): PhpProgramSummary =
    PhpProgramSummary(ProgramSummary.merge(this.namespaceToType, other.namespaceToType))

}

case class PhpField(name: String, typeName: String) extends FieldLike

case class PhpMethod(
  name: String,
  parameterTypes: List[(String, String)],
  returnType: String,
  baseTypeFullName: Option[String]
) extends MethodLike

case class PhpType(name: String, methods: List[PhpMethod], fields: List[PhpField])
    extends TypeLike[PhpMethod, PhpField] {

  @targetName("add")
  override def +(o: TypeLike[PhpMethod, PhpField]): TypeLike[PhpMethod, PhpField] = {
    this.copy(methods = mergeMethods(o), fields = mergeFields(o))
  }
}
