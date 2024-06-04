package io.joern.x2cpg.passes.frontend

import io.joern.x2cpg.passes.frontend.TypeNodePass.fullToShortName
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.NewType
import io.shiftleft.passes.{KeyPool, CpgPass}
import io.shiftleft.semanticcpg.language._
import io.shiftleft.codepropertygraph.generated.PropertyNames

import scala.collection.mutable
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

/** Creates a `TYPE` node for each type in `usedTypes` as well as all inheritsFrom type names in the CPG
  *
  * Alternatively, set `getTypesFromCpg = true`. If this is set, the `registeredTypes` argument will be ignored.
  * Instead, type nodes will be created for every unique `TYPE_FULL_NAME` value in the CPG.
  */
class TypeNodePass protected (
  registeredTypes: List[String],
  cpg: Cpg,
  keyPool: Option[KeyPool],
  getTypesFromCpg: Boolean
) extends CpgPass(cpg, "types", keyPool) {

  protected def typeDeclTypes: mutable.Set[String] = {
    val typeDeclTypes = mutable.Set[String]()
    cpg.typeDecl.foreach { typeDecl =>
      typeDeclTypes += typeDecl.fullName
      typeDeclTypes ++= typeDecl.inheritsFromTypeFullName
    }
    typeDeclTypes
  }

  protected def typeFullNamesFromCpg: Set[String] = {
    cpg.all
      .map(_.property(PropertyNames.TYPE_FULL_NAME))
      .filter(_ != null)
      .map(_.toString)
      .toSet
  }

  protected def fullToShortName(typeName: String): String = TypeNodePass.fullToShortName(typeName)

  override def run(diffGraph: DiffGraphBuilder): Unit = {
    val typeFullNameValues =
      if (getTypesFromCpg)
        typeFullNamesFromCpg
      else
        registeredTypes.toSet

    val usedTypesSet = typeDeclTypes ++ typeFullNameValues
    usedTypesSet.remove("<empty>")
    val usedTypes =
      (usedTypesSet.filterInPlace(!_.endsWith(NamespaceTraversal.globalNamespaceName)).toArray :+ "ANY").toSet.sorted

    usedTypes.foreach { typeName =>
      val shortName = fullToShortName(typeName)
      val node = NewType()
        .name(shortName)
        .fullName(typeName)
        .typeDeclFullName(typeName)
      diffGraph.addNode(node)
    }
  }
}

object TypeNodePass {
  def withTypesFromCpg(cpg: Cpg, keyPool: Option[KeyPool] = None): TypeNodePass = {
    new TypeNodePass(Nil, cpg, keyPool, getTypesFromCpg = true)
  }

  def withRegisteredTypes(registeredTypes: List[String], cpg: Cpg, keyPool: Option[KeyPool] = None): TypeNodePass = {
    new TypeNodePass(registeredTypes, cpg, keyPool, getTypesFromCpg = false)
  }

  def fullToShortName(typeName: String): String = {
    typeName.takeWhile(_ != ':').split('.').lastOption.getOrElse(typeName)
  }
}
