package io.joern.x2cpg.passes.frontend

import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.nodes.NewType
import io.shiftleft.codepropertygraph.generated.{Cpg, Properties}
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

import scala.collection.mutable

/** Creates a `TYPE` node for each type in `usedTypes` as well as all inheritsFrom type names in the CPG
  *
  * Alternatively, set `getTypesFromCpg = true`. If this is set, the `registeredTypes` argument will be ignored.
  * Instead, type nodes will be created for every unique `TYPE_FULL_NAME` value in the CPG.
  */
class TypeNodePass protected (registeredTypes: Set[String], cpg: Cpg, getTypesFromCpg: Boolean)
    extends CpgPass(cpg, "types") {

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
      .map(_.property(Properties.TypeFullName))
      .filter(_ != null)
      .toSet
  }

  protected def fullToShortName(typeName: String): String = TypeNodePass.fullToShortName(typeName)

  override def run(diffGraph: DiffGraphBuilder): Unit = {
    val typeFullNameValues =
      if (getTypesFromCpg) typeFullNamesFromCpg
      else registeredTypes

    val usedTypesSet = typeDeclTypes ++ typeFullNameValues
    usedTypesSet.remove("<empty>")
    usedTypesSet.addOne(Defines.Any)

    val usedTypes = usedTypesSet.filterInPlace(!_.endsWith(NamespaceTraversal.globalNamespaceName)).sorted

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
  def withTypesFromCpg(cpg: Cpg): TypeNodePass = {
    new TypeNodePass(Set.empty, cpg, getTypesFromCpg = true)
  }

  def withRegisteredTypes(registeredTypes: Set[String], cpg: Cpg): TypeNodePass = {
    new TypeNodePass(registeredTypes, cpg, getTypesFromCpg = false)
  }

  def fullToShortName(typeName: String): String = {
    if (typeName.endsWith(">")) {
      // special case for typeFullName with generics as suffix
      typeName.takeWhile(c => c != ':' && c != '<').split('.').lastOption.getOrElse(typeName)
    } else {
      typeName.takeWhile(_ != ':').split('.').lastOption.getOrElse(typeName)
    }
  }
}
