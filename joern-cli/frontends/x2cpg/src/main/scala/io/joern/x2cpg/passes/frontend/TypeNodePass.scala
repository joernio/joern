package io.joern.x2cpg.passes.frontend

import io.joern.x2cpg.passes.frontend.TypeNodePass.fullToShortName
import io.shiftleft.codepropertygraph.Cpg
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
  // Lambda typeDecl type names fit the structure
  // `a.b.c.d.ClassName.lambda$method$name:returnType(paramTypes)`
  // so this regex works by greedily matching the package and class names
  // at the start and cutting off the matched group before the signature.
  private val lambdaTypeRegex = raw".*\.(.*):.*\(.*\)".r

  def withTypesFromCpg(cpg: Cpg, keyPool: Option[KeyPool] = None): TypeNodePass = {
    new TypeNodePass(Nil, cpg, keyPool, getTypesFromCpg = true)
  }

  def withRegisteredTypes(registeredTypes: List[String], cpg: Cpg, keyPool: Option[KeyPool] = None): TypeNodePass = {
    new TypeNodePass(registeredTypes, cpg, keyPool, getTypesFromCpg = false)
  }

  def fullToShortName(typeName: String): String = {
    typeName match {
      case lambdaTypeRegex(methodName)                                   => methodName
      case name if name.endsWith(":program")                             => ":program" // for JavaScript only atm.
      case name if name.endsWith(NamespaceTraversal.globalNamespaceName) => NamespaceTraversal.globalNamespaceName
      case name if name.contains(":") => name.split(':').lastOption.getOrElse(typeName)
      case _                          => typeName.split('.').lastOption.getOrElse(typeName)
    }
  }
}
