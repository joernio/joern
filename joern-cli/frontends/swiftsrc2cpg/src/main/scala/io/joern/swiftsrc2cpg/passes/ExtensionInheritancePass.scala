package io.joern.swiftsrc2cpg.passes

import io.shiftleft.codepropertygraph.generated.{Cpg, PropertyNames}
import io.shiftleft.codepropertygraph.generated.nodes.TypeDecl
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language.*

/** Pass that applies inheritance relationships discovered from Swift extensions to type declarations in the CPG.
  *
  * @param cpg
  *   the code property graph to update
  * @param inheritsMapping
  *   mapping from TypeDecl fullNames to sets of extension target type fullNames to be added as inherits-from
  */
class ExtensionInheritancePass(cpg: Cpg, inheritsMapping: Map[String, Set[String]])
    extends ForkJoinParallelCpgPass[(String, Set[String])](cpg) {

  override def generateParts(): Array[(String, Set[String])] = inheritsMapping.traversal.toArray

  private def setInherits(builder: DiffGraphBuilder, typeDecl: TypeDecl, inheritFullNames: Set[String]): Unit = {
    val existingInherits = typeDecl.inheritsFromTypeFullName
    val allInherits      = existingInherits ++ inheritFullNames
    builder.setNodeProperty(typeDecl, PropertyNames.InheritsFromTypeFullName, allInherits)
  }

  /** Process a single part of the work (a mapping entry) and update the diff graph with inheritance information
    * discovered from extensions.
    *
    * @param builder
    *   the diff graph builder used to record node property changes and edges
    * @param part
    *   a tuple where the first element is the fullName of the `TypeDecl` to update and the second element is the set of
    *   extension target type names to be added as inherits-from entries
    */
  override def runOnPart(builder: DiffGraphBuilder, part: (String, Set[String])): Unit = {

    /** Resolve extension target names to TypeDecl fullNames if a matching `TypeDecl` exists.
      *
      * For each name in the set of extension target type names we look up a matching `TypeDecl` using `nameExact`. If
      * found, use its `fullName`; otherwise keep the original name. This ensures we add consistent fullNames to the
      * `inheritsFrom` property and create `INHERITS_FROM` edges to the correct `typ` nodes.
      *
      * The value is `lazy` to avoid performing lookups when the outer `for` does not find a `TypeDecl` for `part._1`.
      */
    lazy val inheritsFullNames = part._2.map { name =>
      cpg.typeDecl.nameExact(name).headOption match {
        case Some(td) => td.fullName
        case None     => name
      }
    }

    for {
      typeDecl <- cpg.typeDecl.fullNameExact(part._1)
    } setInherits(builder, typeDecl, inheritsFullNames)
  }

}
