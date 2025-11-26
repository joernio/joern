package io.joern.swiftsrc2cpg.passes

import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language.*

/** Pass that creates `Binding` nodes for Swift extension methods and binds them to the corresponding TypeDecl.
  *
  * This pass:
  *   - identifies methods whose `fullName` contains the marker `<extension>`,
  *   - extracts the TypeDecl fullName by taking the substring before `<extension>`,
  *   - looks up the matching `TypeDecl` in the CPG, and for each match creates a `NewBinding` node with the method
  *     name, fullName, and signature,
  *   - adds a `BINDS` edge from the `TypeDecl` to the `Binding` and a `REF` edge from the `Binding` to the extension
  *     `Method`.
  *
  * Note: `generateParts` filters methods to only those containing `<extension>`, so extracting the substring is safe.
  *
  * @param cpg
  *   the code property graph to operate on
  */
class ExtensionMethodBindingsPass(cpg: Cpg) extends ForkJoinParallelCpgPass[Method](cpg) {

  override def generateParts(): Array[Method] =
    cpg.method.filter(_.fullName.contains("<extension>")).toArray

  override def runOnPart(builder: DiffGraphBuilder, part: Method): Unit = {
    val typeDeclFullNameFromMethod = part.fullName.substring(0, part.fullName.indexOf("<extension>"))
    for {
      typeDecl <- cpg.typeDecl.fullNameExact(typeDeclFullNameFromMethod)
    } {
      val functionBinding = NewBinding().name(part.name).methodFullName(part.fullName).signature(part.signature)
      builder.addNode(functionBinding)
      builder.addEdge(typeDecl, functionBinding, EdgeTypes.BINDS)
      builder.addEdge(functionBinding, part, EdgeTypes.REF)
    }
  }

}
