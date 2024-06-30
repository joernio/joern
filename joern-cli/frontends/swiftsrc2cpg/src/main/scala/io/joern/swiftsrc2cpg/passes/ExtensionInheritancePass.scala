package io.joern.swiftsrc2cpg.passes

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.TypeDecl
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.PropertyNames
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language.*

import java.io.File

class ExtensionInheritancePass(cpg: Cpg) extends ForkJoinParallelCpgPass[TypeDecl](cpg) {

  private val SourceFolder: String = "/Source/"

  override def generateParts(): Array[TypeDecl] = cpg.typeDecl.filterNot(_.fullName.endsWith("<extension>")).toArray

  private def setInheritsFromFullNames(
    builder: DiffGraphBuilder,
    part: TypeDecl,
    fromExtensions: Iterator[TypeDecl]
  ): Unit = {
    val fullNames = (part.inheritsFromTypeFullName ++ fromExtensions.map(_.fullName)).toSet.toSeq
    builder.setNodeProperty(part, PropertyNames.INHERITS_FROM_TYPE_FULL_NAME, fullNames)
    cpg.typ.fullNameExact(fullNames*).foreach(tgt => builder.addEdge(part, tgt, EdgeTypes.INHERITS_FROM))
  }

  override def runOnPart(builder: DiffGraphBuilder, part: TypeDecl): Unit = {
    val folderOption = Option(new File(part.filename).getParent)
    if (folderOption.isEmpty) {
      val extensions = cpg.typeDecl.filter(_.fullName.endsWith(s":${part.name}<extension>"))
      setInheritsFromFullNames(builder, part, extensions)
    } else {
      val folder = s"${folderOption.get}/".replaceAll("\\\\", "/")
      val folderPrefix = if (folder.contains(SourceFolder)) {
        folder.substring(0, folder.indexOf(SourceFolder))
      } else {
        ""
      }
      val extensions = cpg.typeDecl.filter(t =>
        t.fullName.startsWith(folderPrefix) && t.fullName.endsWith(s":${part.name}<extension>")
      )
      setInheritsFromFullNames(builder, part, extensions)
    }
  }

}
