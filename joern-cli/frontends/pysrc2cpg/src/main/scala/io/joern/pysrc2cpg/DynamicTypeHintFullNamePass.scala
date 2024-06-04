package io.joern.pysrc2cpg

import io.joern.x2cpg.passes.frontend.ImportStringHandling
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.PropertyNames
import io.shiftleft.codepropertygraph.generated.nodes.{CfgNode, MethodParameterIn, MethodReturn, StoredNode}
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language._
import overflowdb.BatchedUpdate

import java.io.File
import java.util.regex.{Matcher, Pattern}

/** The type hints we pick up via the parser are not full names. This pass fixes that by retrieving the import for each
  * dynamic type hint and adjusting the dynamic type hint full name field accordingly.
  */
class DynamicTypeHintFullNamePass(cpg: Cpg) extends ForkJoinParallelCpgPass[CfgNode](cpg) {

  private case class ImportScope(entity: Option[String], alias: Option[String])

  private val fileToImports = cpg.imports.l
    .flatMap(imp => imp.call.file.l.map { f => f.name -> imp })
    .groupBy(_._1)
    .view
    .mapValues(_.map { case (_, imp) =>
      ImportScope(imp.importedEntity, imp.importedAs)
    })

  override def generateParts(): Array[CfgNode] =
    (cpg.methodReturn.filter(x => x.typeFullName != Constants.ANY) ++ cpg.parameter.filter(x =>
      x.typeFullName != Constants.ANY
    )).toArray

  override def runOnPart(builder: DiffGraphBuilder, part: CfgNode): Unit =
    part match {
      case x: MethodReturn      => runOnMethodReturn(builder, x)
      case x: MethodParameterIn => runOnMethodParameter(builder, x)
      case _                    =>
    }

  private def runOnMethodReturn(diffGraph: DiffGraphBuilder, methodReturn: MethodReturn): Unit =
    methodReturn.file.foreach { file =>
      val typeHint = methodReturn.typeFullName
      val imports = fileToImports.getOrElse(file.name, List.empty) ++ methodReturn.method.typeDecl
        .map(td => ImportScope(Option(pythonicTypeNameToImport(td.fullName)), Option(td.name)))
        .toList
      imports
        .filter { x =>
          // TODO: Handle * imports correctly
          x.alias.exists { imported => typeHint.matches(Pattern.quote(imported) + "(\\..+)*") }
        }
        .flatMap(_.entity)
        .foreach { importedEntity =>
          setTypeHints(diffGraph, methodReturn, typeHint, typeHint, importedEntity)
        }
    }

  private def runOnMethodParameter(diffGraph: DiffGraphBuilder, param: MethodParameterIn): Unit =
    param.file.foreach { file =>
      val typeHint = param.typeFullName
      val imports = fileToImports.getOrElse(file.name, List.empty) ++ param.method.typeDecl
        .map(td => ImportScope(Option(pythonicTypeNameToImport(td.fullName)), Option(td.name)))
        .toList
      imports
        // TODO: Handle * imports correctly
        .filter(_.alias.exists { imported => typeHint.matches(Pattern.quote(imported) + "(\\..+)*") })
        .foreach {
          case ImportScope(Some(importedEntity), Some(importedAs)) =>
            setTypeHints(diffGraph, param, typeHint, importedAs, importedEntity)
          case _ =>
        }
    }

  private def pythonicTypeNameToImport(fullName: String): String =
    fullName.replaceFirst("\\.py:<module>", "").replaceAll(Pattern.quote(File.separator), ".")

  private def setTypeHints(
    diffGraph: BatchedUpdate.DiffGraphBuilder,
    node: StoredNode,
    typeHint: String,
    alias: String,
    importedEntity: String
  ) = {
    val importFullPath   = ImportStringHandling.combinedPath(importedEntity, typeHint)
    val typeHintFullName = typeHint.replaceFirst(Pattern.quote(alias), importedEntity)
    val typeFilePath     = typeHintFullName.replaceAll("\\.", Matcher.quoteReplacement(File.separator))
    val pythonicTypeFullName = importFullPath.split("\\.").lastOption match {
      case Some(typeName) =>
        typeFilePath.stripSuffix(s"${File.separator}$typeName").concat(s".py:<module>.$typeName")
      case None => typeHintFullName
    }
    cpg.typeDecl.fullName(s".*${Pattern.quote(pythonicTypeFullName)}").l match {
      case xs if xs.sizeIs == 1 =>
        diffGraph.setNodeProperty(node, PropertyNames.TYPE_FULL_NAME, xs.fullName.head)
      case xs if xs.nonEmpty =>
        diffGraph.setNodeProperty(node, PropertyNames.DYNAMIC_TYPE_HINT_FULL_NAME, xs.fullName.toSeq)
      case _ =>
        diffGraph.setNodeProperty(node, PropertyNames.TYPE_FULL_NAME, pythonicTypeFullName)
    }
  }
}
