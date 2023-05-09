package io.joern.pysrc2cpg

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.PropertyNames
import io.shiftleft.codepropertygraph.generated.nodes.StoredNode
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language._
import overflowdb.BatchedUpdate

import java.io.File
import java.util.regex.{Matcher, Pattern}

/** The type hints we pick up via the parser are not full names. This pass fixes that by retrieving the import for each
  * dynamic type hint and adjusting the dynamic type hint full name field accordingly.
  */
class DynamicTypeHintFullNamePass(cpg: Cpg) extends CpgPass(cpg) {
  override def run(diffGraph: BatchedUpdate.DiffGraphBuilder): Unit = {
    val fileToImports = cpg.imports.l
      .flatMap { imp =>
        imp.call.file.l.map { f => f.name -> imp }
      }
      .groupBy(_._1)
      .view
      .mapValues(_.map(_._2))

    for {
      methodReturn <- cpg.methodReturn.filter(x => x.typeFullName != Constants.ANY)
      file         <- methodReturn.file
      imports      <- fileToImports.get(file.name)
    } {
      val typeHint = methodReturn.typeFullName
      imports
        .filter { x =>
          // TODO: Handle * imports correctly
          x.importedAs.exists { imported => typeHint.matches(Pattern.quote(imported) + "(\\..+)*") }
        }
        .importedEntity
        .foreach { importedEntity =>
          setTypeHints(diffGraph, methodReturn, typeHint, typeHint, importedEntity)
        }
    }

    for {
      param   <- cpg.parameter.filter(x => x.typeFullName != Constants.ANY)
      file    <- param.file
      imports <- fileToImports.get(file.name)
    } {
      val typeHint = param.typeFullName
      imports
        // TODO: Handle * imports correctly
        .filter(_.importedAs.exists { imported => typeHint.matches(Pattern.quote(imported) + "(\\..+)*") })
        .map(i => (i.importedEntity, i.importedAs))
        .foreach {
          case (Some(importedEntity), Some(importedAs)) =>
            setTypeHints(diffGraph, param, typeHint, importedAs, importedEntity)
          case _ =>
        }
    }

  }

  private def setTypeHints(
    diffGraph: BatchedUpdate.DiffGraphBuilder,
    node: StoredNode,
    typeHint: String,
    alias: String,
    importedEntity: String
  ) = {
    val typeFullName = typeHint.replaceFirst(Pattern.quote(alias), importedEntity)
    val typeFilePath = typeFullName.replaceAll("\\.", Matcher.quoteReplacement(File.separator))
    val pythonicTypeFullName = typeFullName.split("\\.").lastOption match {
      case Some(typeName) =>
        typeFilePath.stripSuffix(s"${File.separator}$typeName").concat(s".py:<module>.$typeName")
      case None => typeFullName
    }
    cpg.typeDecl.fullName(s".*${Pattern.quote(pythonicTypeFullName)}").l match {
      case xs if xs.nonEmpty =>
        diffGraph.setNodeProperty(node, PropertyNames.DYNAMIC_TYPE_HINT_FULL_NAME, xs.fullName.toSeq)
      case _ => diffGraph.setNodeProperty(node, PropertyNames.TYPE_FULL_NAME, pythonicTypeFullName)
    }
  }
}
