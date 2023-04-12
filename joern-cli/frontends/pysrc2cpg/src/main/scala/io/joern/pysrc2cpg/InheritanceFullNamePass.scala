package io.joern.pysrc2cpg

import io.joern.x2cpg.passes.base.TypeDeclStubCreator
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Call, TypeBase, TypeDecl, TypeDeclBase}
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, PropertyNames}
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language._

import java.io.File
import java.util.regex.{Matcher, Pattern}
import scala.collection.mutable

/** Using some basic heuristics, will try to resolve type full names from types found within the CPG. Requires
  * ImportPass as a pre-requisite.
  */
class InheritanceFullNamePass(cpg: Cpg) extends CpgPass(cpg) {

  private val typeDeclMap =
    mutable.HashMap.from[String, TypeDeclBase](cpg.typeDecl.where(_.nameNot(".*<.*>$")).map(t => t.fullName -> t))
  private val typeMap =
    mutable.HashMap.from[String, TypeBase](cpg.typ.where(_.nameNot(".*<.*>$")).map(t => t.fullName -> t))

  override def run(builder: DiffGraphBuilder): Unit = {
    cpg.typeDecl
      .filterNot(t =>
        t.inheritsFromTypeFullName == Seq("ANY") || t.inheritsFromTypeFullName == Seq(
          "object"
        ) || t.inheritsFromTypeFullName.isEmpty
      )
      .foreach { t =>
        val resolvedTypeDecls = resolveInheritedTypeFullName(t, builder)
        if (resolvedTypeDecls.nonEmpty) {
          val fullNames = resolvedTypeDecls.map(_.fullName)
          builder.setNodeProperty(t, PropertyNames.INHERITS_FROM_TYPE_FULL_NAME, fullNames)
          fullNames.flatMap(typeMap.get).foreach(tgt => builder.addEdge(t, tgt, EdgeTypes.INHERITS_FROM))
        }
      }
  }

  private def resolveInheritedTypeFullName(td: TypeDecl, builder: DiffGraphBuilder): Seq[TypeDeclBase] = {
    val qualifiedNamesInScope = td.file.ast
      .flatMap {
        case x: Call if x.isCallForImportOut.nonEmpty        => x.isCallForImportOut.importedEntity
        case x: TypeDecl if typeDeclMap.contains(x.fullName) => Option(x.fullName)
        case _                                               => None
      }
      .filterNot(_.endsWith("<module>"))
      .l
    val matchersInScope = qualifiedNamesInScope.map {
      case x if x.contains(".") =>
        val splitName = x.split("\\.")
        s".*${Pattern.quote(splitName.head)}.*${Pattern.quote(splitName.last)}"
      case x => s".*${Pattern.quote(x)}"
    }.distinct
    val validTypeDecls = typeDeclMap.filter { case (name, _) => matchersInScope.exists(m => name.matches(m)) }.values.l
    val filteredTypes  = validTypeDecls.filter(vt => td.inheritsFromTypeFullName.contains(vt.name))
    if (filteredTypes.isEmpty) {
      // Usually in the case of inheriting external types
      qualifiedNamesInScope
        .flatMap { qn =>
          td.inheritsFromTypeFullName.find(t => namesIntersect(qn, t)) match {
            case Some(path) => Option((qn, path))
            case None       => None
          }
        }
        .map { case (qualifiedName, inheritedNames) => pythonicTypeFullName(qualifiedName, inheritedNames) }
        .map { case (name, fullName) =>
          typeDeclMap.get(fullName) match {
            case Some(typeDecl) => typeDecl
            case None =>
              val typeDecl = TypeDeclStubCreator.createTypeDeclStub(name, fullName)
              builder.addNode(typeDecl)
              typeDeclMap.put(fullName, typeDecl)
              typeDecl
          }

        }
    } else {
      filteredTypes
    }
  }

  private def namesIntersect(a: String, b: String): Boolean = {
    val (as, bs, intersect) = splitAndIntersect(a, b)
    intersect.nonEmpty && (as.endsWith(intersect) || bs.endsWith(intersect))
  }

  private def splitAndIntersect(a: String, b: String): (Seq[String], Seq[String], Seq[String]) = {
    val as = a.split("\\.")
    val bs = b.split("\\.")
    (as, bs, as.intersect(bs))
  }

  /** Converts types in the form `foo.bar.Baz` to `foo/bar.py:<module>.Baz` and will result in a tuple of name and full
    * name.
    */
  private def pythonicTypeFullName(importedType: String, importedPath: String): (String, String) = {
    val (a, b) =
      if (importedType.length > importedPath.length)
        (importedType, importedPath)
      else
        (importedPath, importedType)
    val (as, bs, intersect) = splitAndIntersect(a, b)
    val combinedPath =
      if (a == importedPath) bs.diff(intersect).concat(as).mkString(".")
      else as.diff(intersect).concat(bs).mkString(".")
    combinedPath.split("\\.").lastOption match {
      case Some(tName) =>
        (
          tName,
          combinedPath
            .stripSuffix(s".$tName")
            .replaceAll("\\.", Matcher.quoteReplacement(File.separator)) + s".py:<module>.$tName"
        )
      case None => (combinedPath, combinedPath)
    }

  }

}
