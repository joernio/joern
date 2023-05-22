package io.joern.x2cpg.passes.frontend

import io.joern.x2cpg.passes.base.TypeDeclStubCreator
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, PropertyNames}
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language._

import java.io.File
import java.nio.file.Paths
import java.util.regex.{Matcher, Pattern}
import scala.collection.mutable

/** Using some basic heuristics, will try to resolve type full names from types found within the CPG. Requires
  * ImportPass as a pre-requisite.
  */
abstract class XInheritanceFullNamePass(cpg: Cpg) extends CpgPass(cpg) {

  protected val pathSep: Char       = '.'
  protected val fileModuleSep: Char = ':'
  protected val moduleName: String
  protected val fileExt: String

  protected val typeDeclMap: mutable.Map[String, TypeDeclBase] =
    mutable.HashMap.from[String, TypeDeclBase](cpg.typeDecl.map(t => t.fullName -> t))
  protected val typeMap: mutable.Map[String, TypeBase] =
    mutable.HashMap.from[String, TypeBase](cpg.typ.map(t => t.fullName -> t))

  override def run(builder: DiffGraphBuilder): Unit = {
    cpg.typeDecl
      .filterNot(t => inheritsNothingOfInterest(t.inheritsFromTypeFullName))
      .foreach { t =>
        val resolvedTypeDecls = resolveInheritedTypeFullName(t, builder)
        if (resolvedTypeDecls.nonEmpty) {
          val fullNames = resolvedTypeDecls.map(_.fullName)
          builder.setNodeProperty(t, PropertyNames.INHERITS_FROM_TYPE_FULL_NAME, fullNames)
          fullNames.flatMap(typeMap.get).foreach(tgt => builder.addEdge(t, tgt, EdgeTypes.INHERITS_FROM))
        }
      }
  }

  protected def inheritsNothingOfInterest(inheritedTypes: Seq[String]): Boolean =
    inheritedTypes == Seq("ANY") || inheritedTypes == Seq("object") || inheritedTypes.isEmpty

  protected def resolveInheritedTypeFullName(td: TypeDecl, builder: DiffGraphBuilder): Seq[TypeDeclBase] = {
    val qualifiedNamesInScope = td.file.ast
      .flatMap {
        case x: Call if x.isCallForImportOut.nonEmpty =>
          x.isCallForImportOut.importedEntity.map {
            case imp if imp.matches("^[.]+/?.*") =>
              imp.split(pathSep).toList match {
                case ::(head, next) =>
                  (Paths.get(head).normalize().toString +: next).mkString(pathSep.toString)
                case Nil =>
                  Paths.get(imp).normalize().toString
              }
            case imp => imp
          }
        case x: TypeDecl if typeDeclMap.contains(x.fullName) => Option(x.fullName)
        case _                                               => None
      }
      .filterNot(_.endsWith(moduleName))
      .l
    val matchersInScope = qualifiedNamesInScope.map {
      case x if x.contains(pathSep) =>
        val splitName = x.split(pathSep)
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
        .map { case (qualifiedName, inheritedNames) => xTypeFullName(qualifiedName, inheritedNames) }
        .map { case (name, fullName) => createTypeStub(name, fullName, builder) }
    } else {
      filteredTypes
    }
  }

  private def createTypeStub(name: String, fullName: String, builder: DiffGraphBuilder): TypeDeclBase =
    typeDeclMap.get(fullName) match {
      case Some(typeDecl) => typeDecl
      case None =>
        val typeDecl = TypeDeclStubCreator.createTypeDeclStub(name, fullName)
        builder.addNode(typeDecl)
        typeDeclMap.put(fullName, typeDecl)
        typeDecl
    }

  private def namesIntersect(a: String, b: String): Boolean = {
    val (as, bs, intersect) = splitAndIntersect(a, b)
    intersect.nonEmpty && (as.endsWith(intersect) || bs.endsWith(intersect))
  }

  private def splitAndIntersect(a: String, b: String): (Seq[String], Seq[String], Seq[String]) = {
    val as = a.split(pathSep).toIndexedSeq
    val bs = b.split(pathSep).toIndexedSeq
    (as, bs, as.intersect(bs))
  }

  /** Converts types in the form `foo.bar.Baz` to `foo/bar.js::program:Baz` and will result in a tuple of name and full
    * name.
    */
  protected def xTypeFullName(importedType: String, importedPath: String): (String, String) = {
    val (a, b) =
      if (importedType.length > importedPath.length)
        (importedType, importedPath)
      else
        (importedPath, importedType)
    val (as, bs, intersect) = splitAndIntersect(a, b)
    val combinedPath =
      if (a == importedPath) bs.diff(intersect).concat(as).mkString(pathSep.toString)
      else as.diff(intersect).concat(bs).mkString(pathSep.toString)
    combinedPath.split(pathSep).lastOption match {
      case Some(tName) =>
        (
          tName,
          combinedPath
            .stripSuffix(s"$pathSep$tName")
            .replaceAll(s"${Pattern.quote(pathSep.toString)}", Matcher.quoteReplacement(File.separator)) +
            Seq(s"$fileExt$fileModuleSep$moduleName", tName).mkString(pathSep.toString)
        )
      case None => (combinedPath, combinedPath)
    }

  }
}
