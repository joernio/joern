package io.joern.x2cpg.passes.frontend

import io.joern.x2cpg.passes.base.TypeDeclStubCreator
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, PropertyNames}
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language.*

import java.io.File
import java.nio.file.Paths
import java.util.regex.{Matcher, Pattern}

/** Using some basic heuristics, will try to resolve type full names from types found within the CPG. Requires
  * ImportPass as a pre-requisite.
  */
abstract class XInheritanceFullNamePass(cpg: Cpg) extends ForkJoinParallelCpgPass[TypeDecl](cpg) {

  protected val pathSep: Char       = '.'
  protected val fileModuleSep: Char = ':'
  protected val moduleName: String
  protected val fileExt: String

  private val relativePathPattern = Pattern.compile("^[.]+/?.*")

  override def generateParts(): Array[TypeDecl] =
    cpg.typeDecl
      .filterNot(t => inheritsNothingOfInterest(t.inheritsFromTypeFullName))
      .toArray

  override def runOnPart(builder: DiffGraphBuilder, source: TypeDecl): Unit = {
    val resolvedTypeDecls = resolveInheritedTypeFullName(source, builder)
    if (resolvedTypeDecls.nonEmpty) {
      val fullNames = resolvedTypeDecls.map(_.fullName)
      builder.setNodeProperty(source, PropertyNames.INHERITS_FROM_TYPE_FULL_NAME, fullNames)
      cpg.typ.fullNameExact(fullNames*).foreach(tgt => builder.addEdge(source, tgt, EdgeTypes.INHERITS_FROM))
    }
  }

  protected def inheritsNothingOfInterest(inheritedTypes: Seq[String]): Boolean =
    inheritedTypes == Seq("ANY") || inheritedTypes == Seq("object") || inheritedTypes.isEmpty

  private def extractTypeDeclFromNode(node: AstNode): Option[String] = node match {
    case x: Call if x._isCallForImportOut.nonEmpty =>
      x.isCallForImportOut.importedEntity.map {
        case imp if relativePathPattern.matcher(imp).matches() =>
          imp.split(pathSep).toList match {
            case head :: next =>
              (Paths.get(head).normalize().toString +: next).mkString(pathSep.toString)
            case Nil =>
              Paths.get(imp).normalize().toString
          }
        case imp => imp
      }.headOption
    case x: TypeDecl => Option(x.fullName)
    case _           => None
  }

  protected def resolveInheritedTypeFullName(td: TypeDecl, builder: DiffGraphBuilder): Seq[TypeDeclBase] = {
    val callsOfInterest     = td.file.method.flatMap(_._callViaContainsOut)
    val typeDeclsOfInterest = td.file.typeDecl
    val qualifiedNamesInScope = (callsOfInterest ++ typeDeclsOfInterest)
      .flatMap(extractTypeDeclFromNode)
      .filterNot(_.endsWith(moduleName))
      .l
    val matchersInScope = qualifiedNamesInScope.map {
      case x if x.contains(pathSep) =>
        val splitName = x.split(pathSep)
        splitName.last
      case x => x
    }.distinct
    val validTypeDecls = cpg.typeDecl.nameExact(matchersInScope*).toList
    val filteredTypes  = validTypeDecls.filter(vt => td.inheritsFromTypeFullName.contains(vt.name))
    if (filteredTypes.isEmpty) {
      // Usually in the case of inheriting external types
      qualifiedNamesInScope
        .flatMap { qn =>
          td.inheritsFromTypeFullName.find(t => ImportStringHandling.namesIntersect(qn, t, pathSep)) match {
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
    cpg.typeDecl.fullNameExact(fullName).headOption match {
      case Some(typeDecl) => typeDecl
      case None =>
        val typeDecl = TypeDeclStubCreator.createTypeDeclStub(name, fullName)
        builder.addNode(typeDecl)
        typeDecl
    }

  /** Converts types in the form `foo.bar.Baz` to `foo/bar.js::program:Baz` and will result in a tuple of name and full
    * name.
    */
  protected def xTypeFullName(importedType: String, importedPath: String): (String, String) = {
    val combinedPath = ImportStringHandling.combinedPath(importedType, importedPath, pathSep)
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

object ImportStringHandling {
  def namesIntersect(a: String, b: String, pathSep: Char = '.'): Boolean = {
    val (as, bs, intersect) = splitAndIntersect(a, b, pathSep)
    intersect.nonEmpty && (as.endsWith(intersect) || bs.endsWith(intersect))
  }

  private def splitAndIntersect(a: String, b: String, pathSep: Char = '.'): (Seq[String], Seq[String], Seq[String]) = {
    val as = a.split(pathSep).toIndexedSeq
    val bs = b.split(pathSep).toIndexedSeq
    (as, bs, as.intersect(bs))
  }

  def combinedPath(importedType: String, importedPath: String, pathSep: Char = '.'): String = {
    val (a, b) =
      if (importedType.length > importedPath.length)
        (importedType, importedPath)
      else
        (importedPath, importedType)
    val (as, bs, intersect) = splitAndIntersect(a, b, pathSep)

    if (a == importedPath) bs.diff(intersect).concat(as).mkString(pathSep.toString)
    else as.diff(intersect).concat(bs).mkString(pathSep.toString)
  }
}
