package io.joern.x2cpg.frontendspecific.rubysrc2cpg

import io.joern.x2cpg.Defines
import io.joern.x2cpg.frontendspecific.rubysrc2cpg.Constants.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{Cpg, DispatchTypes, EdgeTypes, Operators}
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes.FieldAccess
import org.apache.commons.text.CaseUtils

import java.util.regex.Pattern
import scala.collection.mutable

/** A 3-tuple holding the (name, fullName, importPath) for types in the analysis.
  */
type TypeImportInfo = (String, String, String)

/** In some Ruby frameworks, it is common to have an autoloader library that implicitly loads requirements onto the
  * stack. This pass makes these imports explicit. The most popular one is <a
  * href="https://github.com/fxn/zeitwerk">Zeitwerk</a> which we check in `Gemsfile.lock` to enable this pass.
  *
  * @param externalTypes
  *   a list of additional types to consider that may be importable but are not in the CPG.
  */
class ImplicitRequirePass(cpg: Cpg, externalTypes: Seq[TypeImportInfo] = Nil)
    extends ForkJoinParallelCpgPass[Method](cpg) {

  /** A 3-tuple holding the (name, fullName, importPath, isExternal) for types in the analysis.
    */
  private type TypeImportInfoInternal = (String, String, String, Boolean)

  private val Require: String      = "require"
  private val Self: String         = "self"
  private val typeNameToImportInfo = mutable.Map.empty[String, Seq[TypeImportInfoInternal]]

  override def init(): Unit = {
    cpg.typeDecl
      .isExternal(false)
      .filter { typeDecl =>
        // zeitwerk will match types that share the name of the path.
        // This match is insensitive to camel case, i.e, foo_bar will match type FooBar.
        val fileName = typeDecl.filename.split(Array('/', '\\')).last.stripSuffix(".rb")
        val typeName = typeDecl.name
        typeName == fileName || typeName == CaseUtils.toCamelCase(fileName, true, '_', '-')
      }
      .flatMap(getChildrenTypes(_, None))
      .groupBy(_._1)
      .foreach { case (typeName, typeImportInfos) =>
        typeNameToImportInfo.put(typeName, typeImportInfos)
      }
    typeNameToImportInfo.addAll(externalTypes.map(x => (x._1, x._2, x._3, true)).groupBy(_._1))
  }

  private def typeToTypeImportInfo(typeDecl: TypeDecl, prependWith: Option[String]): Seq[TypeImportInfoInternal] = {
    val typeName = typeDecl.name
    if (typeName.endsWith("<class>")) {
      Nil
    } else {
      // We add both "name" variants, i.e., for Foo::Bar, we have both Bar and Foo::Bar
      (typeName +: prependWith.map(prefix => s"$prefix.${typeDecl.name}").toList).map(qualifiedTypeName =>
        (qualifiedTypeName, typeDecl.fullName, normalizePath(typeDecl.filename), typeDecl.isExternal)
      )
    }
  }

  private def getChildrenTypes(typeDecl: TypeDecl, prependWith: Option[String] = None): Seq[TypeImportInfoInternal] = {
    val childTypes = typeDecl.astChildren.isTypeDecl.not(_.methodBinding).l
    typeToTypeImportInfo(typeDecl, prependWith) ++ childTypes
      .flatMap(getChildrenTypes(_, prependWith.map(_.concat(s".${typeDecl.name}")).orElse(Option(typeDecl.name))))
  }

  private def normalizePath(path: String): String = path.replace("\\", "/").stripSuffix(".rb")

  override def generateParts(): Array[Method] =
    cpg.method.isModule.whereNot(_.astChildren.isCall.nameExact(Require)).toArray

  /** Collects methods within a module.
    */
  private def findMethodsViaAstChildren(module: Method): Iterator[Method] = {
    // TODO For now we have to go via the full name regex because the AST is not yet linked
    // at the execution time of this pass.
    // Iterator(module) ++ module.astChildren.flatMap {
    //  case x: TypeDecl => x.method.flatMap(findMethodsViaAstChildren)
    //  case x: Method   => Iterator(x) ++ x.astChildren.collectAll[Method].flatMap(findMethodsViaAstChildren)
    //  case _           => Iterator.empty
    // }
    cpg.method.fullName(Pattern.quote(module.fullName) + ".*")
  }

  override def runOnPart(builder: DiffGraphBuilder, moduleMethod: Method): Unit = {
    val possiblyImportedSymbols = mutable.ArrayBuffer.empty[String]
    val currPath                = normalizePath(moduleMethod.filename)

    val typeDecl = cpg.typeDecl.fullName(Pattern.quote(moduleMethod.fullName) + ".*").l
    typeDecl.inheritsFromTypeFullName.filterNot(_.endsWith("<class>")).foreach(possiblyImportedSymbols.append)

    val methodsOfModule = findMethodsViaAstChildren(moduleMethod).toList
    val callsOfModule   = methodsOfModule.ast.isCall.toList

    callsOfModule.map(x => x.name -> x.code).foreach(println)

    val symbolsGatheredFromCalls = callsOfModule.flatMap {
      case x if x.name == Operators.alloc =>
        // TODO Once constructor invocations are lowered correctly, this case is not needed anymore.
        x.argument.isIdentifier.name
      case x if x.methodFullName == Operators.fieldAccess =>
        fullyQualifiedName(x.asInstanceOf[FieldAccess]) :: Nil
      case _ =>
        Iterator.empty
    }

    possiblyImportedSymbols.appendAll(symbolsGatheredFromCalls)

    var currOrder = moduleMethod.block.astChildren.size
    possiblyImportedSymbols.distinct
      .flatMap { identifierName =>
        typeNameToImportInfo
          .getOrElse(identifierName, Seq.empty)
          .sortBy(_._4) // sorting puts false first
          .flatMap {
            case (_, _, importPath, _) if importPath == currPath =>
              None // ignore an import to a file that defines the type
            case (_, _, importPath, _) =>
              Option(importPath -> createRequireCall(builder, importPath))
          }
          .headOption
      }
      .distinctBy(_._1)
      .foreach { case (_, requireCall) =>
        requireCall.order(currOrder)
        builder.addEdge(moduleMethod.block, requireCall, EdgeTypes.AST)
        currOrder += 1
      }
  }

  private def createRequireCall(builder: DiffGraphBuilder, path: String): NewCall = {
    val requireCallNode = NewCall()
      .name(Require)
      .code(s"$Require '$path'")
      .methodFullName(s"$kernelPrefix.$Require")
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .typeFullName(Defines.Any)
    builder.addNode(requireCallNode)
    // Create literal argument
    val pathLiteralNode =
      NewLiteral().code(s"'$path'").typeFullName(s"$kernelPrefix.String").argumentIndex(1).order(2)
    builder.addEdge(requireCallNode, pathLiteralNode, EdgeTypes.AST)
    builder.addEdge(requireCallNode, pathLiteralNode, EdgeTypes.ARGUMENT)
    requireCallNode
  }

  private def fullyQualifiedName(fa: FieldAccess): String = fieldAccessParts(fa).mkString(".")

  private def fieldAccessParts(fa: FieldAccess): Seq[String] = {
    fa.argument(1) match {
      case subFa: Call if subFa.name == Operators.fieldAccess =>
        fieldAccessParts(subFa.asInstanceOf[FieldAccess]) ++ fa.fieldIdentifier.map(_.canonicalName)
      case self: Identifier if self.name == Self => fa.fieldIdentifier.map(_.canonicalName).toSeq
      case _                                     => Seq.empty
    }
  }

}
