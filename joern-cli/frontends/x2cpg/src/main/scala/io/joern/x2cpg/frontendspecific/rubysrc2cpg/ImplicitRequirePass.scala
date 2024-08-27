package io.joern.x2cpg.frontendspecific.rubysrc2cpg

import io.joern.x2cpg.Defines
import io.joern.x2cpg.frontendspecific.rubysrc2cpg.Constants.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{Cpg, DispatchTypes, EdgeTypes, Operators}
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes.{FieldAccess, Assignment}
import org.apache.commons.text.CaseUtils

import java.util.regex.Pattern
import scala.collection.mutable

/** A 3-tuple holding the (name, fullName, importPath) for types in the analysis.
  */
case class TypeImportInfo(name: String, fullName: String, importPath: String)

/** In some Ruby frameworks, it is common to have an autoloader library that implicitly loads requirements onto the
  * stack. This pass makes these imports explicit. The most popular one is <a
  * href="https://github.com/fxn/zeitwerk">Zeitwerk</a> which we check in `Gemsfile.lock` to enable this pass.
  *
  * @param externalTypes
  *   a list of additional types to consider that may be importable but are not in the CPG.
  */
class ImplicitRequirePass(cpg: Cpg, externalTypes: Seq[TypeImportInfo] = Nil)
    extends ForkJoinParallelCpgPass[Method](cpg) {

  /** A tuple holding information about the type import info, additionally with a boolean indicating if it is external
    * or not.
    */
  private type TypeImportInfoInternal = (TypeImportInfo, Boolean)
  private val typeNameToImportInfo = mutable.Map.empty[String, Seq[TypeImportInfoInternal]]

  private val Require: String    = "require"
  private val Self: String       = "self"
  private val Initialize: String = "initialize"
  private val Clazz: String      = "<class>"

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
      .groupBy { case (typeImportInfo, _) => typeImportInfo.name }
      .foreach { case (typeName, typeImportInfos) =>
        typeNameToImportInfo.put(typeName, typeImportInfos)
      }
    typeNameToImportInfo.addAll(
      externalTypes.map(x => (x, true)).groupBy { case (typeImportInfo, _) => typeImportInfo.name }
    )
  }

  private def typeToTypeImportInfo(typeDecl: TypeDecl, prependWith: Option[String]): Seq[TypeImportInfoInternal] = {
    val typeName = typeDecl.name
    if (typeName.endsWith(Clazz)) {
      Nil
    } else {
      // We add both "name" variants, i.e., for Foo::Bar, we have both Bar and Foo::Bar
      val qualifiedName = prependWith.map(prefix => s"$prefix.${typeDecl.name}").toList
      (typeName +: qualifiedName).map(qualifiedTypeName =>
        val typeImportInfo = TypeImportInfo(qualifiedTypeName, typeDecl.fullName, normalizePath(typeDecl.filename))
        typeImportInfo -> typeDecl.isExternal
      )
    }
  }

  private def getChildrenTypes(typeDecl: TypeDecl, prependWith: Option[String] = None): Seq[TypeImportInfoInternal] = {
    val thisTypeImportInfo = typeToTypeImportInfo(typeDecl, prependWith)

    val childrenTypes          = typeDecl.astChildren.isTypeDecl.not(_.methodBinding).l
    val childPrefix            = prependWith.map(_.concat(s".${typeDecl.name}")).orElse(Option(typeDecl.name))
    val childrenTypeImportInfo = childrenTypes.flatMap(getChildrenTypes(_, childPrefix))
    thisTypeImportInfo ++ childrenTypeImportInfo
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
    typeDecl.inheritsFromTypeFullName.filterNot(_.endsWith(Clazz)).foreach(possiblyImportedSymbols.append)

    val methodsOfModule = findMethodsViaAstChildren(moduleMethod).toList
    val callsOfModule   = methodsOfModule.ast.isCall.toList

    val symbolsGatheredFromCalls = callsOfModule
      .flatMap {
        case x if x.name == Initialize =>
          x.receiver.headOption.flatMap {
            case x: TypeRef    => Option(x.code.replace("::", "."))
            case x: Identifier => Option(x.name)
            case x: Call if x.name == Operators.fieldAccess =>
              Option(fullyQualifiedName(x.asInstanceOf[FieldAccess]))
            case _ => None
          }.iterator
        case x if x.methodFullName == Operators.fieldAccess =>
          fullyQualifiedName(x.asInstanceOf[FieldAccess]) :: Nil
        case _ =>
          Iterator.empty
      }
      .filterNot(_.isBlank)

    possiblyImportedSymbols.appendAll(symbolsGatheredFromCalls)

    var currOrder = moduleMethod.block.astChildren.size
    possiblyImportedSymbols.distinct
      .flatMap { identifierName =>
        typeNameToImportInfo
          .getOrElse(identifierName, Seq.empty)
          .sortBy { case (_, isExternal) => isExternal } // sorting booleans puts false (internal) first
          .collectFirst {
            // ignore an import to a file that defines the type
            case (TypeImportInfo(_, _, importPath), _) if importPath != currPath =>
              importPath -> createRequireCall(builder, importPath)
          }
      }
      .distinctBy(x => x._1)
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
      case assignCall: Call if assignCall.name == Operators.assignment =>
        val assign = assignCall.asInstanceOf[Assignment]
        // Handle the tmp var assign of qualified names
        (assign.target, assign.source) match {
          case (lhs: Identifier, rhs: Call) if lhs.name.startsWith("<tmp-") && rhs.name == Operators.fieldAccess =>
            fieldAccessParts(rhs.asInstanceOf[FieldAccess])
          case _ => Seq.empty
        }
      case _ => Seq.empty
    }
  }

}
