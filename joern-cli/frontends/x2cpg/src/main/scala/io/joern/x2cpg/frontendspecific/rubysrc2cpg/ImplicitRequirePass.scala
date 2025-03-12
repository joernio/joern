package io.joern.x2cpg.frontendspecific.rubysrc2cpg

import io.joern.x2cpg.Defines
import io.joern.x2cpg.frontendspecific.rubysrc2cpg.Constants.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{Cpg, DispatchTypes, EdgeTypes, Operators}
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes.{Assignment, FieldAccess}
import org.apache.commons.text.CaseUtils

import java.util.regex.Pattern
import scala.annotation.tailrec
import scala.collection.mutable

/** A tuple holding the (name, importPath) for types in the analysis.
  */
case class TypeImportInfo(name: String, importPath: String)

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
  private case class TypeImportInfoWithProvidence(info: TypeImportInfo, isExternal: Boolean)
  private val typeNameToImportInfo = mutable.Map.empty[String, Seq[TypeImportInfoWithProvidence]]

  private val Require: String    = "require"
  private val Self: String       = "self"
  private val Initialize: String = "initialize"
  private val Clazz: String      = "<class>"

  override def init(): Unit = {
    val importableTypeInfo = cpg.typeDecl
      .isExternal(false)
      .filter { typeDecl =>
        // zeitwerk will match types that share the name of the path.
        // This match is insensitive to camel case, i.e, foo_bar will match type FooBar.
        val fileName = typeDecl.filename.split(Array('/', '\\')).last
        val typeName = typeDecl.name
        ImplicitRequirePass.isAutoloadable(typeName, fileName)
      }
      .map { typeDecl =>
        val typeImportInfo = TypeImportInfo(typeDecl.name, ImplicitRequirePass.normalizePath(typeDecl.filename))
        TypeImportInfoWithProvidence(typeImportInfo, typeDecl.isExternal)
      }
      .l
    // Group types by symbol and add to map for quicker retrieval later
    typeNameToImportInfo.addAll(importableTypeInfo.groupBy { case TypeImportInfoWithProvidence(typeImportInfo, _) =>
      typeImportInfo.name
    })
    typeNameToImportInfo.addAll(externalTypes.map(TypeImportInfoWithProvidence(_, true)).groupBy {
      case TypeImportInfoWithProvidence(typeImportInfo, _) => typeImportInfo.name
    })
  }

  private def getFieldBaseFromString(fieldAccessString: String): String = {
    val normalizedFieldAccessString = fieldAccessString.replace("::", ".")
    normalizedFieldAccessString.split('.').headOption.getOrElse(normalizedFieldAccessString)
  }

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
    val currPath                = ImplicitRequirePass.normalizePath(moduleMethod.filename)

    val typeDecl = cpg.typeDecl.fullName(Pattern.quote(moduleMethod.fullName) + ".*").l
    typeDecl.inheritsFromTypeFullName
      .filterNot(_.endsWith(Clazz))
      .map(getFieldBaseFromString)
      .foreach(possiblyImportedSymbols.append)

    val methodsOfModule = findMethodsViaAstChildren(moduleMethod).toList
    val callsOfModule   = methodsOfModule.ast.isCall.toList

    val symbolsGatheredFromCalls = callsOfModule
      .flatMap {
        case x if x.name == Initialize =>
          x.receiver.headOption.flatMap {
            case x: TypeRef    => Option(getFieldBaseFromString(x.code))
            case x: Identifier => Option(x.name)
            case x: Call if x.name == Operators.fieldAccess =>
              Option(fieldAccessBase(x.asInstanceOf[FieldAccess]))
            case _ => None
          }.iterator
        case x if x.methodFullName == Operators.fieldAccess =>
          fieldAccessBase(x.asInstanceOf[FieldAccess]) :: Nil
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
          .sortBy { case TypeImportInfoWithProvidence(_, isExternal) =>
            isExternal // sorting booleans puts false (internal) first
          }
          .collectFirst {
            // ignore an import to a file that defines the type
            case TypeImportInfoWithProvidence(TypeImportInfo(_, importPath), _) if importPath != currPath => importPath
          }
      }
      .distinct
      .foreach { importPath =>
        val requireCall = createRequireCall(builder, importPath)
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
      NewLiteral().code(s"'$path'").typeFullName(s"$builtinPrefix.String").argumentIndex(1).order(2)
    builder.addEdge(requireCallNode, pathLiteralNode, EdgeTypes.AST)
    builder.addEdge(requireCallNode, pathLiteralNode, EdgeTypes.ARGUMENT)
    requireCallNode
  }

  private def fieldAccessBase(fa: FieldAccess): String = fieldAccessParts(fa).headOption.getOrElse(fa.argument(1).code)

  @tailrec
  private def fieldAccessParts(fa: FieldAccess): Seq[String] = {
    fa.argument(1) match {
      case subFa: Call if subFa.name == Operators.fieldAccess => fieldAccessParts(subFa.asInstanceOf[FieldAccess])
      case self: Identifier if self.name == Self              => fa.fieldIdentifier.map(_.canonicalName).toSeq
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

object ImplicitRequirePass {

  /** Determines if the given type name and its corresponding parent file name allow for the type to be autoloaded by
    * zeitwerk.
    * @return
    *   true if the type is autoloadable from the given filename.
    */
  def isAutoloadable(typeName: String, fileName: String): Boolean = {
    // We use lowercase as something like `openssl` and `OpenSSL` don't give obvious clues where capitalisation occurs
    val strippedFileName  = normalizePath(fileName).toLowerCase
    val lowerCaseTypeName = typeName.toLowerCase
    lowerCaseTypeName == strippedFileName.toLowerCase || lowerCaseTypeName == CaseUtils
      .toCamelCase(strippedFileName, true, '_', '-')
      .toLowerCase
  }

  private def normalizePath(path: String): String = path.replace("\\", "/").stripSuffix(".rb")

}
