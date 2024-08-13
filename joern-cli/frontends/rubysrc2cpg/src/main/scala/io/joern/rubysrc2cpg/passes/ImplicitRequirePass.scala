package io.joern.rubysrc2cpg.passes

import io.joern.rubysrc2cpg.datastructures.{RubyProgramSummary, RubyType}
import io.joern.rubysrc2cpg.passes.GlobalTypes.{builtinPrefix, kernelPrefix}
import io.joern.x2cpg.utils.NodeBuilders.{newCallNode, newFieldIdentifierNode, newIdentifierNode}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{Cpg, DispatchTypes, EdgeTypes, Operators}
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes
import org.apache.commons.text.CaseUtils

import java.util.regex.Pattern
import scala.collection.mutable

/** In some Ruby frameworks, it is common to have an autoloader library that implicitly loads requirements onto the
  * stack. This pass makes these imports explicit. The most popular one is <a
  * href="https://github.com/fxn/zeitwerk">Zeitwerk</a> which we check in `Gemsfile.lock` to enable this pass.
  */
class ImplicitRequirePass(cpg: Cpg, programSummary: RubyProgramSummary) extends ForkJoinParallelCpgPass[Method](cpg) {

  private val importCallName: String = "require_relative"
  private val typeToPath             = mutable.Map.empty[String, String]

  override def init(): Unit = {
    programSummary.pathToType
      .map { case (path, types) =>
        // zeitwerk will match types that share the name of the path.
        // This match is insensitive to camel case, i.e, foo_bar will match type FooBar.
        val fileName = path.split('/').last
        path -> types.filter { t =>
          val typeName = t.name.split("[.]").last
          typeName == fileName || typeName == CaseUtils.toCamelCase(fileName, true, '_', '-')
        }
      }
      .foreach { case (path, types) =>
        types.foreach { typ => typeToPath.put(typ.name, path) }
      }
  }

  override def generateParts(): Array[Method] =
    cpg.method.isModule.whereNot(_.astChildren.isCall.nameExact(importCallName)).toArray

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

    val typeDecl = cpg.typeDecl.fullName(Pattern.quote(moduleMethod.fullName) + ".*").l
    typeDecl.inheritsFromTypeFullName.foreach(possiblyImportedSymbols.append)

    val methodsOfModule = findMethodsViaAstChildren(moduleMethod).toList
    val callsOfModule   = methodsOfModule.ast.isCall.toList

    val symbolsGatheredFromCalls = callsOfModule.flatMap {
      case x if x.name == Operators.alloc =>
        // TODO Once constructor invocations are lowered correctly, this case is not needed anymore.
        x.argument.isIdentifier.name
      case x if x.methodFullName == Operators.fieldAccess && x.argument(1).code == "self" =>
        x.asInstanceOf[OpNodes.FieldAccess].fieldIdentifier.canonicalName
      case x =>
        Iterator.empty
    }

    possiblyImportedSymbols.appendAll(symbolsGatheredFromCalls)

    possiblyImportedSymbols.distinct
      .foreach { identifierName =>
        val rubyTypes = programSummary.matchingTypes(identifierName)
        val requireCalls = rubyTypes.flatMap { rubyType =>
          typeToPath.get(rubyType.name) match {
            case Some(path)
                if moduleMethod.file.name
                  .map(_.replace("\\", "/"))
                  .headOption
                  .exists(x => rubyType.name.startsWith(x)) =>
              None // do not add an import to a file that defines the type
            case Some(path) =>
              Option(createRequireCall(builder, moduleMethod, path))
            case None =>
              None
          }
        }
        val startIndex = moduleMethod.block.astChildren.size
        requireCalls.zipWithIndex.foreach { case (call, idx) =>
          call.order(startIndex + idx)
          builder.addEdge(moduleMethod.block, call, EdgeTypes.AST)
        }
      }
  }

  private def createRequireCall(builder: DiffGraphBuilder, moduleMethod: Method, path: String): NewCall = {
    val requireCallNode = NewCall()
      .name(importCallName)
      .code(s"$importCallName '$path'")
      .methodFullName(s"$kernelPrefix.$importCallName")
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

}
