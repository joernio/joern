package io.joern.rubysrc2cpg.passes

import io.joern.rubysrc2cpg.datastructures.{RubyProgramSummary, RubyType}
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

  private val importCallName: String = "require"
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

  override def runOnPart(builder: DiffGraphBuilder, part: Method): Unit = {
    val identifiersToMatch = mutable.ArrayBuffer.empty[String]

    val typeDecl = cpg.typeDecl.fullName(Pattern.quote(part.fullName) + ".*").l
    typeDecl.inheritsFromTypeFullName.foreach(identifiersToMatch.append)

    val methods = findMethodsViaAstChildren(part).toList
    val calls   = methods.ast.isCall.toList
    val identifiers = calls.flatMap {
      case x if x.name == Operators.alloc =>
        // TODO Once constructor invocations are lowered correctly, this case is not needed anymore.
        x.argument.isIdentifier.name
      case x if x.methodFullName == Operators.fieldAccess && x.argument(1).code == "self" =>
        x.asInstanceOf[OpNodes.FieldAccess].fieldIdentifier.canonicalName
      case x =>
        Iterator.empty
    }

    identifiers.foreach(identifiersToMatch.append)

    identifiers.distinct
      .foreach { identifierName =>
        val rubyTypes = programSummary.matchingTypes(identifierName)
        val requireCalls = rubyTypes.flatMap { rubyType =>
          typeToPath.get(rubyType.name) match {
            case Some(path)
                if part.file.name
                  .map(_.replace("\\", "/"))
                  .headOption
                  .exists(x => rubyType.name.startsWith(x)) =>
              None // do not add an import to a file that defines the type
            case Some(path) =>
              Option(createRequireCall(builder, rubyType, path))
            case None =>
              None
          }
        }
        val startIndex = part.block.astChildren.size
        requireCalls.zipWithIndex.foreach { case (call, idx) =>
          call.order(startIndex + idx)
          builder.addEdge(part.block, call, EdgeTypes.AST)
        }
      }
  }

  private def createRequireCall(builder: DiffGraphBuilder, rubyType: RubyType, path: String): NewCall = {
    val requireCallNode = NewCall()
      .name(importCallName)
      .code(s"$importCallName '$path'")
      .methodFullName(s"__builtin.$importCallName")
      .dispatchType(DispatchTypes.DYNAMIC_DISPATCH)
      .typeFullName(Defines.Any)
    val receiverIdentifier =
      NewIdentifier().name(importCallName).code(importCallName).typeFullName(Defines.Any).argumentIndex(0).order(1)
    val pathLiteralNode = NewLiteral().code(s"'$path'").typeFullName("__builtin.String").argumentIndex(1).order(2)
    builder.addNode(requireCallNode)
    builder.addEdge(requireCallNode, receiverIdentifier, EdgeTypes.AST)
    builder.addEdge(requireCallNode, receiverIdentifier, EdgeTypes.ARGUMENT)
    builder.addEdge(requireCallNode, receiverIdentifier, EdgeTypes.RECEIVER)
    builder.addEdge(requireCallNode, pathLiteralNode, EdgeTypes.AST)
    builder.addEdge(requireCallNode, pathLiteralNode, EdgeTypes.ARGUMENT)
    requireCallNode
  }

}
