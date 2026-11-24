package io.joern.php2cpg.passes

import io.joern.php2cpg.parser.ClassParser
import io.joern.php2cpg.parser.ClassParser.*
import io.joern.php2cpg.parser.Domain.MethodDelimiter
import io.joern.x2cpg.ValidationMode.Disabled
import io.joern.x2cpg.passes.base.MethodStubCreator
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.{NewMethod, NewTypeDecl}
import io.shiftleft.codepropertygraph.generated.{Cpg, DispatchTypes, NodeTypes}
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import org.slf4j.LoggerFactory

import java.nio.file.Path
import scala.util.{Failure, Success}

/** This pass parses the high-level symbols of the dependencies to include their types to the CPG.
  * @param cpg
  *   the code property graph.
  * @param dependencyDir
  *   the directory holding the downloaded dependencies.
  */
class DependencySymbolsPass(cpg: Cpg, dependencyDir: Path) extends ForkJoinParallelCpgPass[ClassParserClass](cpg) {

  private val logger                          = LoggerFactory.getLogger(this.getClass)
  implicit val validationMode: ValidationMode = ValidationMode.Disabled

  override def generateParts(): Array[ClassParserClass] = new ClassParser(dependencyDir).parse() match {
    case Success(classes)   => mergeDuplicates(classes).toArray
    case Failure(exception) =>
      logger.error("Exception encountered while parsing symbols from dependencies", exception)
      Array.empty
  }

  /** Declarations with the same full name become one type. Order of first appearance is kept. The first declaration
    * wins per method name, which is compared case-insensitively like PHP does.
    */
  private def mergeDuplicates(classes: List[ClassParserClass]): List[ClassParserClass] = {
    val groups = classes.groupBy(_.fullName)
    classes.map(_.fullName).distinct.map { fullName =>
      val group     = groups(fullName)
      val seen      = scala.collection.mutable.Set.empty[String]
      val functions = group.flatMap(_.functions).filter(function => seen.add(function.name.toLowerCase))
      group.head.copy(functions = functions)
    }
  }

  override def runOnPart(builder: DiffGraphBuilder, clazz: ClassParserClass): Unit = {
    val className     = clazz.name
    val classFullName = clazz.fullName
    val typeDecl      = NewTypeDecl()
      .name(clazz.name)
      .fullName(classFullName)
      .isExternal(true)
      .astParentType(NodeTypes.NAMESPACE_BLOCK)
      .astParentFullName(NamespaceTraversal.globalNamespaceName)

    val functions = clazz.functions
      .map { case ClassParserFunction(name, modifiers) =>
        MethodStubCreator.createMethodStub(
          name = name,
          fullName = s"$classFullName$MethodDelimiter$name",
          signature = "<empty>",
          dispatchType = DispatchTypes.DYNAMIC_DISPATCH,
          parameterCount = 0,
          astParentType = NodeTypes.TYPE_DECL,
          astParentFullName = classFullName,
          dstGraph = builder
        )
      }
      .map(Ast.apply(_))

    Ast.storeInDiffGraph(Ast(typeDecl).withChildren(functions), builder)
  }

}
