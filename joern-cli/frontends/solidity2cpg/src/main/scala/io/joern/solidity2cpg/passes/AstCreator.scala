package io.joern.solidity2cpg.passes

import io.joern.solidity2cpg.domain.SuryaObject._
import io.joern.x2cpg.{Ast, AstCreatorBase}
import io.joern.x2cpg.datastructures.Global
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, NodeTypes}
import io.shiftleft.codepropertygraph.generated.nodes.{
  NewBlock,
  NewFile,
  NewMethod,
  NewMethodReturn,
  NewNamespaceBlock,
  NewTypeDecl,
  NewAnnotation,
  NewAnnotationLiteral,
  NewAnnotationParameter,
  NewAnnotationParameterAssign,
  NewArrayInitializer,
  NewBinding,
  NewCall,
  NewClosureBinding,
  NewControlStructure,
  NewFieldIdentifier,
  NewIdentifier,
  NewJumpTarget,
  NewLiteral,
  NewLocal,
  NewMember,
  NewMethodParameterIn,
  NewMethodRef,
  NewModifier,
  NewNode,
  NewReturn,
  NewTypeRef,
  NewUnknown
}
import org.slf4j.LoggerFactory
import overflowdb.BatchedUpdate.DiffGraphBuilder

/** Creates an AST using [[createAst]].
  * @param filename
  *   the name of the file this file is generated from. This should correspond to the .sol file without the temporary
  *   directory prefixed.
  * @param sourceUnit
  *   the parsed [[SourceUnit]] representation of a Surya JSON AST.
  * @param global
  *   shared project-wide information.
  */
class AstCreator(filename: String, sourceUnit: SourceUnit, global: Global) extends AstCreatorBase(filename) {

  private val logger = LoggerFactory.getLogger(classOf[AstCreator])

  /** Add `typeName` to a global map and return it. The map is later passed to a pass that creates TYPE nodes for each
    * key in the map.
    */
  private def registerType(typeName: String): String = {
    global.usedTypes.put(typeName, true)
    typeName
  }

  /** Creates the AST for the given Surya [[SourceUnit]].
    * @return
    *   the changes associated to building the AST.
    */
  override def createAst(): DiffGraphBuilder = {
    val astRoot = astForCompilationUnit(sourceUnit)
    storeInDiffGraph(astRoot)
    diffGraph
  }

  /** Copy nodes/edges of given `AST` into the diff graph
    */
  private def storeInDiffGraph(ast: Ast): scala.Unit = {
    ast.nodes.foreach { node =>
      diffGraph.addNode(node)
    }
    ast.edges.foreach { edge =>
      diffGraph.addEdge(edge.src, edge.dst, EdgeTypes.AST)
    }
    ast.conditionEdges.foreach { edge =>
      diffGraph.addEdge(edge.src, edge.dst, EdgeTypes.CONDITION)
    }
    ast.argEdges.foreach { edge =>
      diffGraph.addEdge(edge.src, edge.dst, EdgeTypes.ARGUMENT)
    }
  }

  private def astForCompilationUnit(sourceUnit: SourceUnit): Ast = {
    // TODO: Import directives may help with resolving types but for now let's ignore
    sourceUnit.children.collectFirst { case x: ImportDirective => x }
    // TODO: Pragma directive will be useful to get which version of Solidity we're using but for now we don't have
    //  anywhere to store that information
    sourceUnit.children.collectFirst { case x: PragmaDirective => x }

    val packageDecl = astForPackageDeclaration()
    val namespaceBlockFullName = {
      packageDecl.root.collect { case x: NewNamespaceBlock => x.fullName }.getOrElse("none")
    }
    val typeDecls: Seq[Ast] = sourceUnit.children.flatMap {
      case x: ContractDefinition => Some(astForTypeDecl(x, namespaceBlockFullName))
      case _                     => None
    }
    packageDecl
      .withChildren(typeDecls)

  }

  private def astForPackageDeclaration(): Ast = {
    val fullName = filename.replace(java.io.File.separator, ".")
    val namespaceBlock = fullName
      .split("\\.")
      .lastOption
      .getOrElse("")
      .replace("\\.*.sol", "") // removes the .SolidityFile.sol at the end of the filename

    Ast(
      NewNamespaceBlock()
        .name(namespaceBlock)
        .fullName(fullName)
        .filename(filename)
    )
  }

  private def astForTypeDecl(contractDef: ContractDefinition, astParentFullName: String): Ast = {
    val fullName  = contractDef.name
    val shortName = fullName.split("\\.").lastOption.getOrElse(contractDef).toString
    // TODO: Should look out for inheritance/implemented types I think this is in baseContracts? Make sure
    val superTypes = contractDef.baseContracts.map { contractDef => contractDef.getType }
    val typeDecl = NewTypeDecl()
      .name(shortName)
      .fullName(fullName)
      .astParentType(NodeTypes.NAMESPACE_BLOCK)
      .astParentFullName(astParentFullName)
      .filename(filename)
      .inheritsFromTypeFullName(superTypes)

    val methods = contractDef.subNodes
      .collect { case x: ModifierDefinition =>
        x
      }
      .map(astsForMethod)
    val functions = contractDef.subNodes
      .collect { case x: FunctionDefinition =>
        x
      }
      .map(astsForFunction)

    Ast(typeDecl)
      .withChildren(methods)
  }

  private def astsForMethod(modifierDefinition: ModifierDefinition): Ast = {
    val methodNode = NewMethod()
      .name(modifierDefinition.name)
    val parameters = modifierDefinition.parameters.collect { case x: VariableDeclaration => x }.map(astForParameter)
    // TODO: Fill these in, try find out what the method return type would be. If multiple then there exists an "any" type
    val methodReturn = NewMethodReturn().typeFullName("")
    Ast(methodNode)
      .withChildren(parameters)
      .withChild(astForBody(modifierDefinition.body.asInstanceOf[Block]))
      .withChild(Ast(methodReturn))
  }

  // TODO: I assume the only types coming into parameter are var decls but it's worth making sure in tests
  private def astForParameter(varDecl: VariableDeclaration): Ast = {
    Ast()
  }

  private def astForBody(body: Block): Ast = {
    val blockNode = NewBlock()
    Ast(blockNode)
      .withChildren(body.statements.map(astForStatement))
  }

  private def astForStatement(statement: BaseASTNode): Ast = {
    statement match {
      case x: ExpressionStatement => Ast()
      case x: VariableDeclaration => astForVarDecl(x)
      case x =>
        logger.warn(s"Unhandled statement of type ${x.getClass}")
        Ast() // etc
    }
  }

  private def astForVarDecl(varDecl: VariableDeclaration): Ast = {
    // TODO: VarDecls should be Local nodes in their block and NOT be duplicated

    // TODO: When a variable is referenced, it should always be referenced as an identifier
    Ast()
  }

  private def astsForFunction(function: FunctionDefinition): Ast = {
    val functionNode = NewTypeDecl()
      .name(function.name)
      .fullName(function.name)
      .astParentType(NodeTypes.NAMESPACE_BLOCK)
      .astParentFullName(function.name);
    Ast()
  }

}
