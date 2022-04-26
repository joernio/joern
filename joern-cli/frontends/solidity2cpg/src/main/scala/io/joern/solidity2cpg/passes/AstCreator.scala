package io.joern.solidity2cpg.passes

import io.joern.solidity2cpg.domain.SuryaObject._
import io.joern.x2cpg.{Ast, AstCreatorBase}
import io.joern.x2cpg.datastructures.Global
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, NodeTypes}
import io.shiftleft.codepropertygraph.generated.nodes.{NewAnnotation, NewAnnotationLiteral, NewAnnotationParameter, NewAnnotationParameterAssign, NewArrayInitializer, NewBinding, NewBlock, NewCall, NewClosureBinding, NewControlStructure, NewFieldIdentifier, NewFile, NewIdentifier, NewJumpTarget, NewLiteral, NewLocal, NewMember, NewMethod, NewMethodParameterIn, NewMethodRef, NewMethodReturn, NewModifier, NewNamespaceBlock, NewNode, NewReturn, NewTypeDecl, NewTypeRef, NewUnknown}
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

    val methods= contractDef.subNodes.map(x => astsForMethod(x, fullName))
    val memberAsts = contractDef.subNodes
      .collect{
        case x: StateVariableDeclaration => x
      }
      .map(astForField)

    Ast(typeDecl)
      .withChildren(methods)
      .withChildren(memberAsts)
  }

  private def astsForMethod(methods: BaseASTNode, contractname: String): Ast = {
    methods match {
      case x: ModifierDefinition => {
        val methodNode = NewMethod()
          .name(x.name)
        val parameters = x.parameters.collect { case x: VariableDeclaration => x }.map(astForParameter)
        // TODO: Fill these in, try find out what the method return type would be. If multiple then there exists an "any" type
        val methodReturn = NewMethodReturn().typeFullName("")
        Ast(methodNode)
          .withChildren(parameters)
          .withChild(astForBody(x.body.asInstanceOf[Block]))
          .withChild(Ast(methodReturn))
      }
      case x: FunctionDefinition => {
        val parameters = x.parameters.collect { case x: VariableDeclaration => x }.map(astForParameter)
        // TODO: Fill these in, try find out what the method return type would be. If multiple then there exists an "any" type
        var methodReturn = Ast()
        var funcType = ""
        if (x.returnParameters != null) {
          methodReturn = astForMethodReturn(x.returnParameters);
          x.returnParameters.collect { case y: VariableDeclaration => {
            y.typeName match {
              case z: ElementaryTypeName => funcType += ":"+z.name
            }
          }
          }
        }
        var types = ""
        var variables = ""
        x.parameters.collect( {
          case x: VariableDeclaration => {
            x.typeName match {
              case x: ElementaryTypeName => {
                types += x.name+ ","
              }
            }
            variables += x.name + ","
          }
        })


        if (types != "") {
          types = types.substring(0, types.length - 1)
        }
        val typeArr = types.split(",")
        val varArr = variables.split(",")
        var code = "function " +  x.name + "("
        for (i <- 0 until  typeArr.length ) {
          if (i == 0) {
            code += typeArr(i)+ " " + varArr(i)
          } else {
            code += ", " + typeArr(i) + " "+ varArr(i)
          }
        }
        code += ")";
        if (x.visibility != null) {
          code += " "+ x.visibility
        }

        if (x.returnParameters != null) {
          if (!funcType.equals("")) {
            code += " returns" + " (" + funcType.substring(1, funcType.length) + ")"
          } else {
            code += " returns" + " (" + funcType + ")"
          }
        }
        var vis = false
//        if (x.visibility == null || x.visibility.equals("private")) {
//          vis = false
//        } else {
//          vis = true
//        }

        var signature = ""
        if (!funcType.equals("")) {
          signature = funcType.substring(1,funcType.length) + "("+types+")"
        } else {
          signature = funcType + "("+types+")"
        }
        val thisNode = createThisParameterNode()
        val methodNode = NewMethod()
          .name(x.name)
          .fullName(contractname+"."+x.name + funcType + "("+types+")")
          .signature(signature)
          .code(code)
//          .isExternal(vis)
          .filename(filename.substring(0,filename.length -4 )+"sol")




        Ast(methodNode)
          .withChild(thisNode)
          .withChildren(parameters)
          .withChild(astForBody(x.body.asInstanceOf[Block]))
          .withChild(methodReturn)
      }
      case x =>
        logger.warn(s"Unhandled statement of type ${x.getClass}")
        Ast() // etc
    }


  }

  // TODO: I assume the only types coming into parameter are var decls but it's worth making sure in tests
  private def astForParameter(varDecl: VariableDeclaration): Ast = {
    val NewMethodParameter = NewMethodParameterIn();
    var typefullName = ""
    var code = ""
    varDecl.typeName match  {
      case x: ElementaryTypeName => typefullName = x.name
      case x: Mapping =>  {
        typefullName = "mapping"
        code = getMappingKeyAndValue(x)}
    }
    var visibility = "";
    varDecl.visibility match {
      case x: String => visibility = " "+x
      case _ => visibility = ""
    }
    NewMethodParameter
      .name(varDecl.name)
      .code(typefullName + code +visibility+ " "+varDecl.name)
      .typeFullName(typefullName)
      .order(1)

    Ast(NewMethodParameter)
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
    val newMember = NewMember();
    var typefullName = ""
    var code = ""
    varDecl.typeName match  {
      case x: ElementaryTypeName => typefullName = x.name
      case x: Mapping =>  {
        typefullName = "mapping"
        code = getMappingKeyAndValue(x)}
    }
    var visibility = "";
    varDecl.visibility match {
      case x: String => visibility = " "+x
      case _ => visibility = ""
    }
        newMember
          .name(varDecl.name)
          .code(typefullName + code +visibility+ " "+varDecl.name)
          .typeFullName(typefullName)
          .order(1)

      Ast(newMember)

    // TODO: VarDecls should be Local nodes in their block and NOT be duplicated

    // TODO: When a variable is referenced, it should always be referenced as an identifier

  }

  private def astForField(stateVariableDeclaration: StateVariableDeclaration):Ast = {
    var counter = 0
    val fieldType = stateVariableDeclaration.variables.collect{case x: VariableDeclaration => x}.map(astForVarDecl);
    Ast().withChildren(fieldType)
  }

  private def getMappingKeyAndValue (mapping: Mapping) : String = {
    var key = mapping.keyType match {
      case x: ElementaryTypeName => x.name
      case x: Mapping => (getMappingKeyAndValue(x))
    }
    var value = mapping.valueType match {
      case x: ElementaryTypeName => x.name
      case x: Mapping => (getMappingKeyAndValue(x))
    }
    (" ("+key +" => " + value+")")
  }

  private def createThisParameterNode(/*method: ThisRef*/): Ast = {
    Ast(
      NewMethodParameterIn()
        .name("this")
        .code("this")
//        .typeFullName(registerType(method.getType.toQuotedString))
//        .dynamicTypeHintFullName(Seq(registerType(method.getType.toQuotedString)))
        .order(0)
    )
  }

  private def astForMethodReturn(value: List[BaseASTNode]): Ast = {
    val returnMethod= NewMethodReturn()
    var typefullName = ""
    var code = ""
      value.collect{
        case x: VariableDeclaration => {

          x.typeName match  {
            case x: ElementaryTypeName => typefullName = x.name
            case x: Mapping =>  {
              typefullName = "mapping"
              code = getMappingKeyAndValue(x)}
          }
          var visibility = "";
          x.visibility match {
            case x: String => visibility = " "+x
            case _ => visibility = ""
          }
          returnMethod
            .code(typefullName + code +visibility+ " "+x.name)
            .typeFullName(x.name)
            .order(1)


        }
      }
    Ast(returnMethod)

  }




}
