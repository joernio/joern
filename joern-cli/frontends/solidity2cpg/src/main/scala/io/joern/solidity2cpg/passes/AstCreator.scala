package io.joern.solidity2cpg.passes

import io.joern.solidity2cpg.domain.SuryaObject
import io.joern.solidity2cpg.domain.SuryaObject._
import io.joern.x2cpg.{Ast, AstCreatorBase}
import io.joern.x2cpg.datastructures.Global
import io.shiftleft.codepropertygraph.generated.{
  ControlStructureTypes,
  DispatchTypes,
  EdgeTypes,
  EvaluationStrategies,
  NodeTypes,
  Operators,
  PropertyNames
}
import io.shiftleft.codepropertygraph.generated.nodes.{
  NewAnnotation,
  NewAnnotationLiteral,
  NewAnnotationParameter,
  NewAnnotationParameterAssign,
  NewArrayInitializer,
  NewBinding,
  NewBlock,
  NewCall,
  NewClosureBinding,
  NewControlStructure,
  NewFieldIdentifier,
  NewFile,
  NewIdentifier,
  NewJumpTarget,
  NewLiteral,
  NewLocal,
  NewMember,
  NewMethod,
  NewMethodParameterIn,
  NewMethodRef,
  NewMethodReturn,
  NewModifier,
  NewNamespaceBlock,
  NewNode,
  NewReturn,
  NewTypeDecl,
  NewTypeRef,
  NewUnknown
}
import org.slf4j.LoggerFactory
import overflowdb.BatchedUpdate.DiffGraphBuilder

import scala.collection.mutable
import scala.jdk.CollectionConverters._

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

  import AstCreator._

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
    val typeDecls: Seq[Ast] = withOrder(sourceUnit.children.collect { case x: ContractDefinition => x }) {
      case (contractDef, order) =>
        astForTypeDecl(contractDef, namespaceBlockFullName, order)
    }
    packageDecl
      .withChildren(typeDecls)

  }
//TODO: Fix
  private def astForPackageDeclaration(): Ast = {
    val fullName = filename.replace(java.io.File.separator, ".")
    var tmp      = filename
    val namespaceBlock = fullName
      .split("\\.")
      .lastOption
      .getOrElse("")
      .replace("\\.*.sol", "") // removes the .SolidityFile.sol at the end of the filename

    if (tmp.contains(".json")) {
      tmp = filename.replace("json", "sol")
    }
//    println("namespaceBlock: "+namespaceBlock + " : "+ tmp+ " : "+ fullName)

    Ast(
      NewNamespaceBlock()
        .name(namespaceBlock)
        .fullName(fullName)
        .filename(tmp)
    )
  }

  private def astForTypeDecl(contractDef: ContractDefinition, astParentFullName: String, order: Int): Ast = {
    val fullName  = registerType(contractDef.name)
    val shortName = fullName.split("\\.").lastOption.getOrElse(contractDef).toString
    // TODO: Should look out for inheritance/implemented types I think this is in baseContracts? Make sure
    val superTypes = contractDef.baseContracts.map {
      case x: InheritanceSpecifier => {
        x.baseName.namePath
      }
    }
    val typeDecl = NewTypeDecl()
      .name(shortName)
      .fullName(fullName)
      .astParentType(NodeTypes.NAMESPACE_BLOCK)
      .astParentFullName(astParentFullName)
      .filename(filename.substring(0, filename.length - 4) + "sol")
      .inheritsFromTypeFullName(superTypes)
      .code(shortName)
      .isExternal(false)
      .order(order)
//    val typeDecls: Seq[Ast] = withOrder(sourceUnit.children.collect { case x: ContractDefinition => x }) {
//      case (contractDef, order) =>
//        astForTypeDecl(contractDef, namespaceBlockFullName, order)
//    }
    val methods = withOrder(contractDef.subNodes.collect { case x: FunctionOrModifierDefinition =>
      x
    }) { (methodOrModifier, order) =>
      astsForMethod(methodOrModifier, contractDef.name, order)
    }

    val memberAsts = withOrder(contractDef.subNodes.collect {
      case x: StateVariableDeclaration => x
      case x: StructDefinition         => x
    }) {
      case (x: StateVariableDeclaration, order) => astForField(x, order)
      case (x: StructDefinition, order)         => astForStruct(x, contractDef.name, order)
    }
//    val structAsts = withOrder(contractDef.subNodes.collect {
//      case x: StructDefinition => x
//    }) {
//
//    }

//    val memberAsts = contractDef.subNodes
//      .collect { case(x, order)=>
//        case x: StateVariableDeclaration => {
//          case(x, order) => astForField(x, order)
//        }
//        case x: StructDefinition  => {
//          case (x, order) astForStruct(x, contractDef.name, order)
//        }
//      }

    Ast(typeDecl)
      .withChildren(methods)
      .withChildren(memberAsts)
  }

  private def astsForMethod(methodOrModifier: FunctionOrModifierDefinition, contractName: String, order: Int): Ast = {
    methodOrModifier match {
      case x: ModifierDefinition =>
        val methodNode = NewMethod()
          .name(x.name)
        val parameters = withOrder(x.parameters.collect { case x: VariableDeclaration => x }) { case (x, order) =>
          astForParameter(x, order)
        }
        // TODO: Fill these in, try find out what the method return type would be. If multiple then there exists an "any" type
        val methodReturn = NewMethodReturn().typeFullName("")
        Ast(methodNode)
          .withChildren(parameters)
          .withChild(astForBody(x.body.asInstanceOf[Block], order))
          .withChild(Ast(methodReturn))

      case x: FunctionDefinition =>
        val parameters = withOrder(x.parameters.collect { case x: VariableDeclaration => x }) { case (x, order) =>
          astForParameter(x, order)
        }

        /** allowing for constructors or functions
          */
        val name = if (x.name != null) x.name else "<init>"

        /** passing returnParameters if found
          */
        val (methodReturn, funcType) = if (x.returnParameters != null) {
          (
            astForMethodReturn(x.returnParameters, order),
            ":" ++ x.returnParameters
              .collect { case y: VariableDeclaration =>
                y.typeName match {
                  case z: ElementaryTypeName => z.name
                }

              }
              .mkString(":")
          )
        } else {
          (Ast(NewMethodReturn()), ":void")
        }

        val modifiers =
          if (x.modifiers.nonEmpty) astForModifiers(x.modifiers);
          else Ast()

        /** creating ast node "thisNode"
          */

        val thisNode = createThisParameterNode(contractName)

        /** getting names of types and variable names
          */
        val params = parameters.flatMap(_.root).map(_.properties(PropertyNames.CODE)).mkString(", ")
        val types  = parameters.flatMap(_.root).map(_.properties(PropertyNames.TYPE_FULL_NAME)).mkString(",")
        val code = if (x.name != null) {
          new mutable.StringBuilder("function ").append(name).append("(")
        } else {
          new mutable.StringBuilder("constructor(")
        }.append(params).append(");")

        /** adding visibility into "code"
          */
        if (x.visibility != null && !x.visibility.equals("default")) {
          code.append(x.visibility)
        }

        /** adding returns into "code" if given
          */
        if (x.returnParameters != null) {
          if (!funcType.equals("")) {
            code.append(s" returns (${funcType.substring(1, funcType.length)})")
          } else {
            code.append(s" returns ($funcType)")
          }
        }

        /** adding to variable "signature"
          */
        val signature = if (!funcType.equals("")) {
          funcType.substring(1, funcType.length) + "(" + types + ")"
        } else {
          funcType + "(" + types + ")"
        }

        /** creating the new method
          */
        val methodNode = NewMethod()
          .name(name)
          .fullName(contractName + "." + name + funcType + "(" + types + ")")
          .signature(signature)
          .code(code.toString())
          .filename(filename.substring(0, filename.length - 4) + "sol")
          .order(order)

        Ast(methodNode)
          .withChild(thisNode)
          .withChildren(parameters)
          .withChild(astForBody(x.body.asInstanceOf[Block], order))
          .withChild(methodReturn)
          .withChild(modifiers)

      case x =>
        logger.warn(s"Unhandled statement of type ${x.getClass}")
        Ast() // etc
    }

  }

  // TODO: I assume the only types coming into parameter are var decls but it's worth making sure in tests
  private def astForParameter(varDecl: VariableDeclaration, order: Int): Ast = {
    val NewMethodParameter = NewMethodParameterIn();
    var typefullName       = ""
    var code               = ""
    var visibility         = "";
    var storage            = ""
    varDecl.typeName match {
      case x: ElementaryTypeName => typefullName = registerType(x.name)
      case x: Mapping => {
        typefullName = registerType("mapping")
        code = getMappingKeyAndValue(x)
      }
      case x: ArrayTypeName => {
        x.baseTypeName match {
          case x: ElementaryTypeName => typefullName += x.name
        }
        typefullName += "["
        if (x.length != null) {
          typefullName += x.length
        }
        typefullName += "]"
        registerType(typefullName)
      }
//      case x:
    }

    varDecl.visibility match {
      case x: String => visibility = " " + x
      case _         => visibility = ""
    }

    varDecl.storageLocation match {
      case x: String => storage = " " + x
      case _         => storage = ""
    }

    NewMethodParameter
      .name(varDecl.name)
      .code(typefullName + code + visibility + storage + " " + varDecl.name)
      .typeFullName(typefullName)
      .order(order)
      .evaluationStrategy(getEvaluationStrategy(varDecl.typeName.getType))

    Ast(NewMethodParameter)
  }

  private def astForBody(body: Block, order: Int): Ast = {
    val blockNode = NewBlock().order(order).argumentIndex(order)
    val vars = body.statements.collect { case x: VariableDeclaration => x }.toSet
    val stmts = body.statements.toSet -- vars
    Ast(blockNode)
      .withChildren(withOrder(vars) { (v, order) => astForLocal(v, order) })
      .withChildren(withOrder(stmts) { case (x, order) =>
        astForStatement(x, vars.size + order)
      })

  }

  private def astForStatement(statement: BaseASTNode, order: Int): Ast = {
    // TODO : Finish all of these statements
    statement match {
      case x: ExpressionStatement          => astForExpression(x.expression, order)
      case x: VariableDeclaration          => astForVarDecl(x, order) // TODO: This is not a statement
      case x: EmitStatement                => Ast()
      case x: ForStatement                 => Ast()
      case x: IfStatement                  => astForIfStatement(x, order)
      case x: ReturnStatement              => astForReturn(x, order)
      case x: VariableDeclarationStatement => astForVarDeclStmt(x, order)
      case x =>
        logger.warn(s"Unhandled statement of type ${x.getClass}")
        Ast() // etc
    }
  }

  private def astForLocal(varDecl: VariableDeclaration, order: Int): Ast = {
    val fullTypeName = varDecl.typeName match {
      case x: ElementaryTypeName => x.name
      case _                     => ""
    }

    Ast(
      NewLocal()
        .name(varDecl.name)
        .code(fullTypeName + " " + varDecl.name)
        .typeFullName(fullTypeName)
        .order(order)
    )

  }

  private def astForVarDecl(varDecl: VariableDeclaration, order: Int): Ast = {
    val newID        = NewIdentifier();
    var typefullName = ""
    var code         = ""
    varDecl.typeName match {
      case x: ElementaryTypeName => typefullName = registerType(x.name)
      case x: Mapping => {
        typefullName = registerType("mapping")
        code = getMappingKeyAndValue(x)
      }
      case x: ArrayTypeName =>
        x.baseTypeName match {
          case x: ElementaryTypeName => typefullName = registerType(x.name)
        }
      case x: UserDefinedTypeName => typefullName = registerType(x.namePath)
      case x: FunctionTypeName    => typefullName = registerType("function(" + getParameters(x.parameterTypes) + ")")
    }
    var visibility = "";
    varDecl.visibility match {
      case x: String => visibility = " " + x
      case _         => visibility = ""
    }
    newID
      .name(varDecl.name)
      .code(code + visibility + " " + varDecl.name)
      .typeFullName(varDecl.name)
      .order(1)
//    println("vd: "+varDecl.name)

    Ast(newID)

    // TODO: VarDecls should be Local nodes in their block and NOT be duplicated

    // TODO: When a variable is referenced, it should always be referenced as an identifier

  }

  private def astForField(stateVariableDeclaration: StateVariableDeclaration, order: Int): Ast = {
    val fieldType =
      stateVariableDeclaration.variables.collect { case x: VariableDeclaration => astForMemberVarDecl(x, order) }

    Ast().withChildren(fieldType)
  }

  private def getMappingKeyAndValue(mapping: Mapping): String = {
    val key = mapping.keyType match {
      case x: ElementaryTypeName => x.name
      case x: Mapping            => (getMappingKeyAndValue(x))
    }
    val value = mapping.valueType match {
      case x: ElementaryTypeName => x.name
      case x: Mapping            => (getMappingKeyAndValue(x))
    }
    (" (" + key + " => " + value + ")")
  }

  private def createThisParameterNode(str: String): Ast = {
    if (str != null) {
      Ast(
        NewMethodParameterIn()
          .name("this")
          .code("this")
          .typeFullName(str)
          //        .typeFullName(registerType(method.getType.toQuotedString))
          //        .dynamicTypeHintFullName(Seq(registerType(method.getType.toQuotedString)))
          .order(0)
          .evaluationStrategy(getEvaluationStrategy("constructor"))
      )
    } else {
      Ast(
        NewMethodParameterIn()
          .name("this")
          .code("this")
          //        .typeFullName(registerType(method.getType.toQuotedString))
          //        .dynamicTypeHintFullName(Seq(registerType(method.getType.toQuotedString)))
          .order(0)
      )
    }
  }

  private def astForMemberVarDecl(varDecl: VariableDeclaration, order: Int): Ast = {
    val newMember    = NewMember();
    var typefullName = ""
    var code         = ""
    varDecl.typeName match {
      case x: ElementaryTypeName => typefullName = registerType(x.name)
      case x: Mapping => {
        typefullName = registerType("mapping")
        code = getMappingKeyAndValue(x)
      }
      case x: ArrayTypeName =>
        x.baseTypeName match {
          case x: ElementaryTypeName => typefullName = registerType(x.name)
        }
      case x: UserDefinedTypeName => typefullName = registerType(x.namePath)
      case x: FunctionTypeName    => typefullName = registerType("function(" + getParameters(x.parameterTypes) + ")")
    }
    var visibility = "";
//    println(varDecl)
//    println(varDecl.visibility)
    varDecl.visibility match {
      case x: String => visibility = " " + x
      case _         => visibility = ""
    }
    newMember
      .name(varDecl.name)
      .code(typefullName + code + visibility + " " + varDecl.name)
      .typeFullName(typefullName)
      .order(order)

    Ast(newMember)

  }

  private def astForMethodReturn(value: List[BaseASTNode], order: Int): Ast = {
    val returnMethod = NewMethodReturn()
    var code         = ""
    var mapkey       = ""
    var visibility   = ""
    var name         = ""
    var counter      = 0;
    value.collect {
      case x: VariableDeclaration => {

        x.typeName match {
          case x: ElementaryTypeName => name = registerType(x.name)
          case x: Mapping => {
            name = registerType("mapping")
            mapkey = getMappingKeyAndValue(x)
          }
        }

        x.visibility match {
          case x: String => visibility = " " + x
          case _         => visibility = ""
        }
        counter += 2
      }
    }
    code = code + visibility + name
    returnMethod
      .code(code)
      .typeFullName(name)
      .order(order + counter)
    Ast(returnMethod)

  }

  private def astForReturn(returnStatement: ReturnStatement, order: Int): Ast = {

    val exprAst = astForExpression(returnStatement.expression, order + 1)
    val returnNode = NewReturn()
      .code(s"return ${(exprAst.root).map(_.properties(PropertyNames.CODE)).mkString(" ")};")
      .order(order)
      .argumentIndex(order)
    Ast(returnNode)
      .withChild(exprAst)
      .withArgEdges(returnNode, exprAst.root.toList)
  }

  private def astForModifiers(modifiers: List[BaseASTNode]): Ast = {
    val modifierNode = NewModifier()
    var args         = ""
    modifiers.collect {
      case x: ModifierInvocation => {
        if (x.arguments != null) {
          x.arguments.collect {
            case x: Identifier => {
              args += x.name + ","
            }
          }
        }
        if (!args.equals("")) {
          args = args.substring(0, args.length - 1)
        }
        modifierNode
          .modifierType(x.name)
          .code(x.name + " (" + args + ")")
      }

    }
    (Ast(modifierNode))
  }

  private def astForExpression(expr: BaseASTNode, order: Int): Ast = {

    expr match {
      case x: MemberAccess       => astForMemberAccess(x)
      case x: Identifier         => astForIdentifier(x)
      case x: FunctionCall       => astForFunctionCall(x, order)
      case x: BinaryOperation    => astForBinaryOperation(x, order)
      case x: UnaryOperation     => astForUnaryOperation(x)
      case x: NumberLiteral      => astForNumberLiteral(x)
      case x: BooleanLiteral     => astForBooleanLiteral(x)
      case x: StringLiteral      => astForStringLiteral(x)
      case x: IndexAccess        => astForIndexAccess(x)
      case x: TupleExpression    => astForTupleExpression(x)
      case x: NewExpression      => astForNewExpression(x)
      case x: TypeNameExpression => astForTypeNameExpression(x)
      case _ => {
        println("name: " + expr.getType)
        Ast()
      }
    }
  }
  private def astForNumberLiteral(numberLiteral: NumberLiteral): Ast = {
    var code         = ""
    val typeFullName = registerType(numberLiteral.number)
    if (numberLiteral.subdenomination != null) {
      code = numberLiteral.number + numberLiteral.subdenomination
    } else {
      code = numberLiteral.number
    }
    Ast(
      NewLiteral()
        .code(code)
        .typeFullName(typeFullName)
    )
  }

  private def astForUnaryOperation(operation: UnaryOperation): Ast = {
    // TODO : finx all cases with "Ast()"
    Ast()
  }
  private def astForTypeNameExpression(expression: TypeNameExpression): Ast = {
    Ast()
  }

  private def astForBinaryOperation(operation: BinaryOperation, order: Int): Ast = {
    var lft = Ast()
    var rht = Ast()
    val operatorName = operation.operator match {
      case "+"  => Operators.addition
      case "-"  => Operators.subtraction
      case "*"  => Operators.multiplication
      case "/"  => Operators.division
      case "%"  => Operators.modulo
      case ">=" => Operators.greaterEqualsThan
      case ">"  => Operators.greaterThan
      case "<=" => Operators.lessEqualsThan
      case "<"  => Operators.lessThan
//      case _: ShlExpr  => Operators.shiftLeft
//      case _: ShrExpr  => Operators.logicalShiftRight
//      case _: UshrExpr => Operators.arithmeticShiftRight
//      case _: CmpExpr  => Operators.compare
//      case _: CmpgExpr => Operators.compare
//      case _: CmplExpr => Operators.compare
//      case _: AndExpr  => Operators.and
//      case _: OrExpr   => Operators.or
//      case _: XorExpr  => Operators.xor
//      case _: EqExpr   => Operators.equals
      case _ => ""
    }
    lft = astForExpression(operation.left, 1)

    rht = astForExpression(operation.right, 2)

    val lfteq = lft.root.map(_.properties(PropertyNames.CODE)).mkString("")
    val rhteq = rht.root.map(_.properties(PropertyNames.CODE)).mkString("")

    val callNode = NewCall()
      .name(operatorName)
      .methodFullName(operatorName)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .code(lfteq + " " + operation.operator + " " + rhteq)
      .argumentIndex(order)
      .order(order)

    Ast(callNode)
      .withChild(lft)
      .withChild(rht)
  }
  private def astForIfStatement(operation: IfStatement, order: Int): Ast = {
    val opNode = operation.condition match {
      case x: BinaryOperation => astForBinaryOperation(x, 1)
      case x: FunctionCall    => astForFunctionCall(x, 1)
    }

    var tb     = Ast()
    var fb     = Ast()
    var foundt = false
    var foundf = false
    if (operation.trueBody != null) {
      operation.trueBody match {
        case x: Block => {
          tb = astForBody(x, 2)
          foundt = true
        }
      }
    }
    if (operation.falseBody != null) {
      operation.falseBody match {
        case x: Block => {
          fb = astForBody(x, 3)
          foundf = true
        }
      }
    }
    val code = opNode.root.map(_.properties(PropertyNames.CODE)).mkString("")
    var ast  = Ast()
    val ifNode =
      NewControlStructure()
        .controlStructureType(ControlStructureTypes.IF)
        .code("if (" + code + ")")
        .order(order)
        .argumentIndex(order)
    if (foundt && foundf) {
      ast = Ast(ifNode)
        .withChild(opNode)
        .withChild(fb)
        .withChild(tb)

    } else if (foundt && !foundf) {
      ast = Ast(ifNode)
        .withChild(opNode)
        .withChild(tb)
    } else if (!foundt && foundf) {

      ast = Ast(ifNode)
        .withChild(opNode)
        .withChild(fb)

    } else {
      ast = Ast(ifNode)
    }
    val ifAst = opNode.root match {
      case Some(r) =>
        ast.withConditionEdge(ifNode, r)
      case None => ast
    }
    ifAst
  }
  private def astForNewExpression(x: NewExpression): Ast = {
    Ast()
  }

  private def astForFunctionCall(call: FunctionCall, order: Int): Ast = {
    var name           = ""
    var code           = ""
    var args           = ""
    var argsArr        = {}
    var methodFullName = ""
    var sig            = ""
    val expr           = astForExpression(call.expression, order + 1)
    val arguments = withOrder(call.arguments.map(x => x)) { case (x, order) =>
      astForExpression(x, order)
    }
    name = expr.root.map(_.properties(PropertyNames.NAME)).mkString("")
    args = arguments.flatMap(_.root).map(_.properties(PropertyNames.CODE)).mkString(", ")
    argsArr = args.split(",")
    if (
      call.methodFullName != null && !call.methodFullName.contains("null") && !call.methodFullName.contains("undefined")
    ) {
      methodFullName = call.methodFullName
      sig = call.methodFullName.split(":")(1)
    }
//    methodFullNameText = expr.root.map(_.properties(PropertyNames.TYPE_FULL_NAME)).mkString("")
//    sig = expr.root.map(_.properties(PropertyNames.SIGNATURE)).mkString("")
    code = name + "(" + args + ")"
    val func = NewCall()
      .name(name)
      .code(name)
      .methodFullName(methodFullName)
      .signature(sig)
//      .code()
    Ast(func)
      .withChild(expr)
      .withChildren(arguments)
  }

  private def astForIdentifier(identifier: Identifier): Ast = {
    val typeFullName = registerType(identifier.name)
    Ast(
      NewIdentifier()
        .name(identifier.name)
        .code(identifier.name)
        .typeFullName(typeFullName)
    )
  }

  private def astForMemberAccess(memberAccess: MemberAccess): Ast = {
    Ast()
  }

  private def astForBooleanLiteral(literal: BooleanLiteral): Ast = {
    Ast()
  }

  private def astForStringLiteral(x: StringLiteral): Ast = {
    Ast()
  }

  private def astForIndexAccess(x: IndexAccess): Ast = {
    Ast()
  }

  private def astForTupleExpression(expression: TupleExpression): Ast = {
    Ast()
  }

  private def astsForDefinition(x: BaseASTNode, order: Int): Ast = {
    x match {
      case x: NumberLiteral   => astForNumberLiteral(x)
      case x: BinaryOperation => astForBinaryOperation(x, order)
      case x: FunctionCall    => astForFunctionCall(x, order)
      case x: TupleExpression => astForTupleExpression(x)
    }
  }

  private def astForVarDeclStmt(statement: VariableDeclarationStatement, order: Int): Ast = {
    // TODO: We need to make sure that we don't duplicate Local nodes. Perhaps using
    val vars = withOrder(statement.variables.collect { case x: VariableDeclaration => x }) { (x, varOrder) =>
      astForVarDecl(x, order + varOrder)
    }

    val initial =
      if (statement.initialValue != null) astsForDefinition(statement.initialValue, statement.variables.size + 1)
      else Ast()
    val call = if (statement.initialValue != null) {
      val lhsCode = vars.flatMap(_.root).flatMap(_.properties.get(PropertyNames.CODE)).mkString
      val rhsCode = initial.root.flatMap(_.properties.get(PropertyNames.CODE)).mkString
      NewCall()
        .name(Operators.assignment)
        .methodFullName(Operators.assignment)
        .code(s"$lhsCode = $rhsCode")
        .dispatchType(DispatchTypes.STATIC_DISPATCH)
        .typeFullName(registerType(vars.flatMap(_.root).flatMap(_.properties.get(PropertyNames.NAME)).mkString))
        .order(vars.size + order)
        .argumentIndex(vars.size + order)
    } else {
      NewCall()
    }

    if (statement.initialValue != null) {
      Ast(call)
        .withChildren(vars)
        .withChild(initial)
    } else {
      Ast()
        .withChildren(vars)
    }

  }

  private def astForStruct(structDefinition: StructDefinition, contractName: String, order: Int): Ast = {
    val typeFullName = registerType(contractName + "." + structDefinition.name)
    val memberNode = NewTypeDecl()
      .name(typeFullName)
      .fullName(typeFullName)

    val members = structDefinition.members.collect { case x: VariableDeclaration =>
      astForVarDecl(x, order)
    }
    Ast(memberNode).withChildren(members)
  }

  private def getEvaluationStrategy(typ: String): String =
    typ match {
      case x: String => {
        if (x.equals("ElementaryTypeName")) {
          EvaluationStrategies.BY_VALUE
        } else if (x.equals("UserDefinedTypeName")) {
          EvaluationStrategies.BY_VALUE
        } else if (x.equals("ArrayTypeName")) {
          EvaluationStrategies.BY_REFERENCE
        } else if (x.equals("Mapping")) {
          EvaluationStrategies.BY_REFERENCE
        } else {
          EvaluationStrategies.BY_SHARING
        }
      }
      case _ => EvaluationStrategies.BY_SHARING
    }

  private def getParameters(paramlist: List[BaseASTNode]): String = {
    var params = ""
    paramlist.collect { case x: VariableDeclaration =>
      x.typeName match {
        case x: ElementaryTypeName => params += registerType(x.name)
        case x: Mapping => {
          params += registerType("mapping")
        }
        case x: ArrayTypeName =>
          x.baseTypeName match {
            case x: ElementaryTypeName => params += registerType(x.name)
          }
        case x: UserDefinedTypeName => params += registerType(x.namePath)
        case x: FunctionTypeName    => params += registerType("function(" + getParameters(x.parameterTypes) + ")")

      }
    }
    (params)
  }
  object AstCreator {

    def withOrder[T <: Any, X](nodeList: java.util.List[T])(f: (T, Int) => X): Seq[X] = {
      nodeList.asScala.zipWithIndex.map { case (x, i) =>
        f(x, i + 1)
      }.toSeq
    }

    def withOrder[T <: Any, X](nodeList: Iterable[T])(f: (T, Int) => X): Seq[X] = {
      nodeList.zipWithIndex.map { case (x, i) =>
        f(x, i + 1)
      }.toSeq
    }
  }

}
