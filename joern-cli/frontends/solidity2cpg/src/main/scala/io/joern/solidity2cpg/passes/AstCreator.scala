package io.joern.solidity2cpg.passes
import io.joern.x2cpg.Ast.storeInDiffGraph
import io.joern.solidity2cpg.domain.SuryaObject._
import io.joern.x2cpg.{Ast, AstCreatorBase}
import io.joern.x2cpg.datastructures.Global
import io.joern.x2cpg.utils.NodeBuilders.modifierNode
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, DispatchTypes, EdgeTypes, EvaluationStrategies, ModifierTypes, NodeTypes, Operators, PropertyNames}
import io.shiftleft.codepropertygraph.generated.nodes.{NewAnnotation, NewAnnotationLiteral, NewAnnotationParameter, NewAnnotationParameterAssign, NewArrayInitializer, NewBinding, NewBlock, NewCall, NewClosureBinding, NewControlStructure, NewFieldIdentifier, NewFile, NewIdentifier, NewJumpTarget, NewLiteral, NewLocal, NewMember, NewMethod, NewMethodParameterIn, NewMethodRef, NewMethodReturn, NewModifier, NewNamespaceBlock, NewNode, NewReturn, NewTypeDecl, NewTypeRef, NewUnknown}
import org.slf4j.LoggerFactory
import overflowdb.BatchedUpdate.DiffGraphBuilder

import java.util
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
  private val typeMap = mutable.HashMap.empty[String, String]
  private var membersList : Array[String] = Array()
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
    storeInDiffGraph(astRoot, diffGraph)
    diffGraph
  }

  private def astForCompilationUnit(sourceUnit: SourceUnit): Ast = {
    // TODO: Import directives may help with resolving types but for now let's ignore
    sourceUnit.children.collectFirst { case x: ImportDirective => x }
    // TODO: Pragma directive will be useful to get which version of Solidity we're using but for now we don't have
    //  anywhere to store that information
    sourceUnit.children.collectFirst { case x: PragmaDirective => x }

    val packageDecl = astForPackageDeclaration(sourceUnit)
    val namespaceBlockFullName = {
      packageDecl.root.collect { case x: NewNamespaceBlock => x.fullName }.getOrElse("none")
    }
    val typeDecls: Seq[Ast] = withOrder(sourceUnit.children.collect { case x: ContractDefinition => x }) {
      case (contractDef, order) =>
        astForTypeDecl(contractDef, namespaceBlockFullName, order)
    }
//    println(sourceUnit.lineNumber.get)
//    println(sourceUnit.columnNumber.get)
    packageDecl
      .withChildren(typeDecls)

  }
//TODO: Fix
  private def astForPackageDeclaration(sourceUnit:SourceUnit): Ast = {
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

    Ast(
      NewNamespaceBlock()
        .name(namespaceBlock)
        .fullName(fullName)
        .filename(tmp)
        .lineNumber(sourceUnit.lineNumber.get)
        .columnNumber(sourceUnit.columnNumber.get)
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
      .lineNumber(contractDef.lineNumber.get)
      .columnNumber(contractDef.columnNumber.get)

    val memberAsts = withOrder(contractDef.subNodes.collect {
      case x: StateVariableDeclaration => x
      case x: StructDefinition         => x
    }) {
      case (x: StateVariableDeclaration, order) => astForField(x, order +1)
      case (x: StructDefinition, order)         => astForStruct(x, contractDef.name, order +1)
    }

    val methods = withOrder(contractDef.subNodes.collect { case x: FunctionOrModifierDefinition =>
      x
    }) { (methodOrModifier, order) =>
      astsForMethodOrModifier(methodOrModifier, contractDef.name, order)
    }


    val mAst = Ast(typeDecl)
      .withChildren(methods)
      .withChildren(memberAsts)

//      mAst.nodes.foreach { n =>
//        val code  = n.properties.getOrElse("CODE", null)
//        val order = n.properties.getOrElse("ORDER", null)
//        println((order, n.label(), code))
//      }
    mAst
  }

  private def astsForMethodOrModifier(
    methodOrModifier: FunctionOrModifierDefinition,
    contractName: String,
    methodOrModifierOrder: Int
  ): Ast = {
    var body = Ast()
    val name = if (methodOrModifier.name != null) methodOrModifier.name else "<init>"
    val parameters =
      if (methodOrModifier.isVirtual) Seq(createThisParameterNode(contractName))
      else {
        if (methodOrModifier.parameters != null)
          createThisParameterNode(contractName) +: withOrder(methodOrModifier.parameters.collect { case x: VariableDeclaration => x }) {
          case (x, order) =>
            astForParameter(x, order)
          }
        else {
          Seq(createThisParameterNode(contractName))
        }
      }
    if (methodOrModifier.body != null) {
       body = astForBody(methodOrModifier.body.asInstanceOf[Block], parameters.size)
    }

    val methodNode = NewMethod()
      .name(name)
      .order(methodOrModifierOrder)
      .astParentType(NodeTypes.TYPE_DECL)
      .astParentFullName(contractName)
      .lineNumber(methodOrModifier.lineNumber.get)
      .columnNumber(methodOrModifier.columnNumber.get)

    // Modifier definition properties are a subset of function definitions so if this turns out to be a function
    // definition we simply handle the additional we need to
    methodOrModifier match {
      case x: ModifierDefinition =>
        // TODO: Fill these in, try find out what the method return type would be. If multiple then there exists an "any" type
        val methodReturn = NewMethodReturn()
          .typeFullName("")
          .lineNumber(x.lineNumber.get)
          .columnNumber(x.columnNumber.get)

        Ast(methodNode)
          .withChildren(parameters)
          .withChild(body)
          .withChild(Ast(methodReturn.order(parameters.size + 2)))

      case x: FunctionDefinition =>
        /** passing returnParameters if found
          */
        var modifierMethod = Ast()
        val returnParams = if (x.returnParameters != null) x.returnParameters else List()
        val funcType = if (returnParams.nonEmpty) {
            ":" ++ returnParams
              .collect {
                case x: VariableDeclaration => getTypeName(x.typeName,x.storageLocation)
                case x => getTypeName(x, "")
              }
              .mkString(":")
        } else {
          ":void"
        }
        val modifiers =
          if (x.modifiers.nonEmpty) astForModifiers(x.modifiers)
          else Seq()

        /** getting names of types and variable names TODO: "this" param only goes for static methods but for now it
          * seems most solidity methods are dynamic
          */
        val params = Ast(NewLocal().name("this").code("this").typeFullName(contractName).order(0)) +: parameters
          .flatMap(_.root)
          .map(_.properties(PropertyNames.CODE))
          .mkString(", ")
        var types = ""
        val varAndtypes = new mutable.StringBuilder
        val varNames = parameters.flatMap(_.root).map(_.properties(PropertyNames.NAME))
        val typeNames = parameters.flatMap(_.root).map(_.properties(PropertyNames.TYPE_FULL_NAME))
        if (parameters.flatMap(_.root).map(_.properties(PropertyNames.TYPE_FULL_NAME)).size >1) {

          for (i <- varNames.indices) {
            if (!varNames(i).toString.equals("this")) {
              varAndtypes.append(typeNames(i).toString + " ")
              varAndtypes.append(varNames(i).toString)
              if (i != varNames.length-1) {
                varAndtypes.append(", ")
              }
            }
          }
          types = parameters.flatMap(_.root).map(_.properties(PropertyNames.TYPE_FULL_NAME)).mkString(",").replace(contractName+",","")
        } else {
          types = parameters.flatMap(_.root).map(_.properties(PropertyNames.TYPE_FULL_NAME)).mkString(",").replace(contractName,"")
        }

        val code = if (x.name != null) {
          new mutable.StringBuilder("function ").append(name).append("(")
        } else {
          new mutable.StringBuilder("constructor(")
        }
          code.append(varAndtypes.toString()).append(")")

        /** adding visibility into "code"
          */
        if (x.visibility != null && !x.visibility.equals("default")) {
          code.append(" "+x.visibility)
          modifierMethod = x.visibility match {
            case "public" => Ast(NewModifier().modifierType(ModifierTypes.PUBLIC).code(x.visibility))
            case "private" => Ast(NewModifier().modifierType(ModifierTypes.PRIVATE).code(x.visibility))
            case _ => Ast()
          }
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

        val methodReturn = astForMethodReturn(returnParams, parameters.size + 1)
        val mAst = Ast(
          methodNode
            .fullName(contractName + "." + name + funcType + "(" + types + ")")
            .signature(signature)
            .code(code.toString())
            .filename(filename.substring(0, filename.length - 4) + "sol")
            .lineNumber(x.lineNumber.get)
            .columnNumber(x.columnNumber.get)
        )
          .withChild(modifierMethod)
          .withChildren(parameters)
          .withChild(body)
          .withChildren(modifiers)
          .withChild(methodReturn)

        // TODO: Remove this when done, but gives a good idea of what has ORDER and what doesn't
//        mAst.nodes.foreach { n =>
//          val code  = n.properties.getOrElse("CODE", null)
//          val order = n.properties.getOrElse("ORDER", null)
//          println((order, n.label(), code))
//        }

        mAst
      case x =>
        logger.warn(s"Unhandled statement of tyvar modifierMethod = Ast()pe ${x.getClass}")
        Ast() // etc
    }

  }

  // TODO: I assume the only types coming into parameter are var decls but it's worth making sure in tests
  private def astForParameter(varDecl: VariableDeclaration, order: Int): Ast = {
    val NewMethodParameter = NewMethodParameterIn();
    var typefullName       = ""
    var code               = ""
    var visibility         = ""
    var storage            = ""
    typefullName = getTypeName(varDecl.typeName, varDecl.storageLocation)

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
      .code(typefullName + code + visibility /*+storage */+ " " + varDecl.name)
      .typeFullName(typefullName)
      .order(order)
      .evaluationStrategy(getEvaluationStrategy(varDecl.typeName.getType))
      .lineNumber(varDecl.lineNumber.get)
      .columnNumber(varDecl.columnNumber.get)

    Ast(NewMethodParameter)
  }
//TODO: fix vars
  private def astForBody(body: Block, order: Int): Ast = {
    val blockNode = NewBlock().order(order).argumentIndex(order)
    try  {
      val stmts     = body.statements
    } catch {
        case e: Exception => logger.warn(s"HERE Unhandled statement of type ${body}")
    }
    val stmts     = body.statements


    val vars = withOrder(stmts) {case (x, order) =>
      astForLocalDeclaration(x, order)
    }

    Ast(blockNode)
      .withChildren(vars)
      .withChildren(withOrder(stmts) { case (x, order) =>
        astForStatement(x, order + vars.size-1)
      })
  }
  private def astForLocalDeclaration (statement: BaseASTNode, order: Int): Ast = {
    val locals = statement match {
      case x: VariableDeclarationStatement => {
        withOrder(x.variables.collect { case x: VariableDeclaration => x }) { (x, varOrder) =>
          astForLocal(x, varOrder - 1 + order)
        }
      }
      case _ => null
    }
  if (locals != null) {
    Ast()
      .withChildren(locals)
  } else {
    Ast()
  }


  }

  private def astForStatement(statement: BaseASTNode, order: Int): Ast = {
    // TODO : Finish all of these statements
    statement match {
      case x: ExpressionStatement          => astForExpression(x.expression, order)
      case x: EmitStatement                => Ast()
      case x: ForStatement                 => astForForStatement(x, order)
      case x: IfStatement                  => astForIfStatement(x, order)
      case x: ReturnStatement              => astForReturn(x, order)
      case x: VariableDeclarationStatement => astForVarDeclStmt(x, order)
      case x: InlineAssemblyStatement      => astForInlineAssemblyStatement(x.body, order)
      case x: ThrowStatement               => astForThrow(x, order)
      case x: WhileStatement               => astForWhileStatement(x, order)
      case x: Block                        => astForBody(x, order)
      case x =>
        logger.warn(s"Unhandled statement of type ${x.getClass}")
        Ast() // etc
    }
  }

  private def astForWhileStatement(statement: WhileStatement, order: Int): Ast ={
    val condition = astForExpression(statement.condition, order)
    val body = astForStatement(statement.body, order + 1)
    val code = s"while (${condition.root.map(_.properties(PropertyNames.CODE)).mkString("")})"
    val whileNode = NewControlStructure()
      .controlStructureType(ControlStructureTypes.WHILE)
      .order(order)
      .argumentIndex(order)
      .code(code)
      .lineNumber(statement.lineNumber.get)
      .columnNumber(statement.columnNumber.get)
    Ast(whileNode)
      .withChild(condition)
      .withChild(body)
  }

  private def astForInlineAssemblyStatement(block: BaseASTNode, order: Int): Ast = {
    block match {
      case x: AssemblyBlock => astForAsmBody(x, order)
      case _=>
      {
        println("InlineAssemblyStatement type not found: "+block.getType)
        Ast()}
    }

  }

    def astForThrow(stmt: ThrowStatement, order: Int): Ast = {
      val throwNode = NewCall()
        .name("<operator>.throw")
        .methodFullName("<operator>.throw")
        .code(stmt.toString())
        .order(order)
        .argumentIndex(order)
        .dispatchType(DispatchTypes.STATIC_DISPATCH)
        .lineNumber(stmt.lineNumber.get)
        .columnNumber(stmt.columnNumber.get)

      Ast(throwNode)
    }

  private def astForAsmBody(body: AssemblyBlock, order: Int): Ast = {
    val blockNode = NewBlock()
      .order(order)
      .argumentIndex(order)
      .code("Assembly")
      .lineNumber(body.lineNumber.get)
      .columnNumber(body.columnNumber.get)
    val operations     = body.operations
    Ast(blockNode)
      .withChildren(withOrder(operations) { case (x, order) =>
        astForAsmStatement(x, order)
      })
  }

  private def astForAsmStatement(statement: BaseASTNode, order: Int): Ast = {
    // TODO : Finish all of these statements
    statement match {
      case x: AssemblyAssignment          => astForAsmAssignment(x, order)
      case x: AssemblyCall                => astForAsmCall(x, order)
      case x: DecimalNumber               => astForDecimalNumber(x, order)
      case x =>
        logger.warn(s"Unhandled Assembly statement of type ${x.getClass}")
        Ast() // etc
    }

  }

  private def astForAsmAssignment(assignment: AssemblyAssignment, order: Int): Ast = {
    val names = withOrder(assignment.names) { case(x, order) =>
      x match {
        case y: Identifier => astForIdentifier(y, order)
        case _=> {
          println("type in AsmAssignment: "+x.getType)
          Ast()
        }
      }
    }
    val expr = astForAsmStatement(assignment.expression, names.size)
    val lfteq = names.flatMap(_.root.map(_.properties(PropertyNames.CODE)).mkString(""))
    val rhteq = expr.root.map(_.properties(PropertyNames.CODE)).mkString("")
    val callNode = NewCall()
      .name(Operators.assignment)
      .methodFullName(Operators.assignment)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .code(lfteq.head + " " + ":=" + " " + rhteq)
      .argumentIndex(order)
      .order(order)
      .lineNumber(assignment.lineNumber.get)
      .columnNumber(assignment.columnNumber.get)
    val children = names :+ expr
    Ast(callNode)
      .withChildren(children)
      .withArgEdges(callNode, children.flatMap(_.root))
  }

  private def astForAsmCall(call: AssemblyCall, order: Int): Ast = {
        var name           = ""
        var code           = ""
        var args           = ""
        val expr           = call.functionName match {
          case "shr" => {name = Operators.arithmeticShiftRight
            Ast(NewIdentifier()
              .name(name)
              .code(call.functionName)
              .order(order + 1)
              .argumentIndex(order + 1))}
          case "shl" => {name = Operators.shiftLeft
            Ast(NewIdentifier()
              .name(name)
              .code(call.functionName)
              .order(order + 1)
              .argumentIndex(order + 1))}
          case _=> Ast(NewIdentifier()
            .name(call.functionName)
            .code(call.functionName)
            .order(order + 1)
            .argumentIndex(order + 1))

        }
        val arguments = withOrder(call.arguments) { case (x, order) =>
          astForAsmStatement(x, order)
        }
        if (name.isEmpty) {
          name = expr.root.map(_.properties(PropertyNames.NAME)).mkString("")
        }
        args = arguments.flatMap(_.root).map(_.properties(PropertyNames.CODE)).mkString(", ")
        code = if (args.isEmpty) {
          expr.root.map(_.properties(PropertyNames.CODE)).mkString("")
        } else {
          expr.root.map(_.properties(PropertyNames.CODE)).mkString("") + "(" + args + ")"
        }
        val func = NewCall()
          .name(name)
          .code(code)
          .dispatchType(DispatchTypes.DYNAMIC_DISPATCH)
          .order(order)
          .argumentIndex(order)
          .lineNumber(call.lineNumber.get)
          .columnNumber(call.columnNumber.get)
        Ast(func)
          .withChild(expr)
          .withChildren(arguments)
          .withArgEdges(func, expr.root.toList)
          .withArgEdges(func, arguments.flatMap(_.root))

  }

  private def astForDecimalNumber(number: DecimalNumber, order: Int): Ast = {
    Ast(
      NewLiteral()
        .code(number.value)
        .order(order)
        .argumentIndex(order)
        .lineNumber(number.lineNumber.get)
        .columnNumber(number.columnNumber.get)
    )
  }

  private def astForLocal(varDecl: VariableDeclaration, order: Int): Ast = {
    val fullTypeName = getTypeName(varDecl.typeName,varDecl.storageLocation)
    Ast(
      NewLocal()
        .name(varDecl.name)
        .code(fullTypeName + " " + varDecl.name)
        .typeFullName(fullTypeName)
        .order(order)
        .lineNumber(varDecl.lineNumber.get)
        .columnNumber(varDecl.columnNumber.get)
    )

  }

  private def astForVarDecl(varDecl: VariableDeclaration, order: Int): Ast = {
    val newID        = NewIdentifier()
    var typefullName = ""
    typefullName = getTypeName(varDecl.typeName, varDecl.storageLocation)
    var visibility = "";
    varDecl.visibility match {
      case x: String => visibility = x + " "
      case _         => visibility = ""
    }
    if (varDecl.storageLocation != null) {
      typefullName += " "+varDecl.storageLocation
    }
    newID
      .name(varDecl.name)
      .code(typefullName + visibility + varDecl.name)
      .typeFullName(typefullName)
      .order(order)
      .argumentIndex(order)
      .lineNumber(varDecl.lineNumber.get)
      .columnNumber(varDecl.columnNumber.get)

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
      case x: UserDefinedTypeName => x.namePath
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
          .order(0)
          .evaluationStrategy(getEvaluationStrategy("constructor"))
      )
    } else {
      Ast(
        NewMethodParameterIn()
          .name("this")
          .code("this")
          .order(0)
      )
    }
  }

  private def astForMemberVarDecl(varDecl: VariableDeclaration, order: Int): Ast = {
    val newMember    = NewMember()
    var typefullName = ""
    typefullName = getTypeName(varDecl.typeName, varDecl.storageLocation)
    var visibility = "";

    varDecl.visibility match {
      case x: String => visibility = " " + x
      case _         => visibility = ""
    }

    val modifierMethod = varDecl.visibility match {
      case "public" => Ast(NewModifier().modifierType(ModifierTypes.PUBLIC).code(varDecl.visibility))
      case "private" => Ast(NewModifier().modifierType(ModifierTypes.PRIVATE).code(varDecl.visibility))
      case _ => Ast()
    }

    typeMap.addOne(varDecl.name, typefullName)
    membersList = membersList :+ (varDecl.name)
    newMember
      .name(varDecl.name)
      .code(typefullName + visibility + " " + varDecl.name)
      .typeFullName(typefullName)
      .order(order)
      .columnNumber(varDecl.columnNumber.get)
      .lineNumber(varDecl.columnNumber.get)

    Ast(newMember)
      .withChild(Ast(modifierNode(ModifierTypes.STATIC)))
      .withChild(modifierMethod)
  }

  /** TODO: This needs some refinement, can methods really return more than one base type?
    */
  private def astForMethodReturn(value: List[BaseASTNode], order: Int): Ast = {
    var visibility = ""
    var name       = ""
    value.collect { case x: VariableDeclaration => x }.foreach { x =>
      name = getTypeName(x.typeName, x.storageLocation)
      x.visibility match {
        case x: String => visibility = " " + x
        case _         =>
      }
    }
    if (!name.equals("")) {
      registerType(name)
    }
    val code = if (!(visibility + name ).equals(""))
      visibility + name
    else {
      "void"
    }

    if (code.equals("void")) {
      name = code
    }

    Ast(
      NewMethodReturn()
        .code(code)
        .typeFullName(name)
        .order(order)
//        .columnNumber(value.columnNumber.get)
//        .lineNumber(value.columnNumber.get)
    )

  }

  private def astForReturn(returnStatement: ReturnStatement, order: Int): Ast = {

    val exprAst = astForExpression(returnStatement.expression, order + 1)
    val returnNode = NewReturn()
      .code(s"return ${(exprAst.root).map(_.properties(PropertyNames.CODE)).mkString(" ")};")
      .order(order)
      .argumentIndex(order)
      .columnNumber(returnStatement.columnNumber.get)
      .lineNumber(returnStatement.columnNumber.get)
    Ast(returnNode)
      .withChild(exprAst)
      .withArgEdges(returnNode, exprAst.root.toList)

  }

  private def astForModifiers(modifiers: List[BaseASTNode]): Seq[Ast] = {
    var args = ""
    modifiers
      .collect { case x: ModifierInvocation => x }
      .map { x =>
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
        Ast(
          NewModifier()
            .modifierType(x.name)
            .code(x.name + " (" + args + ")")
        )
      }
  }

  private def astForExpression(expr: BaseASTNode, order: Int): Ast = {
    expr match {
      case x: MemberAccess       => astForMemberAccess(x, order)
      case x: Identifier         => astForIdentifier(x, order)
      case x: FunctionCall       => astForFunctionCall(x, order)
      case x: BinaryOperation    => astForBinaryOperation(x, order)
      case x: UnaryOperation     => astForUnaryOperation(x, order)
      case x: NumberLiteral      => astForNumberLiteral(x, order)
      case x: BooleanLiteral     => astForBooleanLiteral(x, order)
      case x: StringLiteral      => astForStringLiteral(x, order)
      case x: IndexAccess        => astForIndexAccess(x, order)
      case x: TupleExpression    => astForTupleExpression(x, order)
      case x: NewExpression      => astForNewExpression(x, order)
      case x: TypeNameExpression => astForTypeNameExpression(x, order)
      case x: Conditional        => astForConditional(x,order)
      case x: ElementaryTypeName => astForCastExpr(x,order)
      case _ => {
        println("astForExpression not matched type : " + expr.getType)
        Ast()
      }
    }
  }

  def astForCastExpr(expr: ElementaryTypeName, order:Int): Ast = {
  val callBack = NewCall()
    .name(Operators.cast)
    .methodFullName(Operators.cast)
    .code(expr.name)
    .dispatchType(DispatchTypes.STATIC_DISPATCH)
    .order(order)
    .typeFullName(registerType(expr.name))
    .argumentIndex(order)
    val value = Seq(Ast(NewTypeRef()
      .code(expr.name)
      .order(1)
      .argumentIndex(1)
      .typeFullName(expr.name)
      .columnNumber(expr.columnNumber.get)
      .lineNumber(expr.columnNumber.get)
    ))
  Ast(callBack)
    .withChildren(value)
    .withArgEdges(callBack, value.flatMap(_.root))
  }

  private def astForConditional(x:Conditional, order: Int):Ast = {
    astForExpression(x.condition,order)
  }

  private def astForForStatement(statement: ForStatement, order: Int): Ast = {
    val initial = astForStatement(statement.initExpression, 1)
    val conditionExpr = astForExpression(statement.conditionExpression, 2)
    val loopExpr = astForStatement(statement.loopExpression, 3)
    val body = {
      statement.body match {
        case x: Block => astForBody(x, 4)
        case x: BaseASTNode => astForExpression(x, 4)
      }
    }
    val code = "for (" + initial.root.map(_.properties(PropertyNames.CODE)).mkString("") +"; "+ conditionExpr.root.map(_.properties(PropertyNames.CODE)).mkString("") + "; " + loopExpr.root.map(_.properties(PropertyNames.CODE)).mkString("")+")"
    val forNode = NewControlStructure()
      .controlStructureType(ControlStructureTypes.FOR)
      .order(order)
      .argumentIndex(order)
      .code(code)
      .columnNumber(statement.columnNumber.get)
      .lineNumber(statement.columnNumber.get)
 val ast = Ast(forNode)
    .withChild(initial)
    .withChild(conditionExpr)
    .withChild(loopExpr)
    .withChild(body)

    conditionExpr.root match {
      case Some(r) => ast.withConditionEdge(forNode, r)
      case None => ast
    }

  }

  private def astForNumberLiteral(numberLiteral: NumberLiteral, order: Int): Ast = {
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
        .order(order)
        .argumentIndex(order)
        .columnNumber(numberLiteral.columnNumber.get)
        .lineNumber(numberLiteral.columnNumber.get)
    )
  }

  private def astForUnaryOperation(operation: UnaryOperation, order: Int): Ast = {
    val subExpression = astForExpression(operation.subExpression, 1)
    val operatorName = if (operation.isPrefix) operation.operator match {
      case "!"  => Operators.logicalNot
      case "++" => Operators.preIncrement
      case "--" => Operators.preDecrement
      case "delete" => Operators.delete
      case _   => throw new Exception("Unsupported unary prefix operator: " + operation.operator)
    }
    else operation.operator match {
      case "!" => Operators.logicalNot
      case "++" => Operators.postIncrement
      case "--" => Operators.postDecrement
      case _   => throw new Exception("Unsupported unary operator: " + operation.operator)
    }
    val code = if (operation.isPrefix)
      operation.operator + subExpression.root.map(_.properties(PropertyNames.CODE)).mkString("")
    else
      subExpression.root.map(_.properties(PropertyNames.CODE)).mkString("") + operation.operator
    val callNode = NewCall()
      .name(operatorName)
      .methodFullName(operatorName)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .code(code)
      .order(order)
      .argumentIndex(order)
      .columnNumber(operation.columnNumber.get)
      .lineNumber(operation.columnNumber.get)

    Ast(callNode).withChild(subExpression)
  }
  private def astForTypeNameExpression(expression: TypeNameExpression, order : Int): Ast = {
    Ast()
  }

  private def astForBinaryOperation(operation: BinaryOperation, order: Int): Ast = {

    val operatorName =
      operation.operator match {
      case "+"  => Operators.addition
      case "-"  => Operators.subtraction
      case "*"  => Operators.multiplication
      case "/"  => Operators.division
      case "%"  => Operators.modulo
      case ">=" => Operators.greaterEqualsThan
      case ">"  => Operators.greaterThan
      case "<=" => Operators.lessEqualsThan
      case "<"  => Operators.lessThan
      case "+=" => Operators.assignmentPlus
      case "-=" => Operators.assignmentMinus
      case "="  => Operators.assignment
      case "&&" => Operators.logicalAnd
      case "||" => Operators.logicalOr
      case "==" => Operators.equals
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
    val lft = astForExpression(operation.left, 1)
    val rht = astForExpression(operation.right, 2)
    val lfteq = lft.root.map(_.properties(PropertyNames.CODE)).mkString("")
    val rhteq = rht.root.map(_.properties(PropertyNames.CODE)).mkString("")
    val callNode = NewCall()
      .name(operatorName)
      .methodFullName(operatorName)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .code(lfteq + " " + operation.operator + " " + rhteq)
      .argumentIndex(order)
      .order(order)
      .columnNumber(operation.columnNumber.get)
      .lineNumber(operation.columnNumber.get)
    val children = Seq(lft, rht)

    Ast(callNode)
      .withChildren(children)
      .withArgEdges(callNode, children.flatMap(_.root))
  }
  private def astForIfStatement(operation: IfStatement, order: Int): Ast = {
    val opNode = astForExpression(operation.condition, 1)
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
        case x => {
          tb = astForStatement(x, 2)
          foundt = true}
      }
    }
    if (operation.falseBody != null) {
      val elseNode =
        Ast(NewControlStructure()
          .controlStructureType(ControlStructureTypes.ELSE)
          .order(3)
          .argumentIndex(3)
          .code("else")
          .columnNumber(operation.columnNumber.get)
          .lineNumber(operation.columnNumber.get)
        )

      operation.falseBody match {
        case x: Block => {
          fb = elseNode.withChild(astForBody(x, 1))
          foundf = true
        }
        case x =>  {
          fb = astForStatement(x, 3)
          fb = elseNode.withChild(astForStatement(x, 1))
          foundf = true}
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
        .columnNumber(operation.columnNumber.get)
        .lineNumber(operation.columnNumber.get)
    if (foundt && foundf) {
      ast = Ast(ifNode)
        .withChild(opNode)
        .withChild(tb)
        .withChild(fb)

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
        .withChild(opNode)
    }
    val ifAst = opNode.root match {
      case Some(r) =>
        ast.withConditionEdge(ifNode, r)
      case None => ast
    }
    ifAst
  }
  private def astForNewExpression(x: NewExpression , order : Int): Ast = {
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
      if (call.methodFullName.split(":").length >1) {
        sig = call.methodFullName.split(":")(1)
      } else {
        sig = ""
      }


    }
    code = expr.root.map(_.properties(PropertyNames.CODE)).mkString("") + "(" + args + ")"
    val typeFullName = if (methodFullName != null & !methodFullName.equals("")) {
      if (call.methodFullName.split(":").length >1) {
        methodFullName.substring(0, methodFullName.indexOf("."))
      } else {
        methodFullName
      }
    } else {
      ""
    }
    val func = NewCall()
      .name(name)
      .code(code)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .order(order)
      .argumentIndex(order)
      .methodFullName(methodFullName)
      .signature(sig)
      .typeFullName(typeFullName)
      .columnNumber(call.columnNumber.get)
      .lineNumber(call.columnNumber.get)
    Ast(func)
      .withChild(expr)
      .withChildren(arguments)
      .withArgEdges(func, arguments.flatMap(_.root))
  }

  private def astForIdentifier(identifier: Identifier, order: Int): Ast = {
    val typeFullName = {
      if (typeMap.contains(identifier.name))
        typeMap.get(identifier.name).toList(typeMap.get(identifier.name).size - 1)
      else
        ""
    }
    if (membersList.contains(identifier.name)) {
      val fieldAccessBlock = NewCall()
        .name(Operators.fieldAccess)
        .code(identifier.name)
        .methodFullName(Operators.fieldAccess)
        .dispatchType(DispatchTypes.STATIC_DISPATCH)
        .typeFullName(typeFullName)
        .order(order)
        .argumentIndex(order)
        .columnNumber(identifier.columnNumber.get)
        .lineNumber(identifier.columnNumber.get)
      val thisID = Ast(NewIdentifier()
        .name("this")
        .code("this")
        .typeFullName(typeFullName)
        .order(1)
        .argumentIndex(1)
        .columnNumber(identifier.columnNumber.get)
        .lineNumber(identifier.columnNumber.get)
      )
      val fieldID = Ast(NewFieldIdentifier()
        .canonicalName(identifier.name)
        .argumentIndex(2)
        .order(2)
        .code(identifier.name)
        .columnNumber(identifier.columnNumber.get)
        .lineNumber(identifier.columnNumber.get)
      )
      val children = Seq(thisID, fieldID)
      Ast(fieldAccessBlock)
        .withChildren(children)
        .withArgEdges(fieldAccessBlock, children.flatMap(_.root))
    } else {
      Ast(NewIdentifier()
        .name(identifier.name)
        .code(identifier.name)
        .typeFullName(typeFullName)
        .order(order)
        .argumentIndex(order)
        .columnNumber(identifier.columnNumber.get)
        .lineNumber(identifier.columnNumber.get)
      )
    }

  }

  private def astForMemberAccess(memberAccess: MemberAccess, order : Int): Ast = {
    val expr = astForExpression(memberAccess.expression, order)
    val name = if (expr.root.map(_.properties(PropertyNames.NAME)).mkString("").equals(Operators.fieldAccess) ) {
     expr.nodes(2).properties(PropertyNames.CANONICAL_NAME)+""
    } else {
      expr.root.map(_.properties(PropertyNames.NAME)).mkString("")
    }
    val fieldAccess = NewCall()
      .name(memberAccess.memberName)
      .code(name+"."+memberAccess.memberName)
      .argumentIndex(order)
      .order(order)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .columnNumber(memberAccess.columnNumber.get)
      .lineNumber(memberAccess.columnNumber.get)

    Ast(fieldAccess)
      .withChild(expr)
      .withReceiverEdge(fieldAccess, expr.root.get)
      .withArgEdges(fieldAccess, expr.root.toList)
  }

  private def astForBooleanLiteral(literal: BooleanLiteral, order : Int): Ast = {
    var code         = ""
    val typeFullName = registerType("bool")
    code = literal.value+""
    Ast(
      NewLiteral()
        .code(code)
        .typeFullName(typeFullName)
        .order(order)
        .argumentIndex(order)
        .columnNumber(literal.columnNumber.get)
        .lineNumber(literal.columnNumber.get)
    )
  }

  private def astForStringLiteral(x: StringLiteral, order : Int): Ast = {
    Ast(NewLiteral().code("\""+x.value+"\"").order(order))
  }

  private def astForIndexAccess(x: IndexAccess, order : Int): Ast = {
    val name = Operators.indexAccess
    val base = astForExpression(x.base, 1)
    val index = astForExpression(x.index, 2)
    val baseName = base.root.map(_.properties(PropertyNames.CODE)).mkString("")
    val indexName = index.root.map(_.properties(PropertyNames.CODE)).mkString("")
    val code = baseName+"["+indexName+"]"
    Ast(NewCall()
      .name(name)
      .methodFullName(name)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .code(code)
      .order(order)
      .argumentIndex(order)
      .columnNumber(x.columnNumber.get)
      .lineNumber(x.columnNumber.get)
      )
      .withChild(base)
      .withChild(index)
  }

  private def astForTupleExpression(expression: TupleExpression, order : Int): Ast = {
    val components = withOrder(expression.components) {
      case (x, size) => astForExpression(x, size+order-1)
    }
    Ast().withChildren(components)
  }

  private def astsForDefinition(x: BaseASTNode, order: Int): Ast = {
    astForExpression(x,order)
  }

  private def astForVarDeclStmt(statement: VariableDeclarationStatement, order: Int): Ast = {
    // TODO: We need to make sure that we don't duplicate Local nodes. Perhaps using
    val vars = withOrder(statement.variables.collect { case x: VariableDeclaration => x }) { (x, varOrder) =>
      astForVarDecl(x, varOrder)
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
        .columnNumber(statement.columnNumber.get)
        .lineNumber(statement.columnNumber.get)
    } else {
      NewCall().dispatchType(DispatchTypes.STATIC_DISPATCH).columnNumber(statement.columnNumber.get)
        .lineNumber(statement.columnNumber.get)
    }

    if (statement.initialValue != null) {
      Ast(call)
        .withChildren(vars)
        .withChild(initial)
        .withArgEdges(call, (vars :+ initial).flatMap(_.root))

    } else {
      Ast(call)
        .withChildren(vars)
        .withArgEdges(call, vars.flatMap(_.root))
    }

  }

  private def astForStruct(structDefinition: StructDefinition, contractName: String, order: Int): Ast = {
    val typeFullName = registerType(contractName + "." + structDefinition.name)
    val memberNode = NewTypeDecl()
      .name(typeFullName)
      .fullName(typeFullName)
      .order(order)
      .columnNumber(structDefinition.columnNumber.get)
      .lineNumber(structDefinition.columnNumber.get)

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
      params += getTypeName(x.typeName, x.storageLocation)
    }
    (params)
  }

  private def getTypeName(base : BaseASTNode,storage : String): String = {
    var sto = storage
    if (sto != "" && sto != null) {
      sto = " "+storage
    } else {
      sto = ""
    }
    base match {
      case x: ElementaryTypeName  => registerType(x.name)
      case x: ArrayTypeName       => if (x.length != null){
                                        registerType(getTypeName(x.baseTypeName,"") + "[" + x.length+ "]"+sto )
                                      } else {
                                        registerType(getTypeName(x.baseTypeName,"") + "[]"+sto )
                                      }
      case x: Mapping             => registerType("mapping") + getMappingKeyAndValue(x) +sto
      case x: UserDefinedTypeName => registerType(x.namePath) +sto
      case x: FunctionTypeName    => registerType("function("+getParameters(x.parameterTypes)+")" +sto)
    }
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
