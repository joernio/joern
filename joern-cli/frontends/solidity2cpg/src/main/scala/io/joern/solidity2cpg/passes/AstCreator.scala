package io.joern.solidity2cpg.passes

import io.joern.x2cpg.Ast.storeInDiffGraph
import io.joern.solidity2cpg.domain.SuryaObject._
import io.joern.x2cpg.{Ast, AstCreatorBase}
import io.joern.x2cpg.datastructures.Global
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
    val name = if (methodOrModifier.name != null) methodOrModifier.name else "<init>"
    val parameters = {
      (if (methodOrModifier.isVirtual) createThisParameterNode(contractName)
       else
        createThisParameterNode(contractName)) +: withOrder(methodOrModifier.parameters.collect { case x: VariableDeclaration => x }) {
        case (x, order) =>
          astForParameter(x, order)
      }
    }
    val body = astForBody(methodOrModifier.body.asInstanceOf[Block], parameters.size)
    val methodNode = NewMethod()
      .name(name)
      .order(methodOrModifierOrder)
      .astParentType(NodeTypes.TYPE_DECL)
      .astParentFullName(contractName)

    // Modifier definition properties are a subset of function definitions so if this turns out to be a function
    // definition we simply handle the additional we need to
    methodOrModifier match {
      case x: ModifierDefinition =>
        // TODO: Fill these in, try find out what the method return type would be. If multiple then there exists an "any" type
        val methodReturn = NewMethodReturn().typeFullName("")

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
            .collect { case y: VariableDeclaration =>
              y.typeName match {
                case z: ElementaryTypeName => z.name
              }
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
        if (varDecl.storageLocation != null) {
          typefullName += " "+varDecl.storageLocation
          registerType(typefullName)
        } else {
          registerType(typefullName)
        }
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
      .code(typefullName + code + visibility /*+ storage*/ + " " + varDecl.name)
      .typeFullName(typefullName)
      .order(order)
      .evaluationStrategy(getEvaluationStrategy(varDecl.typeName.getType))

    Ast(NewMethodParameter)
  }
//TODO: fix vars
  private def astForBody(body: Block, order: Int): Ast = {
    val blockNode = NewBlock().order(order).argumentIndex(order)
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
//      case x: VariableDeclaration          => astForVarDecl(x, order) // TODO: This is not a statement
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
      case x: ElementaryTypeName => registerType(x.name)
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
    val newID        = NewIdentifier()
    var typefullName = ""
    var code         = ""
    varDecl.typeName match {
      case x: ElementaryTypeName => typefullName = registerType(x.name)
      case x: Mapping => {
        typefullName = registerType("mapping")
        code = getMappingKeyAndValue(x) + " "
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
      case x: String => visibility = x + " "
      case _         => visibility = ""
    }
    if (varDecl.storageLocation != null) {
      typefullName += " "+varDecl.storageLocation
    }
    newID
      .name(varDecl.name)
      .code(code + visibility + varDecl.name)
      .typeFullName(typefullName)
      .order(order)
      .argumentIndex(order)

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
    val newMember    = NewMember()
    var typefullName = ""
    var code         = ""
    varDecl.typeName match {
      case x: ElementaryTypeName => typefullName = registerType(x.name)
      case x: Mapping => {
        typefullName = registerType("mapping")
        code = getMappingKeyAndValue(x)
      }
      case x: ArrayTypeName => {
        x.baseTypeName match {
          case x: ElementaryTypeName => typefullName = registerType(x.name)
          case x: ArrayTypeName => x.baseTypeName match {
            case x:  ElementaryTypeName => typefullName = registerType(x.name)
          }
        }
      }
      case x: UserDefinedTypeName => typefullName = registerType(x.namePath)
      case x: FunctionTypeName    => typefullName = registerType("function(" + getParameters(x.parameterTypes) + ")")
    }
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
      .code(typefullName + code + visibility + " " + varDecl.name)
      .typeFullName(typefullName)
      .order(order)

    Ast(newMember)
      .withChild(modifierMethod)

  }

  /** TODO: This needs some refinement, can methods really return more than one base type?
    */
  private def astForMethodReturn(value: List[BaseASTNode], order: Int): Ast = {
    var mapkey     = ""
    var visibility = ""
    var name       = ""
    value.collect { case x: VariableDeclaration => x }.foreach { x =>
      x.typeName match {
        case x: ElementaryTypeName => name = registerType(x.name)
        case x: Mapping => {
          name = registerType("mapping")
          mapkey = getMappingKeyAndValue(x)
        }
      }

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
    )

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
      case _ => {
        println("name: " + expr.getType)
        Ast()
      }
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
    )
  }

  private def astForUnaryOperation(operation: UnaryOperation, order: Int): Ast = {
//    println(operation.subExpression)
    val subExpression = astForExpression(operation.subExpression, 1)
    val operatorName = if (operation.isPrefix) operation.operator match {
      case "!"  => Operators.logicalNot
      case "++" => Operators.preIncrement
      case "--" => Operators.preDecrement
    }
    else operation.operator match {
      case "!" => Operators.logicalNot
      case "++" => Operators.postIncrement
      case "--" => Operators.postDecrement
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
//    println(order)
    val callNode = NewCall()
      .name(operatorName)
      .methodFullName(operatorName)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .code(lfteq + " " + operation.operator + " " + rhteq)
      .argumentIndex(order)
      .order(order)
    val children = Seq(lft, rht)

    Ast(callNode)
      .withChildren(children)
      .withArgEdges(callNode, children.flatMap(_.root))
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
          .code("else"))
      operation.falseBody match {
        case x: Block => {
//          fb = astForBody(x, 3)
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
      sig = call.methodFullName.split(":")(1)
    }
    code = expr.root.map(_.properties(PropertyNames.CODE)).mkString("") + "(" + args + ")"
    val typeFullName = if (methodFullName != null & !methodFullName.equals("")) {
      methodFullName.substring(0, methodFullName.indexOf("."))
    } else {
      ""
    }
    val func = NewCall()
      .name(name)
      .code(code)
      .dispatchType(DispatchTypes.DYNAMIC_DISPATCH)
      .order(order)
      .argumentIndex(order)
      .methodFullName(methodFullName)
      .signature(sig)
      .typeFullName(typeFullName)
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
        .order(order)
      val thisID = Ast(NewIdentifier()
        .name("this")
        .code("this")
        .typeFullName(typeFullName)
        .order(1)
        .argumentIndex(1))
      val fieldID = Ast(NewFieldIdentifier()
        .canonicalName(identifier.name)
        .argumentIndex(2)
        .order(2)
        .code(identifier.name)
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
        .argumentIndex(order))
    }

  }

  private def astForMemberAccess(memberAccess: MemberAccess, order : Int): Ast = {
    val expr = astForExpression(memberAccess.expression, order)
    val name = expr.root.map(_.properties(PropertyNames.NAME)).mkString("")
    val fieldAccess = NewCall()
      .name(memberAccess.memberName)
      .methodFullName(Operators.fieldAccess)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .code(name+"."+memberAccess.memberName)
      .argumentIndex(order)
      .order(order)
    val fieldIdentifierNode = Ast(NewFieldIdentifier()
      .canonicalName(name)
      .argumentIndex(2)
      .order(2)
      .code(name))
    Ast(fieldAccess)
      .withChild(expr)
      .withChild(fieldIdentifierNode)
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
    )
  }

  private def astForStringLiteral(x: StringLiteral, order : Int): Ast = {
    Ast(NewLiteral().code("\""+x.value+"\"").order(order))
  }

  private def astForIndexAccess(x: IndexAccess, order : Int): Ast = {
    Ast()
  }

  private def astForTupleExpression(expression: TupleExpression, order : Int): Ast = {
    val components = withOrder(expression.components) {
      case (x, size) => astForExpression(x, size+order-1)
    }
    Ast().withChildren(components)
  }

  private def astsForDefinition(x: BaseASTNode, order: Int): Ast = {
    x match {
      case x: NumberLiteral   => astForNumberLiteral(x, order)
      case x: BinaryOperation => astForBinaryOperation(x, order)
      case x: FunctionCall    => astForFunctionCall(x, order)
      case x: TupleExpression => astForTupleExpression(x, order)
    }
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
//      val methodName = if (initial.){
//        case Operator.ASSIGN               => Operators.assignment
//        case Operator.PLUS                 => Operators.assignmentPlus
//        case Operator.MINUS                => Operators.assignmentMinus
//        case Operator.MULTIPLY             => Operators.assignmentMultiplication
//        case Operator.DIVIDE               => Operators.assignmentDivision
//        case Operator.BINARY_AND           => Operators.assignmentAnd
//        case Operator.BINARY_OR            => Operators.assignmentOr
//        case Operator.XOR                  => Operators.assignmentXor
//        case Operator.REMAINDER            => Operators.assignmentModulo
//        case Operator.LEFT_SHIFT           => Operators.assignmentShiftLeft
//        case Operator.SIGNED_RIGHT_SHIFT   => Operators.assignmentArithmeticShiftRight
//        case Operator.UNSIGNED_RIGHT_SHIFT => Operators.assignmentLogicalShiftRight
//      }
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

//  def astForThrow(stmt: ThrowStmt, order: Int): Ast = {
//    val throwNode = NewCall()
//      .name("<operator>.throw")
//      .methodFullName("<operator>.throw")
//      .lineNumber(line(stmt))
//      .columnNumber(column(stmt))
//      .code(stmt.toString())
//      .order(order)
//      .argumentIndex(order)
//      .dispatchType(DispatchTypes.STATIC_DISPATCH)
//
//    val args = astsForExpression(stmt.getExpression, order = 1, None)
//
//    callAst(throwNode, args)
//  }
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
