package io.joern.abap2cpg.passes

import io.joern.abap2cpg.parser.AbapIntermediateAst
import io.joern.abap2cpg.parser.AbapIntermediateAst.*
import io.joern.x2cpg.{Ast, AstCreatorBase, ValidationMode}
import io.joern.x2cpg.datastructures.Stack.*
import io.joern.x2cpg.datastructures.VariableScopeManager
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

/** Creates CPG from ABAP intermediate representation using Ast builder pattern
  */
class AstCreator(program: ProgramRoot, filename: String)(implicit withSchemaValidation: ValidationMode)
    extends AstCreatorBase[AbapNode, AstCreator](filename) {

  private val scope = new VariableScopeManager()

  /** Get code string from expression */
  private def codeFromExpr(expr: AbapNode): String = {
    expr match {
      case ident: IdentifierExpr => ident.name
      case lit: LiteralExpr      => lit.value
      case call: CallExpr =>
        call.methodName match {
          case Some(method) =>
            val arrow = if (call.isStatic) "=>" else "->"
            s"${call.targetName}${arrow}${method}()"
          case None => s"${call.targetName}()"
        }
      case fieldAccess: FieldAccessExpr =>
        s"${codeFromExpr(fieldAccess.target)}-${fieldAccess.fieldName}"
      case _ => "?"
    }
  }

  // Required abstract methods from AstCreatorBase
  protected def code(node: AbapNode): String = codeFromExpr(node)

  protected def line(node: AbapNode): Option[Int] = node match {
    case n: MethodDef => n.span.start.map(_.row)
    case n: CallExpr => n.span.start.map(_.row)
    case n: AssignmentStmt => n.span.start.map(_.row)
    case n: OperatorCall => n.span.start.map(_.row)
    case n: DataDeclaration => n.span.start.map(_.row)
    case n: IdentifierExpr => n.span.start.map(_.row)
    case n: LiteralExpr => n.span.start.map(_.row)
    case n: FieldAccessExpr => n.span.start.map(_.row)
    case _ => None
  }

  protected def column(node: AbapNode): Option[Int] = node match {
    case n: MethodDef => n.span.start.map(_.col)
    case n: CallExpr => n.span.start.map(_.col)
    case n: AssignmentStmt => n.span.start.map(_.col)
    case n: OperatorCall => n.span.start.map(_.col)
    case n: DataDeclaration => n.span.start.map(_.col)
    case n: IdentifierExpr => n.span.start.map(_.col)
    case n: LiteralExpr => n.span.start.map(_.col)
    case n: FieldAccessExpr => n.span.start.map(_.col)
    case _ => None
  }

  protected def lineEnd(node: AbapNode): Option[Int] = node match {
    case n: MethodDef => n.span.end.map(_.row)
    case n: CallExpr => n.span.end.map(_.row)
    case n: AssignmentStmt => n.span.end.map(_.row)
    case n: OperatorCall => n.span.end.map(_.row)
    case n: DataDeclaration => n.span.end.map(_.row)
    case n: IdentifierExpr => n.span.end.map(_.row)
    case n: LiteralExpr => n.span.end.map(_.row)
    case n: FieldAccessExpr => n.span.end.map(_.row)
    case _ => None
  }

  protected def columnEnd(node: AbapNode): Option[Int] = node match {
    case n: MethodDef => n.span.end.map(_.col)
    case n: CallExpr => n.span.end.map(_.col)
    case n: AssignmentStmt => n.span.end.map(_.col)
    case n: OperatorCall => n.span.end.map(_.col)
    case n: DataDeclaration => n.span.end.map(_.col)
    case n: IdentifierExpr => n.span.end.map(_.col)
    case n: LiteralExpr => n.span.end.map(_.col)
    case n: FieldAccessExpr => n.span.end.map(_.col)
    case _ => None
  }

  /** Main entry point - creates the full CPG for a program */
  override def createAst(): DiffGraphBuilder = {
    val fileNode = NewFile()
      .name(program.fileName)
      .order(0)

    val namespaceBlock = NewNamespaceBlock()
      .name(NamespaceTraversal.globalNamespaceName)
      .fullName(s"$filename:<global>")
      .filename(filename)
      .order(1)

    // Create methods (either in classes or standalone)
    val methodAsts = if (program.classes.nonEmpty) {
      // Methods inside classes
      program.classes.zipWithIndex.flatMap { case (classDef, classIdx) =>
        val typeDecl = NewTypeDecl()
          .name(classDef.name)
          .fullName(classDef.name)
          .isExternal(false)
          .filename(program.fileName)
          .order(classIdx + 1)

        classDef.span.start.foreach { pos =>
          typeDecl.lineNumber(pos.row).columnNumber(pos.col)
        }

        val methodAsts = classDef.methods.zipWithIndex.map { case (method, methodIdx) =>
          astForMethod(method, Some(classDef.name), methodIdx + 1)
        }

        Ast(typeDecl).withChildren(methodAsts) :: Nil
      }
    } else {
      // Standalone methods
      program.methods.zipWithIndex.map { case (method, idx) =>
        astForMethod(method, None, idx + 1)
      }
    }

    val ast = Ast(fileNode)
      .withChild(Ast(namespaceBlock).withChildren(methodAsts))

    Ast.storeInDiffGraph(ast, diffGraph)
    scope.createVariableReferenceLinks(diffGraph, filename)
    diffGraph
  }

  /** Create a method AST */
  private def astForMethod(method: MethodDef, className: Option[String], order: Int): Ast = {
    val fullName = className match {
      case Some(cls) => s"$cls::${method.name}"
      case None      => method.name
    }

    val signature = createSignature(method.parameters)

    val methodNode = NewMethod()
      .name(method.name)
      .fullName(fullName)
      .signature(signature)
      .filename(program.fileName)
      .isExternal(false)
      .order(order)

    method.span.start.foreach { pos =>
      methodNode.lineNumber(pos.row).columnNumber(pos.col)
    }

    // Start method scope
    // order is NOT set here — setOrderWhereNotSet will assign it after parameters (correct position)
    val blockNode = NewBlock()
      .typeFullName("void")
    scope.pushNewMethodScope(fullName, method.name, blockNode, None)

    // Add parameters
    var paramOrder = 1
    val parameterAsts = scala.collection.mutable.ArrayBuffer[Ast]()

    // IMPORTING parameters
    method.parameters.importing.foreach { param =>
      val paramNode = createParameterIn(param, paramOrder)
      scope.addVariable(param.name, paramNode, param.typeName, VariableScopeManager.ScopeType.MethodScope)
      parameterAsts += Ast(paramNode)
      paramOrder += 1
    }

    // EXPORTING parameters
    method.parameters.exporting.foreach { param =>
      val paramNode = createParameterOut(param, paramOrder)
      parameterAsts += Ast(paramNode)
      paramOrder += 1
    }

    // CHANGING parameters (both IN and OUT)
    method.parameters.changing.foreach { param =>
      val paramInNode = createParameterIn(param, paramOrder)
      scope.addVariable(param.name, paramInNode, param.typeName, VariableScopeManager.ScopeType.MethodScope)
      parameterAsts += Ast(paramInNode)

      val paramOutNode = createParameterOut(param, paramOrder)
      parameterAsts += Ast(paramOutNode)
      paramOrder += 1
    }

    // Create method return — order NOT set here, setOrderWhereNotSet places it after the block
    val methodReturn = method.parameters.returning match {
      case Some(returnParam) => createMethodReturn(returnParam)
      case None =>
        NewMethodReturn()
          .evaluationStrategy(EvaluationStrategies.BY_REFERENCE)
          .typeFullName("void")
          .code("void")
    }

    // Create body block with statements
    val stmtAsts = method.body match {
      case Some(stmtList) => astForStatements(stmtList, className)
      case None           => Seq.empty
    }

    val blockAst = Ast(blockNode).withChildren(stmtAsts)

    scope.popScope()

    methodAst(methodNode, parameterAsts.toSeq, blockAst, methodReturn)
  }

  /** Create statements ASTs */
  private def astForStatements(stmtList: StatementList, className: Option[String]): Seq[Ast] = {
    stmtList.statements.zipWithIndex.flatMap { case (stmt, idx) =>
      val order = idx + 1
      stmt match {
        case callExpr: CallExpr =>
          Some(astForCall(callExpr, order, className))

        case assignStmt: AssignmentStmt =>
          Some(astForAssignment(assignStmt, order, className))

        case opCall: OperatorCall =>
          Some(astForOperatorCall(opCall, order, className))

        case dataDecl: DataDeclaration =>
          val localNode = createLocal(dataDecl, order)
          scope.addVariable(dataDecl.name, localNode, dataDecl.typeName, VariableScopeManager.ScopeType.BlockScope)
          val localAst = Ast(localNode)

          // If there's an initial value, create assignment
          dataDecl.initialValue match {
            case Some(initValue) =>
              val assignment = AssignmentStmt(
                target = IdentifierExpr(dataDecl.name, dataDecl.span),
                value = initValue,
                span = dataDecl.span
              )
              Seq(localAst, astForAssignment(assignment, order + 1, className))
            case None =>
              Seq(localAst)
          }

        case _ => None
      }
    }
  }

  /** Create a local variable declaration */
  private def createLocal(dataDecl: DataDeclaration, order: Int): NewLocal = {
    val codeStr = dataDecl.initialValue match {
      case Some(initVal) =>
        s"DATA: ${dataDecl.name} TYPE ${dataDecl.typeName} VALUE ${codeFromExpr(initVal)}."
      case None =>
        s"DATA: ${dataDecl.name} TYPE ${dataDecl.typeName}."
    }

    val localNode = NewLocal()
      .name(dataDecl.name)
      .code(codeStr)
      .typeFullName(dataDecl.typeName)
      .order(order)

    dataDecl.span.start.foreach { pos =>
      localNode.lineNumber(pos.row).columnNumber(pos.col)
    }

    localNode
  }

  /** Create an assignment call */
  private def astForAssignment(assignStmt: AssignmentStmt, order: Int, className: Option[String]): Ast = {
    val callNode = NewCall()
      .name("<operator>.assignment")
      .code(s"${codeFromExpr(assignStmt.target)} = ${codeFromExpr(assignStmt.value)}")
      .methodFullName("<operator>.assignment")
      .typeFullName("ANY")
      .dispatchType("STATIC_DISPATCH")
      .order(order)

    assignStmt.span.start.foreach { pos =>
      callNode.lineNumber(pos.row).columnNumber(pos.col)
    }

    val leftAst = astForExpression(assignStmt.target, 1, className)
    val rightAst = astForExpression(assignStmt.value, 2, className)

    callAst(callNode, Seq(leftAst, rightAst))
  }

  /** Create a call expression */
  private def astForCall(callExpr: CallExpr, order: Int, className: Option[String]): Ast = {
    val methodFullName = (callExpr.targetName.isEmpty, callExpr.methodName) match {
      case (true, Some(method)) =>
        className match {
          case Some(cls) => s"$cls::$method"
          case None      => method
        }
      case (false, Some(method)) => s"${callExpr.targetName}.${method}"
      case (_, None)             => callExpr.targetName
    }

    val code = (callExpr.targetName.isEmpty, callExpr.methodName) match {
      case (true, Some(method))  => s"${method}()"
      case (false, Some(method)) =>
        val arrow = if (callExpr.isStatic) "=>" else "->"
        s"${callExpr.targetName}${arrow}${method}()"
      case (_, None) => s"${callExpr.targetName}()"
    }

    val callNode = NewCall()
      .name(callExpr.methodName.getOrElse(callExpr.targetName))
      .code(code)
      .methodFullName(methodFullName)
      .typeFullName("ANY")
      .dispatchType(if (callExpr.isStatic) "STATIC_DISPATCH" else "DYNAMIC_DISPATCH")
      .order(order)

    callExpr.span.start.foreach { pos =>
      callNode.lineNumber(pos.row).columnNumber(pos.col)
    }

    val argAsts = callExpr.arguments.zipWithIndex.map { case (arg, idx) =>
      astForExpression(arg.value, idx + 1, className)
    }

    callAst(callNode, argAsts)
  }

  /** Create operator call */
  private def astForOperatorCall(opCall: OperatorCall, order: Int, className: Option[String]): Ast = {
    val callNode = NewCall()
      .name(opCall.operatorName)
      .code(opCall.arguments.map(codeFromExpr).mkString(" "))
      .methodFullName(opCall.operatorName)
      .typeFullName("ANY")
      .dispatchType("STATIC_DISPATCH")
      .order(order)

    opCall.span.start.foreach { pos =>
      callNode.lineNumber(pos.row).columnNumber(pos.col)
    }

    val argAsts = opCall.arguments.zipWithIndex.map { case (arg, idx) =>
      astForExpression(arg, idx + 1, className)
    }

    callAst(callNode, argAsts)
  }

  /** Create an expression AST */
  private def astForExpression(expr: AbapNode, argIndex: Int, className: Option[String] = None): Ast = {
    expr match {
      case ident: IdentifierExpr =>
        val identNode = NewIdentifier()
          .name(ident.name)
          .code(ident.name)
          .typeFullName("ANY")
          .argumentIndex(argIndex)
          .order(argIndex)

        ident.span.start.foreach { pos =>
          identNode.lineNumber(pos.row).columnNumber(pos.col)
        }

        scope.addVariableReference(ident.name, identNode, "ANY", EvaluationStrategies.BY_REFERENCE)
        Ast(identNode)

      case lit: LiteralExpr =>
        val litNode = NewLiteral()
          .code(lit.value)
          .typeFullName(lit.literalType)
          .argumentIndex(argIndex)
          .order(argIndex)

        lit.span.start.foreach { pos =>
          litNode.lineNumber(pos.row).columnNumber(pos.col)
        }

        Ast(litNode)

      case callExpr: CallExpr =>
        astForCall(callExpr, argIndex, className)

      case opCall: OperatorCall =>
        astForOperatorCall(opCall, argIndex, className)

      case fieldAccess: FieldAccessExpr =>
        astForFieldAccess(fieldAccess, argIndex)

      case _ =>
        val unknownNode = NewIdentifier()
          .name("UNKNOWN")
          .code("UNKNOWN")
          .typeFullName("ANY")
          .argumentIndex(argIndex)
          .order(argIndex)
        Ast(unknownNode)
    }
  }

  /** Create field access call */
  private def astForFieldAccess(fieldAccess: FieldAccessExpr, order: Int): Ast = {
    val callNode = NewCall()
      .name("<operator>.fieldAccess")
      .code(s"${codeFromExpr(fieldAccess.target)}-${fieldAccess.fieldName}")
      .methodFullName("<operator>.fieldAccess")
      .typeFullName("ANY")
      .dispatchType("STATIC_DISPATCH")
      .order(order)

    fieldAccess.span.start.foreach { pos =>
      callNode.lineNumber(pos.row).columnNumber(pos.col)
    }

    val targetAst = astForExpression(fieldAccess.target, 1, None)

    val fieldIdent = NewFieldIdentifier()
      .canonicalName(fieldAccess.fieldName)
      .code(fieldAccess.fieldName)
      .argumentIndex(2)
      .order(2)

    callAst(callNode, Seq(targetAst, Ast(fieldIdent)))
  }

  /** Create METHOD_PARAMETER_IN node */
  private def createParameterIn(param: Parameter, index: Int): NewMethodParameterIn = {
    val evaluationStrategy = if (param.isValue) {
      EvaluationStrategies.BY_VALUE
    } else {
      EvaluationStrategies.BY_REFERENCE
    }

    NewMethodParameterIn()
      .name(param.name)
      .code(param.name)
      .typeFullName(param.typeName)
      .evaluationStrategy(evaluationStrategy)
      .index(index)
      .order(index)
  }

  /** Create METHOD_PARAMETER_OUT node */
  private def createParameterOut(param: Parameter, index: Int): NewMethodParameterOut = {
    val evaluationStrategy = if (param.isValue) {
      EvaluationStrategies.BY_VALUE
    } else {
      EvaluationStrategies.BY_REFERENCE
    }

    NewMethodParameterOut()
      .name(param.name)
      .code(param.name)
      .typeFullName(param.typeName)
      .evaluationStrategy(evaluationStrategy)
      .index(index)
      .order(index)
  }

  /** Create METHOD_RETURN node */
  private def createMethodReturn(returnParam: Parameter): NewMethodReturn = {
    val evaluationStrategy = if (returnParam.isValue) {
      EvaluationStrategies.BY_VALUE
    } else {
      EvaluationStrategies.BY_REFERENCE
    }

    NewMethodReturn()
      .evaluationStrategy(evaluationStrategy)
      .typeFullName(returnParam.typeName)
      .code(returnParam.name)
  }

  /** Create method signature string */
  private def createSignature(params: MethodParameters): String = {
    val importingTypes = params.importing.map(_.typeName).mkString(", ")
    val exportingTypes = params.exporting.map(_.typeName).mkString(", ")
    val changingTypes = params.changing.map(_.typeName).mkString(", ")
    val returnType = params.returning.map(_.typeName).getOrElse("void")

    val allParams = Seq(importingTypes, exportingTypes, changingTypes)
      .filter(_.nonEmpty)
      .mkString("; ")

    if (allParams.nonEmpty) {
      s"($allParams) -> $returnType"
    } else {
      s"() -> $returnType"
    }
  }
}
