package io.joern.rubysrc2cpg.astcreation
import io.joern.rubysrc2cpg.astcreation.RubyIntermediateAst.{
  ClassFieldIdentifier,
  ControlFlowStatement,
  DummyNode,
  IfExpression,
  InstanceFieldIdentifier,
  MemberAccess,
  RubyExpression,
  RubyFieldIdentifier,
  SingleAssignment,
  StatementList,
  TextSpan,
  UnaryExpression
}
import io.joern.rubysrc2cpg.datastructures.{BlockScope, FieldDecl}
import io.joern.rubysrc2cpg.passes.Defines
import io.joern.rubysrc2cpg.passes.GlobalTypes
import io.joern.rubysrc2cpg.passes.GlobalTypes.{kernelFunctions, kernelPrefix}
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, EdgeTypes, Operators}

import scala.collection.mutable

trait AstCreatorHelper(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  private val usedFullNames = mutable.Set.empty[String]

  /** Ensures a unique full name is assigned based on the current scope.
    * @param name
    *   the name of the entity.
    * @param counter
    *   an optional counter, used to create unique instances in the case of redefinitions.
    * @param useSurroundingTypeFullName
    *   flag for whether the fullName is for accessor-like method lowering
    * @return
    *   a unique full name.
    */
  protected def computeFullName(
    name: String,
    counter: Option[Int] = None,
    useSurroundingTypeFullName: Boolean = false
  ): String = {
    val surroundingName =
      if useSurroundingTypeFullName then scope.surroundingTypeFullName.head else scope.surroundingScopeFullName.head
    val candidate = counter match {
      case Some(cnt) => s"$surroundingName.$name$cnt"
      case None      => s"$surroundingName.$name"
    }
    if (usedFullNames.contains(candidate)) {
      computeFullName(name, counter.map(_ + 1).orElse(Option(0)), useSurroundingTypeFullName)
    } else {
      usedFullNames.add(candidate)
      candidate
    }
  }

  override def column(node: RubyExpression): Option[Int]    = node.column
  override def columnEnd(node: RubyExpression): Option[Int] = node.columnEnd
  override def line(node: RubyExpression): Option[Int]      = node.line
  override def lineEnd(node: RubyExpression): Option[Int]   = node.lineEnd

  override def code(node: RubyExpression): String = shortenCode(node.text)

  protected def isBuiltin(x: String): Boolean            = kernelFunctions.contains(x)
  protected def prefixAsKernelDefined(x: String): String = s"$kernelPrefix$pathSep$x"
  protected def prefixAsBundledType(x: String): String   = s"${GlobalTypes.builtinPrefix}.$x"
  protected def isBundledClass(x: String): Boolean       = GlobalTypes.bundledClasses.contains(x)
  protected def pathSep                                  = "."

  private def astForFieldInstance(name: String, node: RubyExpression & RubyFieldIdentifier): Ast = {
    val identName = node match {
      case _: InstanceFieldIdentifier => Defines.Self
      case _: ClassFieldIdentifier    => scope.surroundingTypeFullName.map(_.split("[.]").last).getOrElse(Defines.Any)
    }

    astForFieldAccess(
      MemberAccess(
        DummyNode(identifierNode(node, identName, identName, Defines.Any))(node.span.spanStart(identName)),
        ".",
        name
      )(node.span)
    )
  }

  protected def handleVariableOccurrence(node: RubyExpression): Ast = {
    val name       = code(node)
    val identifier = identifierNode(node, name, name, Defines.Any)
    val typeRef    = scope.tryResolveTypeReference(name)

    node match {
      case fieldVariable: RubyFieldIdentifier =>
        scope.findFieldInScope(name) match {
          case None =>
            scope.pushField(FieldDecl(name, Defines.Any, false, false, fieldVariable))
            astForFieldInstance(name, fieldVariable)
          case Some(field) =>
            astForFieldInstance(name, field.node)
        }
      case _ =>
        scope.lookupVariable(name) match {
          case None if typeRef.isDefined =>
            Ast(identifier.typeFullName(typeRef.get.name))
          case None =>
            val local = localNode(node, name, name, Defines.Any)
            scope.addToScope(name, local) match {
              case BlockScope(block) => diffGraph.addEdge(block, local, EdgeTypes.AST)
              case _                 =>
            }
            Ast(identifier).withRefEdge(identifier, local)
          case Some(local) =>
            local match {
              case x: NewLocal             => identifier.dynamicTypeHintFullName(x.dynamicTypeHintFullName)
              case x: NewMethodParameterIn => identifier.dynamicTypeHintFullName(x.dynamicTypeHintFullName)
            }
            Ast(identifier).withRefEdge(identifier, local)
        }
    }

  }

  protected def astForAssignment(
    lhs: NewNode,
    rhs: NewNode,
    lineNumber: Option[Int],
    columnNumber: Option[Int]
  ): Ast = {
    astForAssignment(Ast(lhs), Ast(rhs), lineNumber, columnNumber)
  }

  protected def astForAssignment(
    lhs: Ast,
    rhs: Ast,
    lineNumber: Option[Int],
    columnNumber: Option[Int],
    code: Option[String] = None
  ): Ast = {
    val _code =
      code.getOrElse(Seq(lhs, rhs).flatMap(_.root).collect { case x: ExpressionNew => x.code }.mkString(" = "))
    val assignment = NewCall()
      .name(Operators.assignment)
      .methodFullName(Operators.assignment)
      .code(_code)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .lineNumber(lineNumber)
      .columnNumber(columnNumber)

    callAst(assignment, Seq(lhs, rhs))
  }

  protected def memberForMethod(
    method: NewMethod,
    astParentType: Option[String] = None,
    astParentFullName: Option[String] = None
  ): NewMember = {
    val member = NewMember().name(method.name).code(method.name).dynamicTypeHintFullName(Seq(method.fullName))
    astParentType.foreach(member.astParentType(_))
    astParentFullName.foreach(member.astParentFullName(_))
    member
  }

  /** Lowers the `||=` and `&&=` assignment operators to the respective `.nil?` checks
    */
  def lowerAssignmentOperator(lhs: RubyExpression, rhs: RubyExpression, op: String, span: TextSpan): RubyExpression &
    ControlFlowStatement = {
    val condition  = nilCheckCondition(lhs, op, "nil?", span)
    val thenClause = nilCheckThenClause(lhs, rhs, span)
    nilCheckIfStatement(condition, thenClause, span)
  }

  /** Generates the required `.nil?` check condition used in the lowering of `||=` and `&&=`
    */
  private def nilCheckCondition(lhs: RubyExpression, op: String, memberName: String, span: TextSpan): RubyExpression = {
    val memberAccess =
      MemberAccess(lhs, op = ".", memberName = "nil?")(span.spanStart(s"${lhs.span.text}.nil?"))
    if op == "||=" then memberAccess
    else UnaryExpression(op = "!", expression = memberAccess)(span.spanStart(s"!${memberAccess.span.text}"))
  }

  /** Generates the assignment and the `thenClause` used in the lowering of `||=` and `&&=`
    */
  private def nilCheckThenClause(lhs: RubyExpression, rhs: RubyExpression, span: TextSpan): RubyExpression = {
    StatementList(List(SingleAssignment(lhs, "=", rhs)(span.spanStart(s"${lhs.span.text} = ${rhs.span.text}"))))(
      span.spanStart(s"${lhs.span.text} = ${rhs.span.text}")
    )
  }

  /** Generates the if statement for the lowering of `||=` and `&&=`
    */
  private def nilCheckIfStatement(
    condition: RubyExpression,
    thenClause: RubyExpression,
    span: TextSpan
  ): RubyExpression & ControlFlowStatement = {
    IfExpression(condition = condition, thenClause = thenClause, elsifClauses = List.empty, elseClause = None)(
      span.spanStart(s"if ${condition.span.text} then ${thenClause.span.text} end")
    )
  }

  protected val UnaryOperatorNames: Map[String, String] = Map(
    "!"   -> Operators.logicalNot,
    "not" -> Operators.logicalNot,
    "~"   -> Operators.not,
    "+"   -> Operators.plus,
    "-"   -> Operators.minus
  )

  protected val BinaryOperatorNames: Map[String, String] =
    Map(
      "+"   -> Operators.addition,
      "-"   -> Operators.subtraction,
      "*"   -> Operators.multiplication,
      "/"   -> Operators.division,
      "%"   -> Operators.modulo,
      "**"  -> Operators.exponentiation,
      "=="  -> Operators.equals,
      "===" -> Operators.equals,
      "!="  -> Operators.notEquals,
      "<"   -> Operators.lessThan,
      "<="  -> Operators.lessEqualsThan,
      ">"   -> Operators.greaterThan,
      ">="  -> Operators.greaterEqualsThan,
      "<=>" -> Operators.compare,
      "&&"  -> Operators.logicalAnd,
      "and" -> Operators.logicalAnd,
      "or"  -> Operators.logicalOr,
      "||"  -> Operators.logicalOr,
      "&"   -> Operators.and,
      "|"   -> Operators.or,
      "^"   -> Operators.xor,
//      "<<"  -> Operators.shiftLeft,  Note: Generally Ruby abstracts this as an append operator based on the LHS
      ">>" -> Operators.arithmeticShiftRight
    )

  protected val AssignmentOperatorNames: Map[String, String] = Map(
    "="   -> Operators.assignment,
    "+="  -> Operators.assignmentPlus,
    "-="  -> Operators.assignmentMinus,
    "*="  -> Operators.assignmentMultiplication,
    "/="  -> Operators.assignmentDivision,
    "%="  -> Operators.assignmentModulo,
    "**=" -> Operators.assignmentExponentiation,
    "|="  -> Operators.assignmentOr,
    "&="  -> Operators.assignmentAnd,
    "<<=" -> Operators.assignmentShiftLeft,
    ">>=" -> Operators.assignmentArithmeticShiftRight
  )
}
