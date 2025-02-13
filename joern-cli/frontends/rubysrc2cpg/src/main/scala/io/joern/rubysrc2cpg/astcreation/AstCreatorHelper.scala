package io.joern.rubysrc2cpg.astcreation
import io.joern.rubysrc2cpg.astcreation.RubyIntermediateAst.{
  ClassFieldIdentifier,
  ControlFlowStatement,
  DummyNode,
  ElseClause,
  IfExpression,
  IndexAccess,
  InstanceFieldIdentifier,
  MemberAccess,
  MemberCall,
  RubyExpression,
  RubyFieldIdentifier,
  SelfIdentifier,
  SimpleIdentifier,
  SingleAssignment,
  StatementList,
  StaticLiteral,
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
  protected def prefixAsKernelDefined(x: String): String = Defines.prefixAsKernelDefined(x)
  protected def prefixAsCoreType(x: String): String      = Defines.prefixAsCoreType(x)
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

  /** Lowers the `||=` and `&&=` assignment operators to the respective conditional checks, e.g, `aaa ||= bbb` becomes
    * `aaa = bbb if !aaa` `aaa &&= bbb` becomes aaa = bbb if aaa`
    */
  def lowerAssignmentOperator(lhs: RubyExpression, rhs: RubyExpression, op: String, span: TextSpan): RubyExpression &
    ControlFlowStatement = {
    val condition =
      if op == "||=" then UnaryExpression(op = "!", expression = lhs)(span.spanStart(s"!${lhs.span.text}"))
      else lhs
    val thenClause = StatementList(
      List(SingleAssignment(lhs, "=", rhs)(span.spanStart(s"${lhs.span.text} = ${rhs.span.text}")))
    )(span.spanStart(s"${lhs.span.text} = ${rhs.span.text}"))
    IfExpression(condition = condition, thenClause = thenClause, elsifClauses = List.empty, elseClause = None)(
      span.spanStart(s"${thenClause.span.text} if ${condition.span.text}")
    )
  }

  /** Regex matches implicitly assign values to global variables. Lowering the `=~` operator may look like
    *
    * { tmp = 'hello'.match(/h(el)lo/); if tmp; $~ = tmp; $& = tmp[0]; tmp.begin(0); else $~= nil; $& = nil; nil end; }
    */
  def lowerRegexMatch(target: RubyExpression, regex: RubyExpression, originSpan: TextSpan): RubyExpression = {
    // Create tmpName that takes the regex match result
    val tmpName     = this.tmpGen.fresh
    val tmpGenLocal = NewLocal().name(tmpName).code(tmpName).typeFullName(Defines.Any)
    scope.addToScope(tmpName, tmpGenLocal) match {
      case BlockScope(block) => diffGraph.addEdge(block, tmpGenLocal, EdgeTypes.AST)
      case _                 =>
    }
    def tmp = SimpleIdentifier()(originSpan.spanStart(tmpName))

    val matchCall = {
      val code = s"${regex.text}.match(${target.text})"
      MemberCall(regex, ".", "match", target :: Nil)(originSpan.spanStart(code))
    }
    val tmpAssignment = {
      val code = s"$tmpName = ${matchCall.text}"
      SingleAssignment(tmp, "=", matchCall)(originSpan.spanStart(code))
    }

    def self            = SelfIdentifier()(originSpan.spanStart(Defines.Self))
    def globalTilde     = MemberAccess(self, ".", "$~")(originSpan.spanStart("$~"))
    def globalAmpersand = MemberAccess(self, ".", "$&")(originSpan.spanStart("$&"))

    val ifStmt = IfExpression(
      condition = tmp,
      thenClause = {
        val tildeCode   = s"$$~ = $tmpName"
        val tildeAssign = SingleAssignment(globalTilde, "=", tmp)(originSpan.spanStart(tildeCode))

        def intLiteral(n: Int) = StaticLiteral(Defines.prefixAsCoreType(Defines.Integer))(originSpan.spanStart(s"$n"))
        val tmpIndex0          = IndexAccess(tmp, intLiteral(0) :: Nil)(originSpan.spanStart(s"$tmpName[0]"))

        val ampersandCode   = s"$$& = $tmpName[0]"
        val ampersandAssign = SingleAssignment(globalAmpersand, "=", tmpIndex0)(originSpan.spanStart(ampersandCode))

        // use a simple heuristic to determine the N matched groups
        val matchGroups = (1 to regex.text.count(_ == '(')).map { idx =>
          val matchGroupAsgnCode = s"$$$idx = $tmpName[$idx]"
          val matchGroup         = MemberAccess(self, ".", "$")(originSpan.spanStart("$"))
          val matchGroupIndexN   = IndexAccess(matchGroup, intLiteral(idx) :: Nil)(originSpan.spanStart(s"$$[$idx]"))
          val tmpIndexN          = IndexAccess(tmp, intLiteral(idx) :: Nil)(originSpan.spanStart(s"$tmpName[$idx]"))
          SingleAssignment(matchGroupIndexN, "=", tmpIndexN)(originSpan.spanStart(matchGroupAsgnCode))
        }.toList

        // tmp.begin(0) is the lowered return value of `~=`
        val beginCall = MemberCall(tmp, ".", "begin", intLiteral(0) :: Nil)(originSpan.spanStart(s"$tmpName.begin(0)"))
        StatementList(tildeAssign :: ampersandAssign :: Nil ++ matchGroups :+ beginCall)(
          originSpan.spanStart(s"$tildeCode; $ampersandCode")
        )
      },
      elseClause = Option {
        def nil         = StaticLiteral(Defines.prefixAsCoreType(Defines.NilClass))(originSpan.spanStart("nil"))
        val tildeCode   = s"$$~ = nil"
        val tildeAssign = SingleAssignment(globalTilde, "=", nil)(originSpan.spanStart(tildeCode))

        val ampersandCode   = s"$$& = nil"
        val ampersandAssign = SingleAssignment(globalAmpersand, "=", nil)(originSpan.spanStart(ampersandCode))

        val elseSpan = originSpan.spanStart(s"$tildeCode; $ampersandCode; nil")
        ElseClause(StatementList(tildeAssign :: ampersandAssign :: nil :: Nil)(elseSpan))(elseSpan)
      }
    )(originSpan.spanStart(s"if $tmpName ... else ... end"))

    StatementList(tmpAssignment :: ifStmt :: Nil)(originSpan)
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
