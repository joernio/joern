package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.astcreation.RubyIntermediateAst.{
  AliasStatement,
  AllowedTypeDeclarationChild,
  ArrayLiteral,
  ClassFieldIdentifier,
  DefaultMultipleAssignment,
  IfExpression,
  MemberAccess,
  MethodDeclaration,
  ProcedureDeclaration,
  RubyCall,
  RubyExpression,
  RubyFieldIdentifier,
  SelfIdentifier,
  SimpleCall,
  SimpleIdentifier,
  SingleAssignment,
  SingletonClassDeclaration,
  SingletonMethodDeclaration,
  SplattingRubyNode,
  StatementList,
  StaticLiteral,
  TextSpan,
  TypeDeclBodyCall,
  UnaryExpression
}
import io.joern.rubysrc2cpg.parser.RubyJsonHelpers.nilLiteral
import io.joern.rubysrc2cpg.passes.Defines
import io.joern.rubysrc2cpg.passes.Defines.getBuiltInType
import io.shiftleft.codepropertygraph.generated.nodes.Unknown
import org.slf4j.LoggerFactory
import upickle.core.*
import upickle.default.*

object RubyJsonHelpers {

  private val logger = LoggerFactory.getLogger(getClass)

  implicit class JsonObjHelper(o: ujson.Obj) {

    def toTextSpan: TextSpan = {
      val metaData =
        if (o.obj.contains(ParserKeys.MetaData)) read[MetaData](o(ParserKeys.MetaData))
        else read[MetaData](o)

      val offset = Option(metaData.offsetStart) -> Option(metaData.offsetEnd) match {
        case (Some(start), Some(end)) => Option(start -> end)
        case _                        => None
      }

      TextSpan(
        line = Option(metaData.lineNumber).filterNot(_ == -1),
        column = Option(metaData.columnNumber).filterNot(_ == -1),
        lineEnd = Option(metaData.lineNumberEnd).filterNot(_ == -1),
        columnEnd = Option(metaData.columnNumberEnd).filterNot(_ == -1),
        offset = offset,
        text = metaData.code
      )
    }

    def visitOption(key: String)(implicit visit: ujson.Value => RubyExpression): Option[RubyExpression] =
      if contains(key) then Option(visit(o(key))) else None

    def visitArray(key: String)(implicit visit: ujson.Value => RubyExpression): List[RubyExpression] = {
      o(key).arr.map(visit).toList
    }

    def contains(key: String): Boolean = o.obj.get(key).exists(x => x != null && x != ujson.Null)

  }

  protected def nilLiteral(span: TextSpan): StaticLiteral =
    StaticLiteral(getBuiltInType(Defines.NilClass))(span.spanStart("nil"))

  def createClassBodyAndFields(
    obj: ujson.Obj
  )(implicit visit: ujson.Value => RubyExpression): (StatementList, List[RubyExpression & RubyFieldIdentifier]) = {

    def bodyMethod(fieldStatements: List[RubyExpression]): MethodDeclaration = {
      val body = fieldStatements
        .map {
          case field: SimpleIdentifier =>
            val assignmentSpan = field.span.spanStart(s"${field.span.text} = nil")
            SingleAssignment(ClassFieldIdentifier()(field.span), "=", nilLiteral(field.span))(assignmentSpan)
          case field: RubyFieldIdentifier =>
            val assignmentSpan = field.span.spanStart(s"${field.span.text} = nil")
            SingleAssignment(field, "=", nilLiteral(field.span))(assignmentSpan)
          case assignment @ SingleAssignment(_: RubyFieldIdentifier, _, _) => assignment
          case assignment @ SingleAssignment(lhs: SimpleIdentifier, _, _) =>
            assignment.copy(lhs = ClassFieldIdentifier()(lhs.span))(assignment.span)
          case otherExpr => otherExpr
        }
        .distinctBy {
          case _ @SingleAssignment(lhs: RubyFieldIdentifier, _, _) => lhs.text
          case x                                                   => x
        }

      MethodDeclaration(Defines.TypeDeclBody, Nil, StatementList(body)(obj.toTextSpan.spanStart(s"(...)")))(
        obj.toTextSpan.spanStart(s"def <body>; (...); end")
      )
    }

    /** @param expr
      *   An expression that is a direct child to a class or module.
      * @return
      *   true if the expression constitutes field-related behaviour, false if otherwise.
      */
    def isFieldStmt(expr: RubyExpression): Boolean = {
      expr match {
        case _: SingleAssignment    => true
        case _: SimpleIdentifier    => true
        case _: RubyFieldIdentifier => true
        case _                      => false
      }
    }

    /** Extracts a field from the expression.
      * @param expr
      *   An expression that is a direct child to a class or module.
      */
    def getFields(expr: RubyExpression): List[RubyExpression & RubyFieldIdentifier] = {
      expr match {
        case field: SimpleIdentifier                             => ClassFieldIdentifier()(field.span) :: Nil
        case field: RubyFieldIdentifier                          => field :: Nil
        case _ @SingleAssignment(lhs: RubyFieldIdentifier, _, _) => lhs :: Nil
        case _ @SingleAssignment(lhs: SimpleIdentifier, _, _)    => ClassFieldIdentifier()(lhs.span) :: Nil
        case proc: ProcedureDeclaration                          => getFields(proc.body)
        case _ @StatementList(stmts)                             => stmts.flatMap(getFields).distinctBy(_.text)
        case x: RubyCall                                         => x.arguments.flatMap(getFields).distinctBy(_.text)
        case _                                                   => Nil
      }
    }

    obj.visitOption(ParserKeys.Body).map(lowerAliasStatementsToMethods) match {
      case Some(stmtList @ StatementList(expression :: Nil)) if expression.isInstanceOf[AllowedTypeDeclarationChild] =>
        (StatementList(bodyMethod(Nil) :: expression :: Nil)(stmtList.span), getFields(expression))
      case Some(stmtList @ StatementList(expression :: Nil)) if isFieldStmt(expression) =>
        (StatementList(bodyMethod(expression :: Nil) :: Nil)(stmtList.span), getFields(expression))
      case Some(stmtList: StatementList) =>
        val (fieldStmts, otherStmts)   = stmtList.statements.partition(isFieldStmt)
        val (typeDeclStmts, bodyStmts) = otherStmts.partition(_.isInstanceOf[AllowedTypeDeclarationChild])
        val fields = (fieldStmts.flatMap(getFields) ++ typeDeclStmts.flatMap(getFields)).distinctBy(_.text)
        val body = stmtList.copy(statements =
          bodyMethod(fieldStmts ++ typeDeclStmts.flatMap(getFields) ++ bodyStmts) +: typeDeclStmts
        )(stmtList.span)

        (body, fields)
      case None => (StatementList(bodyMethod(Nil) :: Nil)(obj.toTextSpan.spanStart("<empty>")), Nil)
    }
  }

  def createBodyMemberCall(name: String, textSpan: TextSpan): TypeDeclBodyCall = {
    TypeDeclBodyCall(
      MemberAccess(SelfIdentifier()(textSpan.spanStart(Defines.Self)), "::", name)(
        textSpan.spanStart(s"${Defines.Self}::$name")
      ),
      name
    )(textSpan.spanStart(s"${Defines.Self}::$name::${Defines.TypeDeclBody}"))
  }

  def getParts(memberAccess: MemberAccess): List[String] = {
    memberAccess.target match {
      case targetMemberAccess: MemberAccess => getParts(targetMemberAccess) :+ memberAccess.memberName
      case expr                             => expr.text :: memberAccess.memberName :: Nil
    }
  }

  /** Lowers the `||=` and `&&=` assignment operators to the respective `.nil?` checks
    */
  def lowerAssignmentOperator(lhs: RubyExpression, rhs: RubyExpression, op: String, span: TextSpan): RubyExpression = {
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
  ): RubyExpression = {
    IfExpression(condition = condition, thenClause = thenClause, elsifClauses = List.empty, elseClause = None)(
      span.spanStart(s"if ${condition.span.text} then ${thenClause.span.text} end")
    )
  }

  def lowerMultipleAssignment(
    obj: ujson.Obj,
    lhsNodes: List[RubyExpression],
    rhsNodes: List[RubyExpression],
    defaultResult: () => RubyExpression,
    nilResult: () => RubyExpression
  ): RubyExpression = {

    /** Recursively expand and duplicate splatting nodes so that they line up with what they consume.
      *
      * @param nodes
      *   the splat nodes.
      * @param expandSize
      *   how many more duplicates to create.
      */
    def slurp(nodes: List[RubyExpression], expandSize: Int): List[RubyExpression] = nodes match {
      case (head: SplattingRubyNode) :: tail if expandSize > 0 => head :: slurp(head :: tail, expandSize - 1)
      case head :: tail                                        => head :: slurp(tail, expandSize)
      case Nil                                                 => List.empty
    }
    val op = "="
    lazy val defaultAssignments = lhsNodes
      .zipAll(rhsNodes, defaultResult(), nilResult())
      .map { case (lhs, rhs) => SingleAssignment(lhs, op, rhs)(obj.toTextSpan) }

    val assignments = if ((lhsNodes ++ rhsNodes).exists(_.isInstanceOf[SplattingRubyNode])) {
      rhsNodes.size - lhsNodes.size match {
        // Handle slurping the RHS values
        case x if x > 0 => {
          val slurpedLhs = slurp(lhsNodes, x)

          slurpedLhs
            .zip(rhsNodes)
            .groupBy(_._1)
            .toSeq
            .map { case (lhsNode, xs) => lhsNode -> xs.map(_._2) }
            .sortBy { x => slurpedLhs.indexOf(x._1) } // groupBy produces a map which discards insertion order
            .map {
              case (SplattingRubyNode(lhs), rhss) =>
                SingleAssignment(lhs, op, ArrayLiteral(rhss)(obj.toTextSpan))(obj.toTextSpan)
              case (lhs, rhs :: Nil) => SingleAssignment(lhs, op, rhs)(obj.toTextSpan)
              case (lhs, rhss)       => SingleAssignment(lhs, op, ArrayLiteral(rhss)(obj.toTextSpan))(obj.toTextSpan)
            }
            .toList
        }
        // Handle splitting the RHS values
        case x if x < 0 => {
          val slurpedRhs = slurp(rhsNodes, Math.abs(x))

          lhsNodes
            .zip(slurpedRhs)
            .groupBy(_._2)
            .toSeq
            .map { case (rhsNode, xs) => rhsNode -> xs.map(_._1) }
            .sortBy { x => slurpedRhs.indexOf(x._1) } // groupBy produces a map which discards insertion order
            .flatMap {
              case (SplattingRubyNode(rhs), lhss) =>
                lhss.map(SingleAssignment(_, op, SplattingRubyNode(rhs)(rhs.span))(obj.toTextSpan))
              case (rhs, lhs :: Nil) => Seq(SingleAssignment(lhs, op, rhs)(obj.toTextSpan))
              case (rhs, lhss) => lhss.map(SingleAssignment(_, op, SplattingRubyNode(rhs)(rhs.span))(obj.toTextSpan))
            }
            .toList
        }
        case _ => defaultAssignments
      }
    } else {
      val diff = rhsNodes.size - lhsNodes.size
      if diff < 0 then defaultAssignments.dropRight(Math.abs(diff)) else defaultAssignments
    }
    DefaultMultipleAssignment(assignments)(obj.toTextSpan)
  }

  def infinityUpperBound(obj: ujson.Obj): MemberAccess =
    MemberAccess(
      SimpleIdentifier(Option(getBuiltInType(Defines.Float)))(obj.toTextSpan.spanStart("Float")),
      "::",
      "INFINITY"
    )(obj.toTextSpan.spanStart("Float::INFINITY"))

  def infinityLowerBound(obj: ujson.Obj): UnaryExpression =
    UnaryExpression(
      "-",
      MemberAccess(
        SimpleIdentifier(Option(getBuiltInType(Defines.Float)))(obj.toTextSpan.spanStart("Float")),
        "::",
        "INFINITY"
      )(obj.toTextSpan.spanStart("Float::INFINITY"))
    )(obj.toTextSpan.spanStart("-Float::INFINITY"))

  def lowerAliasStatementsToMethods(classBody: RubyExpression): StatementList = {
    val loweredStmts = classBody match {
      case x: StatementList => lowerSingletonClassDeclarations(x)
      case x                => lowerSingletonClassDeclarations(StatementList(List(x))(x.span))
    }

    val stmts = loweredStmts match {
      case StatementList(stmts) => stmts
      case x                    => List(x)
    }

    val methodParamMap = stmts.collect { case method: ProcedureDeclaration =>
      method.methodName -> method.parameters
    }.toMap

    val transformedStmts = stmts.map {
      case alias: AliasStatement if methodParamMap.contains(alias.oldName) =>
        val aliasingMethodParams = methodParamMap(alias.oldName)
        val argsCode             = aliasingMethodParams.map(_.text).mkString(", ")
        val callCode             = s"${alias.oldName}($argsCode)"

        val forwardingCall = SimpleCall(
          SimpleIdentifier(None)(alias.span.spanStart(alias.oldName)),
          aliasingMethodParams.map { x => SimpleIdentifier(None)(alias.span.spanStart(x.span.text)) }
        )(alias.span.spanStart(callCode))
        val aliasMethodBody = StatementList(forwardingCall :: Nil)(alias.span.spanStart(callCode))
        MethodDeclaration(alias.newName, aliasingMethodParams, aliasMethodBody)(
          alias.span.spanStart(s"def ${alias.newName}($argsCode)")
        )

      case alias: AliasStatement =>
        logger.warn(s"Unable to correctly lower aliased method ${alias.oldName} (aliased method not found)")
        val forwardingCall = SimpleCall(SimpleIdentifier(None)(alias.span.spanStart(alias.oldName)), Nil)(alias.span)
        MethodDeclaration(alias.newName, Nil, StatementList(forwardingCall :: Nil)(alias.span))(alias.span)
      case expr => expr
    }

    StatementList(transformedStmts)(classBody.span)
  }

  private def lowerSingletonClassDeclarations(classBody: RubyExpression): RubyExpression = {
    classBody match {
      case stmtList: StatementList =>
        StatementList(stmtList.statements.flatMap {
          case _ @SingletonClassDeclaration(_, baseClass: Some[RubyExpression], body: StatementList, _) =>
            body.statements.map {
              case method @ MethodDeclaration(methodName, parameters, body) =>
                SingletonMethodDeclaration(baseClass.get, methodName, parameters, body)(method.span)
              case nonMethodStatement => nonMethodStatement
            }
          case nonStmtListBody => nonStmtListBody :: Nil
        })(stmtList.span)
      case nonStmtList => nonStmtList
    }
  }

  private case class MetaData(
    code: String,
    @upickle.implicits.key("start_line") lineNumber: Int,
    @upickle.implicits.key("start_column") columnNumber: Int,
    @upickle.implicits.key("end_line") lineNumberEnd: Int,
    @upickle.implicits.key("end_column") columnNumberEnd: Int,
    @upickle.implicits.key("offset_start") offsetStart: Int,
    @upickle.implicits.key("offset_end") offsetEnd: Int
  ) derives ReadWriter

}
