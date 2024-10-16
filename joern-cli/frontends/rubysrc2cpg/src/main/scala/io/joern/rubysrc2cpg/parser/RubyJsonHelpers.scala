package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.astcreation.RubyIntermediateAst.{
  ClassFieldIdentifier,
  MemberAccess,
  MethodDeclaration,
  RubyExpression,
  RubyFieldIdentifier,
  SelfIdentifier,
  SimpleIdentifier,
  SingleAssignment,
  StatementList,
  StaticLiteral,
  TextSpan,
  TypeDeclBodyCall
}
import io.joern.rubysrc2cpg.passes.Defines
import io.joern.rubysrc2cpg.passes.Defines.getBuiltInType
import upickle.core.*
import upickle.default.*

object RubyJsonHelpers {

  implicit class JsonObjHelper(o: ujson.Obj) {

    def toTextSpan: TextSpan = {
      val metaData =
        if (o.obj.contains(ParserKeys.MetaData)) read[MetaData](o(ParserKeys.MetaData))
        else read[MetaData](o)

      TextSpan(
        line = Option(metaData.lineNumber).filterNot(_ == -1),
        column = Option(metaData.columnNumber).filterNot(_ == -1),
        lineEnd = Option(metaData.lineNumberEnd).filterNot(_ == -1),
        columnEnd = Option(metaData.columnNumberEnd).filterNot(_ == -1),
        offset = Option(metaData.lineNumber, metaData.lineNumberEnd + 1),
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

  protected def nilLiteral(span: TextSpan): StaticLiteral = StaticLiteral(getBuiltInType(Defines.NilClass))(span)

  def createClassBodyAndFields(
    obj: ujson.Obj
  )(implicit visit: ujson.Value => RubyExpression): (StatementList, List[RubyExpression & RubyFieldIdentifier]) = {

    def bodyMethod(fieldStatements: List[RubyExpression]): MethodDeclaration = {

      val body = fieldStatements.flatMap {
        case field: SimpleIdentifier =>
          val assignmentSpan = field.span.spanStart(s"${field.span} = nil")
          Option(SingleAssignment(ClassFieldIdentifier()(field.span), "=", nilLiteral(field.span))(assignmentSpan))
        case field: RubyFieldIdentifier =>
          val assignmentSpan = field.span.spanStart(s"${field.span} = nil")
          Option(SingleAssignment(field, "=", nilLiteral(field.span))(assignmentSpan))
        case assignment @ SingleAssignment(_: RubyFieldIdentifier, _, _) => Option(assignment)
        case assignment @ SingleAssignment(lhs: SimpleIdentifier, op, _) =>
          val fieldAssignment = assignment.copy(lhs = ClassFieldIdentifier()(lhs.span))(assignment.span)
          Option(fieldAssignment)
        case _ => None
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
    def getField(expr: RubyExpression): Option[RubyExpression & RubyFieldIdentifier] = {
      expr match {
        case field: SimpleIdentifier                             => Option(ClassFieldIdentifier()(field.span))
        case field: RubyFieldIdentifier                          => Option(field)
        case _ @SingleAssignment(lhs: RubyFieldIdentifier, _, _) => Option(lhs)
        case _ @SingleAssignment(lhs: SimpleIdentifier, _, _)    => Option(ClassFieldIdentifier()(lhs.span))
        case _                                                   => None
      }
    }

    obj.visitOption(ParserKeys.Body) match {
      case Some(stmtList: StatementList) =>
        val (fieldStmts, otherStmts) = stmtList.statements.partition(isFieldStmt)
        val body                     = stmtList.copy(statements = bodyMethod(fieldStmts) +: otherStmts)(stmtList.span)
        val fields                   = fieldStmts.flatMap(getField)
        (body, fields)
      case Some(expression) if isFieldStmt(expression) =>
        (StatementList(bodyMethod(expression :: Nil) :: Nil)(obj.toTextSpan), getField(expression).toList)
      case Some(expression) =>
        (StatementList(bodyMethod(Nil) :: expression :: Nil)(obj.toTextSpan), Nil)
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

  private case class MetaData(
    code: String,
    @upickle.implicits.key("start_line") lineNumber: Int,
    @upickle.implicits.key("start_column") columnNumber: Int,
    @upickle.implicits.key("end_line") lineNumberEnd: Int,
    @upickle.implicits.key("end_column") columnNumberEnd: Int
  ) derives ReadWriter

}
