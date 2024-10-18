package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.astcreation.RubyIntermediateAst.{
  AllowedTypeDeclarationChild,
  ClassFieldIdentifier,
  MemberAccess,
  MethodDeclaration,
  ProcedureDeclaration,
  RubyCall,
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
import io.joern.rubysrc2cpg.parser.RubyJsonHelpers.nilLiteral
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

    obj.visitOption(ParserKeys.Body) match {
      case Some(stmtList @ StatementList(expression :: Nil)) if expression.isInstanceOf[AllowedTypeDeclarationChild] =>
        (stmtList, getFields(expression))
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
      case Some(expression) if isFieldStmt(expression) || !expression.isInstanceOf[AllowedTypeDeclarationChild] =>
        (StatementList(bodyMethod(expression +: getFields(expression)) :: Nil)(obj.toTextSpan), getFields(expression))
      case Some(expression) =>
        (StatementList(bodyMethod(Nil) :: expression :: Nil)(obj.toTextSpan), getFields(expression))
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

  private case class MetaData(
    code: String,
    @upickle.implicits.key("start_line") lineNumber: Int,
    @upickle.implicits.key("start_column") columnNumber: Int,
    @upickle.implicits.key("end_line") lineNumberEnd: Int,
    @upickle.implicits.key("end_column") columnNumberEnd: Int
  ) derives ReadWriter

}
