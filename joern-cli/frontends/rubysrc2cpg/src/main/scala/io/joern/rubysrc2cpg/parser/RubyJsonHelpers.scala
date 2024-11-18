package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.astcreation.RubyIntermediateAst.{
  AliasStatement,
  AllowedTypeDeclarationChild,
  ArrayLiteral,
  ArrayParameter,
  ClassFieldIdentifier,
  ControlFlowStatement,
  DefaultMultipleAssignment,
  FieldsDeclaration,
  ForExpression,
  IfExpression,
  MemberAccess,
  MethodDeclaration,
  ProcParameter,
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

    /** @param expr
      *   An expression that is a direct child to a class or module.
      * @return
      *   true if the expression is a Splatting Field Declaration (`attr_x(*foo)`), false otherwise.
      */
    def isSplattingField(expr: RubyExpression): Boolean = {
      expr match {
        case x: FieldsDeclaration if x.isSplattingFieldDecl => true
        case _: AllowedTypeDeclarationChild                 => false
        case _                                              => false
      }
    }

    /** Extracts a field from the expression.
      * @param expr
      *   An expression that is a direct child to a class or module.
      */
    def getFields(
      expr: RubyExpression,
      typeDeclChildStatements: Boolean = true
    ): List[RubyExpression & RubyFieldIdentifier] = {
      expr match {
        case field: SimpleIdentifier if typeDeclChildStatements    => ClassFieldIdentifier()(field.span) :: Nil
        case field: RubyFieldIdentifier if typeDeclChildStatements => field :: Nil
        case _ @SingleAssignment(lhs: RubyFieldIdentifier, _, _)   => lhs :: Nil
        case _ @SingleAssignment(lhs: SimpleIdentifier, _, _) if typeDeclChildStatements =>
          ClassFieldIdentifier()(lhs.span) :: Nil
        case proc: ProcedureDeclaration => getFields(proc.body, false)
        case _ @StatementList(stmts)    => stmts.flatMap(x => getFields(x, typeDeclChildStatements)).distinctBy(_.text)
        case _                          => Nil
      }
    }

    /** Attempts to evaluate and parse the collection associated with the splattingField, generating FieldDeclarations
      * for each of the elements.
      * @param fieldStmts
      *   List of all the field statements
      * @param splattingFields
      *   List of splatting fields
      * @return
      *   List of:
      *   - Some(_) => if splattingField either is evaluated to a list of FieldDeclarations, otherwise a SimpleCall
      *   - None => if splattingField cannot be evaluated to either FieldsDeclaration or SimpleCall
      */
    def lowerSplattingFieldDecl(
      fieldStmts: List[RubyExpression],
      splattingFields: List[RubyExpression]
    ): List[Option[RubyExpression]] = {
      splattingFields.flatMap {
        case x @ FieldsDeclaration(fieldName :: Nil, accessType) if fieldName.isInstanceOf[SplattingRubyNode] =>
          fieldStmts.map {
            case _ @SingleAssignment(lhs: SimpleIdentifier, _, rhs: MemberAccess)
                if rhs.memberName == "freeze" && lhs.span.text == fieldName.span.text.stripPrefix("*") =>
              rhs.target match {
                case y: ArrayLiteral =>
                  Some(FieldsDeclaration(y.elements, accessType)(x.span))
                case _ => None
              }
            case _ @SingleAssignment(_: SimpleIdentifier, _, rhs: ArrayLiteral) =>
              Some(FieldsDeclaration(rhs.elements, accessType)(x.span))
            case _ =>
              Some(
                SimpleCall(SimpleIdentifier()(x.span.spanStart(accessType)), List(fieldName))(
                  x.span.spanStart(s"$accessType(${fieldName.span.text})")
                )
              )
          }
        case _ => None
      }
    }

    obj.visitOption(ParserKeys.Body).map(lowerAliasStatementsToMethods) match {
      case Some(stmtList @ StatementList(expression :: Nil)) if expression.isInstanceOf[AllowedTypeDeclarationChild] =>
        if (isSplattingField(expression)) {
          val splattingField = expression.asInstanceOf[FieldsDeclaration]
          splattingField.fieldNames.headOption match {
            case Some(splattingFieldName) =>
              val nonExpandedSplattingFieldCall =
                SimpleCall(
                  SimpleIdentifier()(expression.span.spanStart(splattingField.accessType)),
                  List(splattingFieldName)
                )(expression.span.spanStart(s"${splattingField.accessType}(${splattingFieldName.span.text})"))
              (
                StatementList(bodyMethod(List(nonExpandedSplattingFieldCall)) :: Nil)(stmtList.span),
                getFields(expression)
              )
            case None =>
              logger.warn(s"No fieldName found for Splatting Field Decl: ${splattingField.span.text}")
              (StatementList(bodyMethod(Nil) :: expression :: Nil)(stmtList.span), getFields(expression))
          }
        } else {
          (StatementList(bodyMethod(Nil) :: expression :: Nil)(stmtList.span), getFields(expression))
        }
      case Some(stmtList @ StatementList(expression :: Nil)) if isFieldStmt(expression) =>
        (StatementList(bodyMethod(expression :: Nil) :: Nil)(stmtList.span), getFields(expression))
      case Some(stmtList: StatementList) =>
        val (fieldStmts, otherStmts)              = stmtList.statements.partition(isFieldStmt)
        val (typeDeclStmts, bodyStmts)            = otherStmts.partition(_.isInstanceOf[AllowedTypeDeclarationChild])
        val (splattingFields, otherTypeDeclStmts) = typeDeclStmts.partition(isSplattingField)
        val (expandedSplattingFields, nonExpandedSplattingFieldsCalls) =
          lowerSplattingFieldDecl(fieldStmts, splattingFields)
            .filter(_.isDefined)
            .map(_.get)
            .partition(_.isInstanceOf[FieldsDeclaration])

        val fields =
          (fieldStmts.flatMap(x => getFields(x)) ++ otherTypeDeclStmts.flatMap(x => getFields(x)))
            .distinctBy(_.text)
        val body = stmtList.copy(statements =
          bodyMethod(
            fieldStmts ++ otherTypeDeclStmts.flatMap(x => getFields(x)) ++ bodyStmts ++ nonExpandedSplattingFieldsCalls
          ) +: (otherTypeDeclStmts ++ expandedSplattingFields)
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

    val transformedStmts = stmts.map {
      case alias: AliasStatement =>
        val span                 = alias.span
        val forwardingCallTarget = SimpleIdentifier(None)(span.spanStart(alias.oldName))
        val forwardedArgs  = SplattingRubyNode(SimpleIdentifier()(span.spanStart("args")))(span.spanStart("*args"))
        val forwardedBlock = SimpleIdentifier()(span.spanStart("&block"))
        val forwardingCall = SimpleCall(forwardingCallTarget, forwardedArgs :: forwardedBlock :: Nil)(
          span.spanStart(s"${alias.oldName}(*args, &block)")
        )

        val aliasMethodBody = StatementList(forwardingCall :: Nil)(forwardingCall.span)
        val aliasingMethodParams =
          ArrayParameter("*args")(span.spanStart("*args")) :: ProcParameter("&block")(span.spanStart("&block")) :: Nil
        MethodDeclaration(alias.newName, aliasingMethodParams, aliasMethodBody)(
          alias.span.spanStart(s"def ${alias.newName}(*args, &block)")
        )
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
