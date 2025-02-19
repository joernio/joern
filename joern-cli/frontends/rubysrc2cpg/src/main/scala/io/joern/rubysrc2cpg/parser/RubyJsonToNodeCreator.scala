package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.astcreation.RubyIntermediateAst.*
import io.joern.rubysrc2cpg.parser.AstType.Send
import io.joern.rubysrc2cpg.parser.RubyJsonHelpers.*
import io.joern.rubysrc2cpg.passes.Defines
import io.joern.rubysrc2cpg.passes.Defines.{NilClass, RubyOperators}
import io.joern.rubysrc2cpg.passes.GlobalTypes.corePrefix
import io.joern.rubysrc2cpg.utils.FreshNameGenerator
import io.joern.x2cpg.frontendspecific.rubysrc2cpg.ImportsPass
import io.joern.x2cpg.frontendspecific.rubysrc2cpg.ImportsPass.ImportCallNames
import org.slf4j.LoggerFactory
import ujson.*

class RubyJsonToNodeCreator(
  variableNameGen: FreshNameGenerator[String] = FreshNameGenerator(id => s"<tmp-$id>"),
  procParamGen: FreshNameGenerator[Left[String, Nothing]] = FreshNameGenerator(id => Left(s"<proc-param-$id>")),
  fileName: String = ""
) {

  private val logger       = LoggerFactory.getLogger(getClass)
  private val classNameGen = FreshNameGenerator(id => s"<anon-class-$id>")

  private implicit val implVisit: ujson.Value => RubyExpression = (x: ujson.Value) => visit(x)

  protected def freshClassName(span: TextSpan): SimpleIdentifier = {
    SimpleIdentifier(None)(span.spanStart(classNameGen.fresh))
  }

  private def defaultTextSpan(code: String = ""): TextSpan = TextSpan(None, None, None, None, None, code)

  private def defaultResult(span: Option[TextSpan] = None): RubyExpression =
    Unknown()(span.getOrElse(defaultTextSpan()))

  private def visit(v: ujson.Value): RubyExpression = {
    v match {
      case obj: ujson.Obj => visit(obj)
      case ujson.Null     => StatementList(Nil)(defaultTextSpan())
      case ujson.Str(x)   => StaticLiteral(Defines.prefixAsCoreType(Defines.String))(defaultTextSpan(x))
      case x =>
        logger.warn(s"Unhandled ujson type ${x.getClass}")
        defaultResult()
    }
  }

  /** Main entrypoint of JSON deserialization.
    */
  def visitProgram(obj: ujson.Value): StatementList = {
    visit(obj.obj) match {
      case x: StatementList => x
      case x                => StatementList(x :: Nil)(x.span)
    }
  }

  private def visit(obj: ujson.Obj): RubyExpression = {

    def visitAstType(typ: AstType): RubyExpression = {
      typ match {
        case AstType.Alias                        => visitAlias(obj)
        case AstType.And                          => visitAnd(obj)
        case AstType.AndAssign                    => visitAndAssign(obj)
        case AstType.Arg                          => visitArg(obj)
        case AstType.Args                         => visitArgs(obj)
        case AstType.Array                        => visitArray(obj)
        case AstType.ArrayPattern                 => visitArrayPattern(obj)
        case AstType.ArrayPatternWithTail         => visitArrayPatternWithTail(obj)
        case AstType.BackRef                      => visitBackRef(obj)
        case AstType.Begin                        => visitBegin(obj)
        case AstType.Block                        => visitBlock(obj)
        case AstType.BlockArg                     => visitBlockArg(obj)
        case AstType.BlockPass                    => visitBlockPass(obj)
        case AstType.BlockWithNumberedParams      => visitBlockWithNumberedParams(obj)
        case AstType.Break                        => visitBreak(obj)
        case AstType.CaseExpression               => visitCaseExpression(obj)
        case AstType.CaseMatchStatement           => visitCaseMatchStatement(obj)
        case AstType.ClassDefinition              => visitClassDefinition(obj)
        case AstType.ClassVariable                => visitClassVariable(obj)
        case AstType.ClassVariableAssign          => visitSingleAssignment(obj)
        case AstType.ConstVariableAssign          => visitSingleAssignment(obj)
        case AstType.ConditionalSend              => visitSend(obj, isConditional = true)
        case AstType.Defined                      => visitDefined(obj)
        case AstType.DynamicString                => visitDynamicString(obj)
        case AstType.DynamicSymbol                => visitDynamicSymbol(obj)
        case AstType.Ensure                       => visitEnsure(obj)
        case AstType.ExclusiveFlipFlop            => visitExclusiveFlipFlop(obj)
        case AstType.ExclusiveRange               => visitExclusiveRange(obj)
        case AstType.ExecutableString             => visitExecutableString(obj)
        case AstType.False                        => visitFalse(obj)
        case AstType.FindPattern                  => visitFindPattern(obj)
        case AstType.Float                        => visitFloat(obj)
        case AstType.ForStatement                 => visitForStatement(obj)
        case AstType.ForPostStatement             => visitForStatement(obj)
        case AstType.ForwardArg                   => visitForwardArg(obj)
        case AstType.ForwardArgs                  => visitForwardArgs(obj)
        case AstType.ForwardedArgs                => visitForwardedArgs(obj)
        case AstType.GlobalVariable               => visitGlobalVariable(obj)
        case AstType.GlobalVariableAssign         => visitGlobalVariableAssign(obj)
        case AstType.Hash                         => visitHash(obj)
        case AstType.HashPattern                  => visitHashPattern(obj)
        case AstType.Identifier                   => visitIdentifier(obj)
        case AstType.IfGuard                      => visitIfGuard(obj)
        case AstType.IfStatement                  => visitIfStatement(obj)
        case AstType.InclusiveFlipFlop            => visitInclusiveFlipFlop(obj)
        case AstType.InclusiveRange               => visitInclusiveRange(obj)
        case AstType.InPattern                    => visitInPattern(obj)
        case AstType.Int                          => visitInt(obj)
        case AstType.InstanceVariable             => visitInstanceVariable(obj)
        case AstType.InstanceVariableAssign       => visitSingleAssignment(obj)
        case AstType.KwArg                        => visitKwArg(obj)
        case AstType.KwBegin                      => visitKwBegin(obj)
        case AstType.KwNilArg                     => visitKwNilArg(obj)
        case AstType.KwOptArg                     => visitKwOptArg(obj)
        case AstType.KwRestArg                    => visitKwRestArg(obj)
        case AstType.KwSplat                      => visitKwSplat(obj)
        case AstType.LocalVariable                => visitLocalVariable(obj)
        case AstType.LocalVariableAssign          => visitSingleAssignment(obj)
        case AstType.MatchAlt                     => visitMatchAlt(obj)
        case AstType.MatchAs                      => visitMatchAs(obj)
        case AstType.MatchNilPattern              => visitMatchNilPattern(obj)
        case AstType.MatchPattern                 => visitMatchPattern(obj)
        case AstType.MatchPatternP                => visitMatchPatternP(obj)
        case AstType.MatchRest                    => visitMatchRest(obj)
        case AstType.MatchVariable                => visitMatchVariable(obj)
        case AstType.MatchWithLocalVariableAssign => visitMatchWithLocalVariableAssign(obj)
        case AstType.MethodDefinition             => visitMethodDefinition(obj)
        case AstType.ModuleDefinition             => visitModuleDefinition(obj)
        case AstType.MultipleAssignment           => visitMultipleAssignment(obj)
        case AstType.MultipleLeftHandSide         => visitMultipleLeftHandSide(obj)
        case AstType.Next                         => visitNext(obj)
        case AstType.Nil                          => visitNil(obj)
        case AstType.NthRef                       => visitNthRef(obj)
        case AstType.OperatorAssign               => visitOperatorAssign(obj)
        case AstType.OptionalArgument             => visitOptionalArgument(obj)
        case AstType.Or                           => visitOr(obj)
        case AstType.OrAssign                     => visitOrAssign(obj)
        case AstType.Pair                         => visitPair(obj)
        case AstType.PostExpression               => visitPostExpression(obj)
        case AstType.PreExpression                => visitPreExpression(obj)
        case AstType.ProcArgument                 => visitProcArgument(obj)
        case AstType.Rational                     => visitRational(obj)
        case AstType.Redo                         => visitRedo(obj)
        case AstType.Retry                        => visitRetry(obj)
        case AstType.Return                       => visitReturn(obj)
        case AstType.RegexExpression              => visitRegexExpression(obj)
        case AstType.RegexOption                  => visitRegexOption(obj)
        case AstType.ResBody                      => visitResBody(obj)
        case AstType.RestArg                      => visitRestArg(obj)
        case AstType.RescueStatement              => visitRescueStatement(obj)
        case AstType.ScopedConstant               => visitScopedConstant(obj)
        case AstType.Self                         => visitSelf(obj)
        case AstType.Send                         => visitSend(obj)
        case AstType.ShadowArg                    => visitShadowArg(obj)
        case AstType.SingletonMethodDefinition    => visitSingletonMethodDefinition(obj)
        case AstType.SingletonClassDefinition     => visitSingletonClassDefinition(obj)
        case AstType.Splat                        => visitSplat(obj)
        case AstType.StaticString                 => visitStaticString(obj)
        case AstType.StaticSymbol                 => visitStaticSymbol(obj)
        case AstType.Super                        => visitSuper(obj)
        case AstType.SuperNoArgs                  => visitSuperNoArgs(obj)
        case AstType.TopLevelConstant             => visitTopLevelConstant(obj)
        case AstType.True                         => visitTrue(obj)
        case AstType.UnDefine                     => visitUnDefine(obj)
        case AstType.UnlessExpression             => visitUnlessExpression(obj)
        case AstType.UnlessGuard                  => visitUnlessGuard(obj)
        case AstType.UntilExpression              => visitUntilExpression(obj)
        case AstType.UntilPostExpression          => visitUntilPostExpression(obj)
        case AstType.WhenStatement                => visitWhenStatement(obj)
        case AstType.WhileStatement               => visitWhileStatement(obj)
        case AstType.WhilePostStatement           => visitWhileStatement(obj)
        case AstType.Yield                        => visitYield(obj)
      }
    }

    val astTypeStr = obj(ParserKeys.Type).str
    AstType.fromString(astTypeStr) match {
      case Some(typ) => visitAstType(typ)
      case _ =>
        logger.warn(s"Unhandled `parser` type '$astTypeStr'")
        defaultResult()
    }
  }

  private def visitAccessModifier(obj: Obj): RubyExpression = {
    obj(ParserKeys.Name).str match {
      case "public"    => PublicModifier()(obj.toTextSpan)
      case "private"   => PrivateModifier()(obj.toTextSpan)
      case "protected" => ProtectedModifier()(obj.toTextSpan)
      case modifierName =>
        logger.warn(s"Unknown modifier type $modifierName")
        defaultResult(Option(obj.toTextSpan))
    }
  }

  private def visitAlias(obj: Obj): RubyExpression = {
    if (AstType.fromString(obj(ParserKeys.Type).str).contains(AstType.Send)) {
      obj.visitArray(ParserKeys.Arguments) match {
        case name :: alias :: _ => // different order than the normal `alias` kw
          AliasStatement(alias.text.stripPrefix(":"), name.text.stripPrefix(":"))(obj.toTextSpan)
        case _ => defaultResult(Option(obj.toTextSpan))
      }
    } else {
      val name  = visit(obj(ParserKeys.Name)).text.stripPrefix(":")
      val alias = visit(obj(ParserKeys.Alias)).text.stripPrefix(":")
      AliasStatement(alias, name)(obj.toTextSpan)
    }

  }

  private def visitAnd(obj: Obj): RubyExpression = {
    val op  = "&&"
    val lhs = visit(obj(ParserKeys.Lhs))
    val rhs = visit(obj(ParserKeys.Rhs))
    BinaryExpression(lhs, op, rhs)(obj.toTextSpan)
  }

  private def visitAndAssign(obj: Obj): RubyExpression = {
    val lhs = visit(obj(ParserKeys.Lhs)) match {
      case param: MandatoryParameter => param.toSimpleIdentifier
      case x                         => x
    }
    val rhs = visit(obj(ParserKeys.Rhs))
    OperatorAssignment(lhs, "&&=", rhs)(obj.toTextSpan)
  }

  private def visitArg(obj: Obj): RubyExpression = MandatoryParameter(obj(ParserKeys.Value).str)(obj.toTextSpan)

  private def visitArgs(obj: Obj): RubyExpression = defaultResult(Option(obj.toTextSpan))

  private def visitArray(obj: Obj): RubyExpression = {
    val children = obj.visitArray(ParserKeys.Children).flatMap {
      case x: AssociationList => x.elements
      case x                  => x :: Nil
    }

    ArrayLiteral(children)(obj.toTextSpan)
  }

  private def visitArrayPattern(obj: Obj): RubyExpression = {
    val children = obj.visitArray(ParserKeys.Children)
    ArrayPattern(children)(obj.toTextSpan)
  }

  private def visitArrayPatternWithTail(obj: Obj): RubyExpression = defaultResult(Option(obj.toTextSpan))

  private def visitBackRef(obj: Obj): RubyExpression = SimpleIdentifier()(obj.toTextSpan)

  private def visitBegin(obj: Obj): RubyExpression = {
    StatementList(obj.visitArray(ParserKeys.Body))(obj.toTextSpan)
  }

  private def visitGroupedParameter(arrayParam: ArrayLiteral): RubyExpression = {
    val freshTmpVar       = variableNameGen.fresh
    val tmpMandatoryParam = MandatoryParameter(freshTmpVar)(arrayParam.span.spanStart(freshTmpVar))

    val singleAssignments = arrayParam.elements.map { param =>
      val rhsSplattingNode = SplattingRubyNode(tmpMandatoryParam)(arrayParam.span.spanStart(s"*$freshTmpVar"))
      val lhs = param match {
        case x: SimpleIdentifier => SimpleIdentifier()(x.span)
        case x: ArrayParameter =>
          SplattingRubyNode(SimpleIdentifier()(arrayParam.span.spanStart(x.span.text.stripPrefix("*"))))(
            arrayParam.span.spanStart(x.span.text)
          )
        case x: ArrayLiteral =>
          visitGroupedParameter(x)
        case x =>
          logger.warn(
            s"Invalid parameter type in grouped parameter list: ${x.getClass} (code: ${arrayParam.span.text})"
          )
          defaultResult(Option(arrayParam.span))
      }
      SingleAssignment(lhs, "=", rhsSplattingNode)(
        arrayParam.span.spanStart(s"${lhs.span.text} = ${rhsSplattingNode.span.text}")
      )
    }

    GroupedParameter(
      tmpMandatoryParam.span.text,
      tmpMandatoryParam,
      GroupedParameterDesugaring(singleAssignments)(arrayParam.span)
    )(arrayParam.span)
  }

  private def visitBlock(obj: Obj): RubyExpression = {
    val parameters = obj(ParserKeys.Arguments).asInstanceOf[ujson.Obj].visitArray(ParserKeys.Children).map {
      case x: ArrayLiteral => visitGroupedParameter(x)
      case x               => x
    }

    val assignments = parameters.collect { case x: GroupedParameter =>
      x.multipleAssignment
    }

    val body = obj.visitOption(ParserKeys.Body) match {
      case Some(stmt: StatementList) => stmt.copy(stmt.statements ++ assignments)(stmt.span)
      case Some(expr)                => StatementList(expr +: assignments)(expr.span)
      case None                      => StatementList(Nil)(obj.toTextSpan)
    }

    val block = Block(parameters, body)(body.span.spanStart(obj.toTextSpan.text))
    visit(obj(ParserKeys.CallName)) match {
      case classNew: ObjectInstantiation if classNew.span.text == "Class.new" =>
        AnonymousClassDeclaration(freshClassName(obj.toTextSpan), None, block.toStatementList)(obj.toTextSpan)
      case objNew: ObjectInstantiation                         => objNew.withBlock(block)
      case lambda: SimpleIdentifier if lambda.text == "lambda" => ProcOrLambdaExpr(block)(obj.toTextSpan)
      case ident: SimpleIdentifier if ident.span.text == "loop" =>
        val trueLiteral = StaticLiteral(Defines.prefixAsCoreType(Defines.TrueClass))(ident.span.spanStart("true"))
        DoWhileExpression(trueLiteral, body)(ident.span)
      case simpleIdentifier: SimpleIdentifier =>
        SimpleCall(simpleIdentifier, Nil)(obj.toTextSpan).withBlock(block)
      case simpleCall: RubyCall => simpleCall.withBlock(block)
      case memberAccess @ MemberAccess(target, op, memberName) =>
        val memberCall = MemberCall(target, op, memberName, List.empty)(memberAccess.span)
        memberCall.withBlock(block)
      case x: ProtectedModifier =>
        SimpleCall(x.toSimpleIdentifier, Nil)(obj.toTextSpan).withBlock(block)
      case x =>
        logger.warn(s"Unexpected call type used for block ${x.getClass}, ignoring block")
        x
    }
  }

  private def visitBlockArg(obj: Obj): RubyExpression = {
    val span = obj.toTextSpan
    val name = obj(ParserKeys.Value).strOpt.filterNot(_ == "&").getOrElse(procParamGen.fresh.value)
    ProcParameter(name)(span)
  }

  private def visitBlockPass(obj: Obj): RubyExpression = {
    lazy val default = SimpleIdentifier()(obj.toTextSpan.spanStart(procParamGen.current.value))
    obj.visitOption(ParserKeys.Value).getOrElse(default)
  }

  private def visitBlockWithNumberedParams(obj: Obj): RubyExpression = SimpleIdentifier()(obj.toTextSpan)

  private def visitBracketAssignmentAsSend(obj: Obj): RubyExpression = {
    val lhsBase = visit(obj(ParserKeys.Receiver))
    val args    = obj.visitArray(ParserKeys.Arguments)

    val lhs =
      IndexAccess(lhsBase, List(args.head))(obj.toTextSpan.spanStart(s"${lhsBase.span.text}[${args.head.span.text}]"))

    val rhs =
      if args.size == 2 then args(1)
      else SimpleIdentifier()(obj.toTextSpan.spanStart("*"))

    SingleAssignment(lhs, "=", rhs)(obj.toTextSpan)
  }

  private def visitBreak(obj: Obj): RubyExpression = BreakExpression()(obj.toTextSpan)

  private def visitCaseExpression(obj: Obj): RubyExpression = {
    val expression  = obj.visitOption(ParserKeys.CaseExpression)
    val whenClauses = obj.visitArray(ParserKeys.WhenClauses)

    val elseClause = obj.visitOption(ParserKeys.ElseClause) match {
      case Some(elseClause) => Some(ElseClause(elseClause)(elseClause.span))
      case None             => None
    }

    CaseExpression(expression, whenClauses, elseClause)(obj.toTextSpan)
  }

  private def visitCaseMatchStatement(obj: Obj): RubyExpression = {
    val expression = visit(obj(ParserKeys.Statement))
    val inClauses  = obj.visitArray(ParserKeys.Bodies)
    val elseClause = obj.visitOption(ParserKeys.ElseClause).map(x => ElseClause(x)(x.span))

    CaseExpression(Some(expression), inClauses, elseClause)(obj.toTextSpan)
  }

  private def visitClassDefinition(obj: Obj): RubyExpression = {
    val (name, namespaceParts) = visit(obj(ParserKeys.Name)) match {
      case memberAccess: MemberAccess =>
        val memberIdentifier = SimpleIdentifier()(memberAccess.span.spanStart(memberAccess.memberName))
        (memberIdentifier, Option(getParts(memberAccess).dropRight(1)))
      case identifier => (identifier, None)
    }
    val baseClass      = obj.visitOption(ParserKeys.SuperClass)
    val (body, fields) = createClassBodyAndFields(obj)
    val bodyMemberCall = createBodyMemberCall(name.text, obj.toTextSpan)
    ClassDeclaration(
      name = name,
      baseClass = baseClass,
      body = body,
      fields = fields,
      bodyMemberCall = Option(bodyMemberCall),
      namespaceParts = namespaceParts
    )(obj.toTextSpan)
  }

  private def visitClassVariable(obj: Obj): RubyExpression = ClassFieldIdentifier()(obj.toTextSpan)

  private def visitCollectionAliasSend(obj: Obj): RubyExpression = {
    // Modify this `obj` to conform to what the AstCreator would expect i.e, Array [1,2,3] would be an Array::[] call
    val collectionName = obj(ParserKeys.Name).str
    val metaData       = obj(ParserKeys.MetaData)
    metaData.obj.put(ParserKeys.Code, collectionName)
    val receiver = ujson.Obj(
      ParserKeys.Type     -> ujson.Str(AstType.ScopedConstant.name),
      ParserKeys.MetaData -> metaData,
      ParserKeys.Base     -> ujson.Null,
      ParserKeys.Name     -> ujson.Str(collectionName)
    )
    val arguments = obj(ParserKeys.Arguments).arr.headOption
      .flatMap {
        case x: ujson.Obj => AstType.fromString(x(ParserKeys.Type).str).map(t => t -> x)
        case _            => None
      }
      .map {
        case (AstType.Array, o) =>
          o.visitArray(ParserKeys.Children).flatMap {
            case x: AssociationList => x.elements
            case x                  => x :: Nil
          }
        case (_, o) =>
          visit(o) :: Nil
      }
      .getOrElse(Nil)

    val textSpan = obj.toTextSpan.spanStart(s"$collectionName [${arguments.map(_.span.text).mkString(", ")}]")

    IndexAccess(visit(receiver), arguments)(textSpan)
  }

  private def visitDefined(obj: Obj): RubyExpression = {
    val name =
      SimpleIdentifier(Option(Defines.prefixAsKernelDefined(Defines.Defined)))(
        obj.toTextSpan.spanStart(Defines.Defined)
      )
    val arguments = obj.visitArray(ParserKeys.Arguments)
    SimpleCall(name, arguments)(obj.toTextSpan)
  }

  private def visitDynamicString(obj: Obj): RubyExpression = {
    val typeFullName = Defines.prefixAsCoreType(Defines.String)
    val expressions  = obj.visitArray(ParserKeys.Children)
    DynamicLiteral(typeFullName, expressions)(obj.toTextSpan)
  }

  private def visitDynamicSymbol(obj: Obj): RubyExpression = {
    val typeFullName = Defines.prefixAsCoreType(Defines.Symbol)
    val expressions  = obj.visitArray(ParserKeys.Children)
    DynamicLiteral(typeFullName, expressions)(obj.toTextSpan)
  }

  private def visitEnsure(obj: Obj): RubyExpression = {
    val ensureClause = EnsureClause(visit(obj(ParserKeys.Body)))(obj.toTextSpan)
    visit(obj(ParserKeys.Statement)) match {
      case rescueExpression: RescueExpression =>
        rescueExpression.copy(
          rescueExpression.body,
          rescueExpression.rescueClauses,
          rescueExpression.elseClause,
          Some(ensureClause)
        )(obj.toTextSpan)
      case x =>
        RescueExpression(x, List.empty, Option.empty, Some(ensureClause))(obj.toTextSpan)
    }
  }

  private def visitExclusiveFlipFlop(obj: Obj): RubyExpression = defaultResult(Option(obj.toTextSpan))

  private def visitExclusiveRange(obj: Obj): RubyExpression = {
    val start = visit(obj(ParserKeys.Start))
    val end   = visit(obj(ParserKeys.End))
    val op    = RangeOperator(true)(obj.toTextSpan.spanStart("..."))
    RangeExpression(start, end, op)(obj.toTextSpan)
  }

  private def visitExecutableString(obj: Obj): RubyExpression = {
    val operatorName = RubyOperators.backticks
    val callName =
      SimpleIdentifier(Option(Defines.prefixAsKernelDefined(operatorName)))(obj.toTextSpan.spanStart(operatorName))
    val arguments = obj.visitArray(ParserKeys.Arguments)
    SimpleCall(callName, arguments)(obj.toTextSpan)
  }

  private def visitFalse(obj: Obj): RubyExpression =
    StaticLiteral(Defines.prefixAsCoreType(Defines.FalseClass))(obj.toTextSpan)

  private def visitFieldDeclaration(obj: Obj): RubyExpression = {
    val arguments  = obj.visitArray(ParserKeys.Arguments)
    val accessType = obj(ParserKeys.Name).str
    FieldsDeclaration(arguments, accessType)(obj.toTextSpan)
  }

  private def visitFindPattern(obj: Obj): RubyExpression = defaultResult(Option(obj.toTextSpan))

  private def visitFieldAssignmentSend(obj: Obj, fieldName: String): RubyExpression = {
    val span         = obj.toTextSpan
    val receiver     = visit(obj(ParserKeys.Receiver))
    val memberAccess = MemberAccess(receiver, ".", fieldName)(receiver.span.spanStart(s"${receiver.text}.@$fieldName"))
    val argument = obj
      .visitArray(ParserKeys.Arguments)
      .headOption
      .getOrElse(StaticLiteral(Defines.prefixAsCoreType(Defines.NilClass))(span.spanStart("nil")))
    SingleAssignment(memberAccess, "=", argument)(span)
  }

  private def visitFloat(obj: Obj): RubyExpression =
    StaticLiteral(Defines.prefixAsCoreType(Defines.Float))(obj.toTextSpan)

  private def visitForStatement(obj: Obj): RubyExpression = {
    val forVariable      = visit(obj(ParserKeys.Variable))
    val iterableVariable = visit(obj(ParserKeys.Collection))
    val doBlock = visit(obj(ParserKeys.Body)) match {
      case stmtList: StatementList => stmtList
      case other                   => StatementList(List(other))(other.span)
    }

    ForExpression(forVariable, iterableVariable, doBlock)(obj.toTextSpan)
  }

  private def visitForwardArg(obj: Obj): RubyExpression = {
    logger.warn("Forward arg unhandled")
    defaultResult(Option(obj.toTextSpan))
  }

  // Note: Forward args should probably be handled more explicitly, but this should preserve flows if the same
  // identifier is used in latter forwarding
  private def visitForwardArgs(obj: Obj): RubyExpression = MandatoryParameter("...")(obj.toTextSpan)

  private def visitForwardedArgs(obj: Obj): RubyExpression = SimpleIdentifier()(obj.toTextSpan)

  private def visitGlobalVariable(obj: Obj): RubyExpression = {
    val span     = obj.toTextSpan
    val name     = obj(ParserKeys.Value).str
    val selfBase = SelfIdentifier()(span.spanStart("self"))
    MemberAccess(selfBase, ".", name)(span)
  }

  private def visitGlobalVariableAssign(obj: Obj): RubyExpression = {
    val span = obj.toTextSpan

    val selfBase = SelfIdentifier()(span.spanStart("self"))
    val lhsName  = obj(ParserKeys.Lhs).str
    val lhs      = MemberAccess(selfBase, ".", lhsName)(span.spanStart(s"${selfBase.span.text}.$lhsName"))

    val rhs = visit(obj(ParserKeys.Rhs))
    val op  = "="

    SingleAssignment(lhs, op, rhs)(obj.toTextSpan)
  }

  private def visitHash(obj: Obj): RubyExpression = {
    val isHashLiteral = obj.toTextSpan.text.stripMargin.startsWith("{")

    obj.visitArray(ParserKeys.Children) match {
      case (assoc: Association) :: Nil =>
        if isHashLiteral then HashLiteral(List(assoc))(obj.toTextSpan)
        else assoc // 2 => 1 is interpreted as {2: 1}, so we lower this for now
      case children =>
        if isHashLiteral then HashLiteral(children)(obj.toTextSpan)
        else AssociationList(children)(obj.toTextSpan)
    }
  }

  private def visitHashPattern(obj: Obj): RubyExpression = defaultResult(Option(obj.toTextSpan))

  private def visitIdentifier(obj: Obj): RubyExpression = SimpleIdentifier()(obj.toTextSpan)

  private def visitIfGuard(obj: Obj): RubyExpression = defaultResult(Option(obj.toTextSpan))

  private def visitIfStatement(obj: Obj): RubyExpression = {
    val condition = visit(obj(ParserKeys.Condition))

    val elseClause = obj.visitOption(ParserKeys.ElseBranch).map {
      case x: IfExpression => x
      case x               => ElseClause(StatementList(List(x))(x.span))(x.span)
    }

    obj.visitOption(ParserKeys.ThenBranch) match {
      case Some(thenBranch) =>
        IfExpression(condition, thenBranch, elsifClauses = List.empty, elseClause)(obj.toTextSpan)
      case None =>
        val nilBlock = ReturnExpression(
          List(StaticLiteral(Defines.prefixAsCoreType(Defines.NilClass))(obj.toTextSpan.spanStart("nil")))
        )(obj.toTextSpan.spanStart("return nil"))
        IfExpression(condition, nilBlock, elsifClauses = List.empty, elseClause)(obj.toTextSpan)
    }
  }

  private def visitInclude(obj: Obj): RubyExpression = {
    val callName = obj(ParserKeys.Name).str
    val target   = SimpleIdentifier()(obj.toTextSpan.spanStart(callName))
    val argument = obj.visitArray(ParserKeys.Arguments).head

    IncludeCall(target, argument)(obj.toTextSpan)
  }

  private def visitInclusiveFlipFlop(obj: Obj): RubyExpression = defaultResult(Option(obj.toTextSpan))

  private def visitInclusiveRange(obj: Obj): RubyExpression = {
    val start = obj.visitOption(ParserKeys.Start) match {
      case Some(expr) => expr
      case None       => infinityLowerBound(obj)
    }
    val end = obj.visitOption(ParserKeys.End) match {
      case Some(expr) => expr
      case None       => infinityUpperBound(obj)
    }
    val op = RangeOperator(false)(obj.toTextSpan.spanStart(".."))
    RangeExpression(start, end, op)(obj.toTextSpan)
  }

  private def visitIndexAccessAsSend(obj: Obj): RubyExpression = {
    val target  = visit(obj(ParserKeys.Receiver))
    val indices = obj.visitArray(ParserKeys.Arguments)
    val isRegexMatch = indices.headOption.exists {
      case x: StaticLiteral => x.typeFullName == Defines.prefixAsCoreType(Defines.Regexp)
      case _                => false
    }
    if (isRegexMatch) {
      // For regex match that looks like "hello"[/h(el)lo/]
      val newProps = obj.value
      newProps.put(ParserKeys.Name, s"match${Defines.NeedsRegexLowering}")
      visitSend(obj.copy(value = newProps))
    } else {
      IndexAccess(target, indices)(obj.toTextSpan)
    }
  }

  private def visitInPattern(obj: Obj): RubyExpression = {
    val patternType = visit(obj(ParserKeys.Pattern))
    val patternBody = visit(obj(ParserKeys.Body))

    InClause(patternType, patternBody)(obj.toTextSpan)
  }

  private def visitInt(obj: Obj): RubyExpression = {
    val typeFullName = Defines.prefixAsCoreType(Defines.Integer)
    StaticLiteral(typeFullName)(obj.toTextSpan)
  }

  private def visitInstanceVariable(obj: Obj): RubyExpression = InstanceFieldIdentifier()(obj.toTextSpan)

  private def visitKwArg(obj: Obj): RubyExpression = {
    val name = obj(ParserKeys.Key).str
    val default = obj
      .visitOption(ParserKeys.Value)
      .getOrElse(StaticLiteral(Defines.prefixAsCoreType(Defines.NilClass))(obj.toTextSpan.spanStart("nil")))
    OptionalParameter(name, default)(obj.toTextSpan)
  }

  private def visitKwBegin(obj: Obj): RubyExpression = {
    val stmts = obj(ParserKeys.Body) match {
      case o: Obj => visit(o) :: Nil
      case _: Arr => obj.visitArray(ParserKeys.Body)
      case _ =>
        val span = obj.toTextSpan
        logger.warn(s"Unhandled JSON body type for `KwBegin`: ${span.text}")
        defaultResult(Option(span)) :: Nil
    }
    StatementList(stmts)(obj.toTextSpan)
  }

  private def visitKwNilArg(obj: Obj): RubyExpression = defaultResult(Option(obj.toTextSpan))

  private def visitKwOptArg(obj: Obj): RubyExpression = visitKwArg(obj)

  private def visitKwRestArg(obj: Obj): RubyExpression = {
    val name = if obj.contains(ParserKeys.Value) then obj(ParserKeys.Value).str else obj.toTextSpan.text
    HashParameter(name)(obj.toTextSpan)
  }

  private def visitKwSplat(obj: Obj): RubyExpression = {
    val values = visit(obj(ParserKeys.Value)) match {
      case x: StatementList => x.statements.head
      case x                => x
    }
    SplattingRubyNode(values)(obj.toTextSpan)
  }

  private def visitLocalVariable(obj: Obj): RubyExpression = SimpleIdentifier()(obj.toTextSpan)

  private def visitMatchAlt(obj: Obj): RubyExpression = defaultResult(Option(obj.toTextSpan))

  private def visitMatchAs(obj: Obj): RubyExpression = defaultResult(Option(obj.toTextSpan))

  private def visitMatchNilPattern(obj: Obj): RubyExpression = defaultResult(Option(obj.toTextSpan))

  private def visitMatchPattern(obj: Obj): RubyExpression = defaultResult(Option(obj.toTextSpan))

  private def visitMatchPatternP(obj: Obj): RubyExpression = defaultResult(Option(obj.toTextSpan))

  private def visitMatchRest(obj: Obj): RubyExpression = defaultResult(Option(obj.toTextSpan))

  private def visitMatchVariable(obj: Obj): RubyExpression = MatchVariable()(obj.toTextSpan)

  private def visitMatchWithLocalVariableAssign(obj: Obj): RubyExpression = {
    val lhs = visit(obj(ParserKeys.Lhs))
    val rhs = visit(obj(ParserKeys.Rhs))
    MemberCall(lhs, ".", RubyOperators.regexpMatch, rhs :: Nil)(obj.toTextSpan)
  }

  private def visitMethodAccessModifier(obj: Obj): RubyExpression = {
    val body = obj.visitArray(ParserKeys.Arguments) match {
      case head :: Nil => head
      case xs          => xs.head
    }

    obj(ParserKeys.Name).str match {
      case "public_class_method" =>
        PublicMethodModifier(body)(obj.toTextSpan)
      case "private_class_method" =>
        PrivateMethodModifier(body)(obj.toTextSpan)
      case modifierName =>
        logger.warn(s"Unknown modifier type $modifierName")
        defaultResult(Option(obj.toTextSpan))
    }
  }

  private def visitMethodDefinition(obj: Obj): RubyExpression = {
    val name       = obj(ParserKeys.Name).str
    val parameters = visitMethodParameters(obj(ParserKeys.Arguments).asInstanceOf[ujson.Obj])
    val body = obj
      .visitOption(ParserKeys.Body)
      .map {
        case x: StatementList => x
        case x                => StatementList(List(x))(x.span)
      }
      .getOrElse(StatementList(Nil)(obj.toTextSpan.spanStart("<empty>")))
    MethodDeclaration(name, parameters, body)(obj.toTextSpan)
  }

  private def visitModuleDefinition(obj: Obj): RubyExpression = {
    val (name, namespaceParts) = visit(obj(ParserKeys.Name)) match {
      case memberAccess: MemberAccess =>
        val memberIdentifier = SimpleIdentifier()(memberAccess.span.spanStart(memberAccess.memberName))
        (memberIdentifier, Option(getParts(memberAccess).dropRight(1)))
      case identifier => (identifier, None)
    }
    val (body, fields) = createClassBodyAndFields(obj)
    val bodyMemberCall = createBodyMemberCall(name.text, obj.toTextSpan)
    ModuleDeclaration(
      name = name,
      body = body,
      fields = fields,
      bodyMemberCall = Option(bodyMemberCall),
      namespaceParts = namespaceParts
    )(obj.toTextSpan)
  }

  private def visitMultipleAssignment(obj: Obj): RubyExpression = {
    val lhs = visit(obj(ParserKeys.Lhs)) match {
      case _ @ArrayLiteral(elements) => elements
      case expr                      => expr :: Nil
    }
    val rhs = visit(obj(ParserKeys.Rhs)) match {
      case _ @ArrayLiteral(elements) => elements
      case expr                      => expr :: Nil
    }
    lowerMultipleAssignment(
      obj,
      lhs,
      rhs,
      () => defaultResult(),
      () => StaticLiteral(Defines.prefixAsCoreType(Defines.NilClass))(obj.toTextSpan)
    )
  }

  private def visitMultipleLeftHandSide(obj: Obj): RubyExpression = {
    val arr = visitArray(obj).asInstanceOf[ArrayLiteral]
    arr.copy(elements = arr.elements.map {
      case param: MandatoryParameter => param.toSimpleIdentifier
      case expr                      => expr
    })(arr.span)
  }

  private def visitNext(obj: Obj): RubyExpression = NextExpression()(obj.toTextSpan)

  private def visitNil(obj: Obj): RubyExpression =
    StaticLiteral(Defines.prefixAsCoreType(Defines.NilClass))(obj.toTextSpan)

  private def visitNthRef(obj: Obj): RubyExpression = {
    // We represent $1 as $[1] in order to track these arbitrary numeric accesses in a way the data-flow engine
    // understands
    val span              = obj.toTextSpan
    val name              = obj(ParserKeys.Value).num.toInt
    val selfBase          = SelfIdentifier()(span.spanStart("self"))
    val amperMemberAccess = MemberAccess(selfBase, ".", "$")(span)
    val indexPos = StaticLiteral(Defines.prefixAsCoreType(Defines.Integer))(obj.toTextSpan.spanStart(name.toString))
    IndexAccess(amperMemberAccess, indexPos :: Nil)(obj.toTextSpan.spanStart(s"$$[$name]"))
  }

  private def visitObjectInstantiation(obj: Obj): RubyExpression = {
    // The receiver is the target with the JSON parser
    val receiver  = visit(obj(ParserKeys.Receiver))
    val arguments = obj.visitArray(ParserKeys.Arguments)
    SimpleObjectInstantiation(receiver, arguments)(obj.toTextSpan)
  }

  private def visitOperatorAssign(obj: Obj): RubyExpression = {
    val lhs = visit(obj(ParserKeys.Lhs)) match {
      case param: MandatoryParameter => param.toSimpleIdentifier
      case x                         => x
    }
    val op  = s"${obj(ParserKeys.Op).str}="
    val rhs = visit(obj(ParserKeys.Rhs))
    SingleAssignment(lhs, op, rhs)(obj.toTextSpan)
  }

  private def visitOptionalArgument(obj: Obj): RubyExpression = {
    val name    = obj(ParserKeys.Key).str
    val default = visit(obj(ParserKeys.Value))
    OptionalParameter(name, default)(obj.toTextSpan)
  }

  private def visitOr(obj: Obj): RubyExpression = {
    val op  = "||"
    val lhs = visit(obj(ParserKeys.Lhs))
    val rhs = visit(obj(ParserKeys.Rhs))
    BinaryExpression(lhs, op, rhs)(obj.toTextSpan)
  }

  private def visitOrAssign(obj: Obj): RubyExpression = {
    val lhs = visit(obj(ParserKeys.Lhs)) match {
      case param: MandatoryParameter => param.toSimpleIdentifier
      case x                         => x
    }
    val rhs = visit(obj(ParserKeys.Rhs))
    OperatorAssignment(lhs, "||=", rhs)(obj.toTextSpan)
  }

  private def visitPair(obj: Obj): RubyExpression = {
    val key   = visit(obj(ParserKeys.Key))
    val value = visit(obj(ParserKeys.Value))
    Association(key, value)(obj.toTextSpan)
  }

  private def visitMethodParameters(paramsNode: Obj): List[RubyExpression] = {
    AstType.fromString(paramsNode(ParserKeys.Type).str) match {
      case Some(AstType.Args)        => paramsNode.visitArray(ParserKeys.Children)
      case Some(AstType.ForwardArgs) => visit(paramsNode) :: Nil
      case Some(x) =>
        logger.warn(s"Not explicitly handled parameter type '$x', no special handling applied")
        visit(paramsNode) :: Nil
      case _ =>
        logger.error(s"Unknown JSON type used as method parameter ${paramsNode(ParserKeys.Type).str}")
        defaultResult(Option(paramsNode.toTextSpan)) :: Nil
    }
  }

  private def visitPostExpression(obj: Obj): RubyExpression = defaultResult(Option(obj.toTextSpan))

  private def visitPreExpression(obj: Obj): RubyExpression = defaultResult(Option(obj.toTextSpan))

  private def visitProcArgument(obj: Obj): RubyExpression = defaultResult(Option(obj.toTextSpan))

  private def visitRaise(obj: Obj): RubyExpression = {
    val callName = obj(ParserKeys.Name).str
    val target   = SimpleIdentifier()(obj.toTextSpan.spanStart(callName))

    obj.visitArray(ParserKeys.Arguments) match {
      case Nil => RaiseCall(target, List.empty)(obj.toTextSpan)
      case (argument: StaticLiteral) :: Nil =>
        val simpleErrorId =
          SimpleIdentifier(Option(Defines.prefixAsCoreType("StandardError")))(argument.span.spanStart("StandardError"))
        val implicitSimpleErrInst = SimpleObjectInstantiation(simpleErrorId, argument :: Nil)(
          argument.span.spanStart(s"StandardError.new(${argument.text})")
        )
        RaiseCall(target, implicitSimpleErrInst :: Nil)(obj.toTextSpan)
      case argument :: Nil =>
        RaiseCall(target, List(argument))(obj.toTextSpan)
      case arguments =>
        RaiseCall(target, arguments)(obj.toTextSpan)
    }

  }

  private def visitRational(obj: Obj): RubyExpression =
    StaticLiteral(Defines.prefixAsCoreType(Defines.Rational))(obj.toTextSpan)

  private def visitRedo(obj: Obj): RubyExpression = {
    val callTarget = SimpleIdentifier()(obj.toTextSpan.spanStart("redo"))
    SimpleCall(callTarget, Nil)(obj.toTextSpan)
  }

  private def visitRetry(obj: Obj): RubyExpression = {
    val callTarget = SimpleIdentifier()(obj.toTextSpan.spanStart("retry"))
    SimpleCall(callTarget, Nil)(obj.toTextSpan)
  }

  private def visitReturn(obj: Obj): RubyExpression = {
    if (obj.contains(ParserKeys.Values)) {
      val returnExpressions = obj.visitArray(ParserKeys.Values)
      ReturnExpression(returnExpressions)(obj.toTextSpan)
    } else if (obj.contains(ParserKeys.Value)) {
      ReturnExpression(visit(obj(ParserKeys.Value)) :: Nil)(obj.toTextSpan)
    } else {
      ReturnExpression(List.empty)(obj.toTextSpan)
    }
  }

  private def visitRegexExpression(obj: Obj): RubyExpression = {
    obj.visitOption(ParserKeys.Value) match {
      case Some(_ @StatementList(stmts)) =>
        DynamicLiteral(Defines.prefixAsCoreType(Defines.Regexp), stmts)(obj.toTextSpan)
      case _ => StaticLiteral(Defines.prefixAsCoreType(Defines.Regexp))(obj.toTextSpan)
    }
  }

  private def visitRegexOption(obj: Obj): RubyExpression = defaultResult(Option(obj.toTextSpan))

  private def visitResBody(obj: Obj): RubyExpression = {
    val exceptionClassList = obj.visitOption(ParserKeys.ExecList)
    val variables          = obj.visitOption(ParserKeys.ExecVar)
    val body = obj.visitOption(ParserKeys.Body) match {
      case Some(stmt: StatementList) => stmt
      case Some(expr)                => StatementList(expr :: Nil)(expr.span)
      case None                      => StatementList(Nil)(obj.toTextSpan)
    }
    RescueClause(exceptionClassList, variables, body)(obj.toTextSpan)
  }

  private def visitRestArg(obj: Obj): RubyExpression = {
    obj(ParserKeys.Value) match {
      case ujson.Null      => ArrayParameter("*")(obj.toTextSpan)
      case ujson.Str(name) => ArrayParameter(name)(obj.toTextSpan)
      case x =>
        logger.warn(s"Unhandled `restarg` JSON type '$x'")
        defaultResult(Option(obj.toTextSpan))
    }
  }

  private def visitRescueStatement(obj: Obj): RubyExpression = {
    val stmt          = visit(obj(ParserKeys.Statement))
    val rescueClauses = obj.visitArray(ParserKeys.Bodies).asInstanceOf[List[RescueClause]]
    val elseClause = obj.visitOption(ParserKeys.ElseClause) match {
      case Some(body) => Option(ElseClause(body)(body.span))
      case None       => Option.empty
    }

    RescueExpression(stmt, rescueClauses, elseClause, Option.empty)(obj.toTextSpan)
  }

  private def visitRequireLike(obj: Obj): RubyExpression = {
    val callName = obj(ParserKeys.Name).str
    val target   = SimpleIdentifier()(obj.toTextSpan.spanStart(callName))
    val argument = obj
      .visitArray(ParserKeys.Arguments)
      .headOption
      .getOrElse(StaticLiteral(Defines.prefixAsCoreType(Defines.NilClass))(obj.toTextSpan.spanStart("nil")))
    val isRelative = callName == "require_relative" || callName == "require_all"
    val isWildcard = callName == "require_all"
    RequireCall(target, argument, isRelative, isWildcard)(obj.toTextSpan)
  }

  private def visitScopedConstant(obj: Obj): RubyExpression = {
    val identifier = obj(ParserKeys.Name).str
    if (obj.contains(ParserKeys.Base)) {
      val target = visit(obj(ParserKeys.Base))
      val op     = if obj.toTextSpan.text.contains("::") then "::" else "."
      MemberAccess(target, op, identifier)(obj.toTextSpan)
    } else {
      SimpleIdentifier()(obj.toTextSpan)
    }
  }

  private def visitSelf(obj: Obj): RubyExpression = SelfIdentifier()(obj.toTextSpan)

  private def visitBinaryExpression(obj: Obj): RubyExpression = {
    val op  = obj(ParserKeys.Name).str
    val lhs = visit(obj(ParserKeys.Receiver))
    obj.visitArray(ParserKeys.Arguments).headOption match {
      case Some(rhs) => BinaryExpression(lhs, op, rhs)(obj.toTextSpan)
      case None      => MemberCall(lhs, ".", op, Nil)(obj.toTextSpan)
    }
  }

  private def visitSend(obj: Obj, isConditional: Boolean = false): RubyExpression = {
    val callName    = obj(ParserKeys.Name).str
    val hasReceiver = obj.contains(ParserKeys.Receiver)
    callName match {
      case "new"                                                                => visitObjectInstantiation(obj)
      case "Array" | "Hash"                                                     => visitCollectionAliasSend(obj)
      case "[]"                                                                 => visitIndexAccessAsSend(obj)
      case "[]="                                                                => visitBracketAssignmentAsSend(obj)
      case "raise"                                                              => visitRaise(obj)
      case "include"                                                            => visitInclude(obj)
      case "alias_method"                                                       => visitAlias(obj)
      case "attr_reader" | "attr_writer" | "attr_accessor"                      => visitFieldDeclaration(obj)
      case "private" | "public" | "protected"                                   => visitAccessModifier(obj)
      case "private_class_method" | "public_class_method"                       => visitMethodAccessModifier(obj)
      case requireLike if ImportCallNames.contains(requireLike) && !hasReceiver => visitRequireLike(obj)
      case _ if BinaryOperators.isBinaryOperatorName(callName)                  => visitBinaryExpression(obj)
      case _ if UnaryOperators.isUnaryOperatorName(callName) =>
        UnaryExpression(callName, visit(obj(ParserKeys.Receiver)))(obj.toTextSpan)
      case _ if RubyOperators.regexMethods.contains(callName) =>
        val newProps = obj.value
        newProps.put(ParserKeys.Name, s"$callName${Defines.NeedsRegexLowering}")
        visitSend(obj.copy(value = newProps))
      case s"$name=" if hasReceiver => visitFieldAssignmentSend(obj, name)
      case _ =>
        val target      = SimpleIdentifier()(obj.toTextSpan.spanStart(callName))
        val argumentArr = obj.visitArray(ParserKeys.Arguments)
        val arguments = argumentArr.flatMap {
          case hashLiteral: HashLiteral   => hashLiteral.elements // a hash is likely named arguments
          case assocList: AssociationList => assocList.elements   // same as above
          case x                          => x :: Nil
        }
        val objSpan         = obj.toTextSpan
        val hasArguments    = arguments.nonEmpty
        val usesParenthesis = objSpan.text.endsWith(")")
        if (obj.contains(ParserKeys.Receiver)) {
          val base         = visit(obj(ParserKeys.Receiver))
          val isMemberCall = usesParenthesis || callName == "<<" || hasArguments
          val op = {
            val dot = if objSpan.text.stripPrefix(base.text).startsWith("::") then "::" else "."
            if isConditional then s"&$dot" else dot
          }
          if isMemberCall then MemberCall(base, op, callName, arguments)(obj.toTextSpan)
          else MemberAccess(base, op, callName)(obj.toTextSpan)
        } else if (hasArguments || usesParenthesis) {
          SimpleCall(target, arguments)(obj.toTextSpan)
        } else {
          // The following allows the AstCreator to approximate when an identifier could be a call or not - puts less
          //  strain on data-flow tracking for externally inherited accessor calls such as `params` in RubyOnRails
          SimpleIdentifier()(obj.toTextSpan.spanStart(callName))
        }
    }
  }

  private def visitShadowArg(obj: Obj): RubyExpression = defaultResult(Option(obj.toTextSpan))

  private def visitSingletonMethodDefinition(obj: Obj): RubyExpression = {
    val base       = visit(obj(ParserKeys.Base))
    val name       = obj(ParserKeys.Name).str
    val parameters = visitMethodParameters(obj(ParserKeys.Arguments).asInstanceOf[ujson.Obj])
    val body =
      obj.visitOption(ParserKeys.Body).getOrElse(StatementList(Nil)(obj.toTextSpan.spanStart("<empty>"))) match {
        case stmtList: StatementList => stmtList
        case expr                    => StatementList(expr :: Nil)(expr.span)
      }
    SingletonMethodDeclaration(base, name, parameters, body)(obj.toTextSpan)
  }

  private def visitSingletonClassDefinition(obj: Obj): RubyExpression = {
    val name      = visit(obj(ParserKeys.Name))
    val baseClass = obj.visitOption(ParserKeys.SuperClass)
    val body      = obj.visitOption(ParserKeys.Body).getOrElse(StatementList(Nil)(obj.toTextSpan.spanStart("<empty>")))

    obj.visitOption(ParserKeys.Def) match {
      case Some(body) =>
        name match {
          case _: SelfIdentifier =>
            val bodyList = body match {
              case stmtList: StatementList => stmtList
              case expr                    => StatementList(expr :: Nil)(expr.span)
            }

            val base = baseClass match {
              case Some(baseClass) => baseClass
              case None            => SelfIdentifier()(obj.toTextSpan.spanStart("self"))
            }

            SingletonClassDeclaration(freshClassName(obj.toTextSpan), Some(base), bodyList)(obj.toTextSpan)
          case _ =>
            def mapDefBody(defBody: RubyExpression): RubyExpression = defBody match {
              case method @ MethodDeclaration(methodName, parameters, body) =>
                val memberAccess =
                  MemberAccess(name, ".", methodName)(method.span.spanStart(s"${name.span.text}.${methodName}"))
                val singletonBlockMethod =
                  SingletonObjectMethodDeclaration(methodName, parameters, body, name)(method.span)
                SingleAssignment(memberAccess, "=", singletonBlockMethod)(
                  method.span.spanStart(s"${memberAccess.span.text} = ${method.span.text}")
                )
              case expr => expr
            }

            val stmts = body match {
              case _ @StatementList(stmts) => stmts.map(mapDefBody)
              case expr                    => mapDefBody(expr) :: Nil
            }
            SingletonStatementList(stmts)(obj.toTextSpan)
        }

      case None =>
        val anonName = freshClassName(obj.toTextSpan)
        SingletonClassDeclaration(name = anonName, baseClass = baseClass, body = body)(obj.toTextSpan)
    }
  }

  private def visitSingleAssignment(obj: Obj): RubyExpression = {
    val lhsSpan = obj.toTextSpan.spanStart(obj(ParserKeys.Lhs).str)
    val lhs = obj(ParserKeys.Lhs).str match {
      case s"@@$_" => ClassFieldIdentifier()(lhsSpan)
      case s"@$_"  => InstanceFieldIdentifier()(lhsSpan)
      case _       => SimpleIdentifier()(lhsSpan)
    }
    obj.visitOption(ParserKeys.Rhs) match {
      case Some(rhs) =>
        SingleAssignment(lhs, "=", rhs)(obj.toTextSpan)
      case None =>
        if (AstType.fromString(obj(ParserKeys.Type).str) == AstType.LocalVariableAssign) {
          // `lvasgn` is used in exec_var for rescueExpr, which only has LHS
          MandatoryParameter(lhs.span.text)(lhs.span)
        } else {
          lhs
        }
    }
  }

  private def visitSplat(obj: Obj): RubyExpression = {
    obj.visitOption(ParserKeys.Value) match {
      case Some(x) => SplattingRubyNode(x)(obj.toTextSpan)
      case None =>
        val emptyStar = SimpleIdentifier()(obj.toTextSpan.spanStart("_"))
        SplattingRubyNode(emptyStar)(obj.toTextSpan)
    }
  }

  private def visitStaticString(obj: Obj): RubyExpression = {
    val typeFullName = Defines.prefixAsCoreType(Defines.String)
    val originalSpan = obj.toTextSpan
    val value        = obj(ParserKeys.Value).str
    // In general, we want the quotations, unless it is a HEREDOC string, then we'd prefer the value
    val span = if !originalSpan.text.contains(value) then originalSpan.spanStart(value) else originalSpan
    StaticLiteral(typeFullName)(span)
  }

  private def visitStaticSymbol(obj: Obj): RubyExpression = {
    val typeFullName = Defines.prefixAsCoreType(Defines.Symbol)
    val objTextSpan  = obj.toTextSpan

    if objTextSpan.text.startsWith(":") then StaticLiteral(typeFullName)(obj.toTextSpan)
    else StaticLiteral(typeFullName)(objTextSpan.spanStart(s":${objTextSpan.text}"))
  }

  private def visitSuper(obj: Obj): RubyExpression = {
    val name =
      SimpleIdentifier(Option(Defines.prefixAsKernelDefined(Defines.Super)))(obj.toTextSpan.spanStart(Defines.Super))
    val arguments = obj.visitArray(ParserKeys.Arguments)
    SimpleCall(name, arguments)(obj.toTextSpan)
  }

  private def visitSuperNoArgs(obj: Obj): RubyExpression = {
    val name =
      SimpleIdentifier(Option(Defines.prefixAsKernelDefined(Defines.Super)))(obj.toTextSpan.spanStart(Defines.Super))
    SimpleCall(name, Nil)(obj.toTextSpan)
  }

  private def visitTopLevelConstant(obj: Obj): RubyExpression = {
    if (obj.contains(ParserKeys.Name)) {
      val identifier = obj(ParserKeys.Name).str
      SimpleIdentifier()(obj.toTextSpan.spanStart(identifier))
    } else {
      SelfIdentifier()(obj.toTextSpan.spanStart("self"))
    }
  }

  private def visitTrue(obj: Obj): RubyExpression =
    StaticLiteral(Defines.prefixAsCoreType(Defines.TrueClass))(obj.toTextSpan)

  private def visitUnDefine(obj: Obj): RubyExpression = {
    defaultResult(Option(obj.toTextSpan))
  }

  private def visitUnlessExpression(obj: Obj): RubyExpression = {
    defaultResult(Option(obj.toTextSpan))
  }

  private def visitUnlessGuard(obj: Obj): RubyExpression = defaultResult(Option(obj.toTextSpan))

  private def visitUntilExpression(obj: Obj): RubyExpression = {
    val condition = visit(obj(ParserKeys.Condition))
    val body      = visit(obj(ParserKeys.Body))

    UntilExpression(condition, body)(obj.toTextSpan)
  }

  private def visitUntilPostExpression(obj: Obj): RubyExpression = {
    val condition = visit(obj(ParserKeys.Condition))
    val body      = visit(obj(ParserKeys.Body))

    DoWhileExpression(condition, body)(obj.toTextSpan)
  }

  private def visitWhenStatement(obj: Obj): RubyExpression = {
    val (matchCondition, matchSplatCondition) = obj.visitArray(ParserKeys.Conditions).partition {
      case x: SplattingRubyNode => false
      case x                    => true
    }

    val thenClause = visit(obj(ParserKeys.ThenBranch))

    WhenClause(matchCondition, matchSplatCondition.headOption, thenClause)(obj.toTextSpan)
  }

  private def visitWhileStatement(obj: Obj): RubyExpression = {
    val condition = visit(obj(ParserKeys.Condition)) match {
      case x: StatementList => x.statements.head
      case x                => x
    }

    val body = visit(obj(ParserKeys.Body))

    WhileExpression(condition, body)(obj.toTextSpan)
  }

  private def visitYield(obj: Obj): RubyExpression = {
    val arguments = obj.visitArray(ParserKeys.Arguments)
    YieldExpr(arguments)(obj.toTextSpan)
  }

}
