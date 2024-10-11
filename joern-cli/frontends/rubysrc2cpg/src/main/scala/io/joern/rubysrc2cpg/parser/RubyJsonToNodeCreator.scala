package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.astcreation.RubyIntermediateAst.{RubyExpression, *}
import io.joern.rubysrc2cpg.parser.RubyJsonHelpers.*
import io.joern.rubysrc2cpg.passes.Defines
import io.joern.rubysrc2cpg.passes.Defines.getBuiltInType
import io.joern.rubysrc2cpg.passes.GlobalTypes.builtinPrefix
import io.joern.rubysrc2cpg.utils.FreshNameGenerator
import io.joern.x2cpg.frontendspecific.rubysrc2cpg.ImportsPass
import io.joern.x2cpg.frontendspecific.rubysrc2cpg.ImportsPass.ImportCallNames
import org.slf4j.LoggerFactory
import ujson.{Obj, Value}

class RubyJsonToNodeCreator(
  variableNameGen: FreshNameGenerator[String] = FreshNameGenerator(id => s"<tmp-$id>"),
  procParamGen: FreshNameGenerator[Left[String, Nothing]] = FreshNameGenerator(id => Left(s"<proc-param-$id>"))
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
      case ujson.Null     => defaultResult()
      case ujson.Str(x)   => StaticLiteral(getBuiltInType(Defines.String))(defaultTextSpan(x))
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
        case AstType.BlockPass                    => visitBlockPass(obj)
        case AstType.ClassDefinition              => visitClassDefinition(obj)
        case AstType.ClassVariable                => visitClassVariable(obj)
        case AstType.ClassVariableAssign          => visitSingleAssignment(obj)
        case AstType.ConstVariableAssign          => visitSingleAssignment(obj)
        case AstType.ConditionalSend              => visitConditionalSend(obj)
        case AstType.Defined                      => visitDefined(obj)
        case AstType.DynamicString                => visitDynamicString(obj)
        case AstType.DynamicSymbol                => visitDynamicSymbol(obj)
        case AstType.ExclusiveFlipFlop            => visitExclusiveFlipFlop(obj)
        case AstType.ExclusiveRange               => visitExclusiveRange(obj)
        case AstType.ExecutableString             => visitExecutableString(obj)
        case AstType.False                        => visitFalse(obj)
        case AstType.FindPattern                  => visitFindPattern(obj)
        case AstType.Float                        => visitFloat(obj)
        case AstType.ForwardArg                   => visitForwardArg(obj)
        case AstType.ForwardArgs                  => visitForwardArgs(obj)
        case AstType.ForwardedArgs                => visitForwardedArgs(obj)
        case AstType.GlobalVariable               => visitGlobalVariable(obj)
        case AstType.GlobalVariableAssign         => visitGlobalVariableAssign(obj)
        case AstType.Hash                         => visitHash(obj)
        case AstType.HashPattern                  => visitHashPattern(obj)
        case AstType.Identifier                   => visitIdentifier(obj)
        case AstType.InclusiveFlipFlop            => visitInclusiveFlipFlop(obj)
        case AstType.InclusiveRange               => visitInclusiveRange(obj)
        case AstType.Int                          => visitInt(obj)
        case AstType.InstanceVariable             => visitInstanceVariable(obj)
        case AstType.InstanceVariableAssign       => visitSingleAssignment(obj)
        case AstType.KwArg                        => visitKwArg(obj)
        case AstType.KwNilArg                     => visitKwNilArg(obj)
        case AstType.KwOptArg                     => visitKwOptArg(obj)
        case AstType.KwRestArg                    => visitKwRestArg(obj)
        case AstType.KwSplat                      => visitKwSplat(obj)
        case AstType.LocalVariable                => visitLocalVariable(obj)
        case AstType.LocalVariableAssign          => visitSingleAssignment(obj)
        case AstType.MatchPattern                 => visitMatchPattern(obj)
        case AstType.MatchPatternP                => visitMatchPatternP(obj)
        case AstType.MatchVariable                => visitMatchVariable(obj)
        case AstType.MatchWithLocalVariableAssign => visitMatchWithLocalVariableAssign(obj)
        case AstType.MethodDefinition             => visitMethodDefinition(obj)
        case AstType.MultipleLeftHandSide         => visitMultipleLeftHandSide(obj)
        case AstType.Nil                          => visitNil(obj)
        case AstType.NthRef                       => visitNthRef(obj)
        case AstType.OperatorAssign               => visitOperatorAssign(obj)
        case AstType.Or                           => visitOr(obj)
        case AstType.OrAssign                     => visitOrAssign(obj)
        case AstType.Pair                         => visitPair(obj)
        case AstType.ProcArgument                 => visitProcArgument(obj)
        case AstType.Rational                     => visitRational(obj)
        case AstType.RestArg                      => visitRestArg(obj)
        case AstType.ScopedConstant               => visitScopedConstant(obj)
        case AstType.Self                         => visitSelf(obj)
        case AstType.Send                         => visitSend(obj)
        case AstType.ShadowArg                    => visitShadowArg(obj)
        case AstType.Splat                        => visitSplat(obj)
        case AstType.StaticString                 => visitStaticString(obj)
        case AstType.StaticSymbol                 => visitStaticSymbol(obj)
        case AstType.Super                        => visitSuper(obj)
        case AstType.TopLevelConstant             => visitTopLevelConstant(obj)
        case AstType.True                         => visitTrue(obj)
        case AstType.UnDefine                     => visitUnDefine(obj)
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

  private def visitAnd(obj: Obj): RubyExpression = {
    val op  = "&&"
    val lhs = visit(obj(ParserKeys.Lhs))
    val rhs = visit(obj(ParserKeys.Rhs))
    BinaryExpression(lhs, op, rhs)(obj.toTextSpan)
  }

  private def visitAndAssign(obj: Obj): RubyExpression = defaultResult(Option(obj.toTextSpan))

  private def visitArg(obj: Obj): RubyExpression = MandatoryParameter(obj(ParserKeys.Value).str)(obj.toTextSpan)

  private def visitArgs(obj: Obj): RubyExpression = defaultResult(Option(obj.toTextSpan))

  private def visitArray(obj: Obj): RubyExpression = ArrayLiteral(obj.visitArray(ParserKeys.Children))(obj.toTextSpan)

  private def visitArrayPattern(obj: Obj): RubyExpression = defaultResult(Option(obj.toTextSpan))

  private def visitArrayPatternWithTail(obj: Obj): RubyExpression = defaultResult(Option(obj.toTextSpan))

  private def visitBackRef(obj: Obj): RubyExpression = defaultResult(Option(obj.toTextSpan))

  private def visitBegin(obj: Obj): RubyExpression = StatementList(obj.visitArray(ParserKeys.Body))(obj.toTextSpan)

  private def visitBlock(obj: Obj): RubyExpression = {
    val parameters = obj(ParserKeys.Arguments).asInstanceOf[ujson.Obj].visitArray(ParserKeys.Children)
    val body       = visit(obj(ParserKeys.Body))
    val block      = Block(parameters, body)(obj.toTextSpan)
    visit(obj(ParserKeys.CallName)) match {
      case simpleCall: RubyCall => simpleCall.withBlock(block)
      case x =>
        logger.warn(s"Unexpected call type used for block ${x.getClass}, ignoring block")
        x
    }
  }

  private def visitBlockPass(obj: Obj): RubyExpression = defaultResult(Option(obj.toTextSpan))

  private def visitClassDefinition(obj: Obj): RubyExpression = {
    val name      = visit(obj(ParserKeys.Name))
    val baseClass = if obj.contains(ParserKeys.SuperClass) then Option(visit(obj(ParserKeys.SuperClass))) else None
    val body      = visit(obj(ParserKeys.Body))
    // TODO: Handle rest of the fields
    ClassDeclaration(name, baseClass, body, fields = Nil, bodyMemberCall = None, namespaceParts = None)(obj.toTextSpan)
  }

  private def visitClassVariable(obj: Obj): RubyExpression = ClassFieldIdentifier()(obj.toTextSpan)

  private def visitConditionalSend(obj: Obj): RubyExpression = defaultResult(Option(obj.toTextSpan))

  private def visitDefined(obj: Obj): RubyExpression = {
    val name      = SimpleIdentifier(Option(getBuiltInType(Defines.Defined)))(obj.toTextSpan.spanStart(Defines.Defined))
    val arguments = obj.visitArray(ParserKeys.Arguments)
    SimpleCall(name, arguments)(obj.toTextSpan)
  }

  private def visitDynamicString(obj: Obj): RubyExpression = {
    val typeFullName = getBuiltInType(Defines.String)
    val expressions  = obj.visitArray(ParserKeys.Children)
    DynamicLiteral(typeFullName, expressions)(obj.toTextSpan)
  }

  private def visitDynamicSymbol(obj: Obj): RubyExpression = {
    val typeFullName = getBuiltInType(Defines.Symbol)
    val expressions  = obj.visitArray(ParserKeys.Children)
    DynamicLiteral(typeFullName, expressions)(obj.toTextSpan)
  }

  private def visitExclusiveFlipFlop(obj: Obj): RubyExpression = defaultResult(Option(obj.toTextSpan))

  private def visitExclusiveRange(obj: Obj): RubyExpression = defaultResult(Option(obj.toTextSpan))

  private def visitExecutableString(obj: Obj): RubyExpression = {
    val callName =
      SimpleIdentifier(Option(getBuiltInType(Defines.Backticks)))(obj.toTextSpan.spanStart(Defines.Backticks))
    val arguments = obj.visitArray(ParserKeys.Arguments)
    SimpleCall(callName, arguments)(obj.toTextSpan)
  }

  private def visitFalse(obj: Obj): RubyExpression = StaticLiteral(getBuiltInType(Defines.FalseClass))(obj.toTextSpan)

  private def visitFindPattern(obj: Obj): RubyExpression = defaultResult(Option(obj.toTextSpan))

  private def visitFloat(obj: Obj): RubyExpression = StaticLiteral(getBuiltInType(Defines.Float))(obj.toTextSpan)

  private def visitForwardArg(obj: Obj): RubyExpression = defaultResult(Option(obj.toTextSpan))

  private def visitForwardArgs(obj: Obj): RubyExpression = defaultResult(Option(obj.toTextSpan))

  private def visitForwardedArgs(obj: Obj): RubyExpression = defaultResult(Option(obj.toTextSpan))

  private def visitGlobalVariable(obj: Obj): RubyExpression = defaultResult(Option(obj.toTextSpan))

  private def visitGlobalVariableAssign(obj: Obj): RubyExpression = defaultResult(Option(obj.toTextSpan))

  private def visitHash(obj: Obj): RubyExpression = HashLiteral(obj.visitArray(ParserKeys.Children))(obj.toTextSpan)

  private def visitHashPattern(obj: Obj): RubyExpression = defaultResult(Option(obj.toTextSpan))

  private def visitIdentifier(obj: Obj): RubyExpression = SimpleIdentifier()(obj.toTextSpan)

  private def visitInclude(obj: Obj): RubyExpression = {
    val callName = obj(ParserKeys.Name).str
    val target   = SimpleIdentifier()(obj.toTextSpan.spanStart(callName))
    val argument = obj.visitArray(ParserKeys.Arguments).head

    IncludeCall(target, argument)(obj.toTextSpan)
  }

  private def visitInclusiveFlipFlop(obj: Obj): RubyExpression = defaultResult(Option(obj.toTextSpan))

  private def visitInclusiveRange(obj: Obj): RubyExpression = defaultResult(Option(obj.toTextSpan))

  private def visitInt(obj: Obj): RubyExpression = {
    val typeFullName = getBuiltInType(Defines.Integer)
    StaticLiteral(typeFullName)(obj.toTextSpan)
  }

  private def visitInstanceVariable(obj: Obj): RubyExpression = InstanceFieldIdentifier()(obj.toTextSpan)

  private def visitKwArg(obj: Obj): RubyExpression = defaultResult(Option(obj.toTextSpan))

  private def visitKwNilArg(obj: Obj): RubyExpression = defaultResult(Option(obj.toTextSpan))

  private def visitKwOptArg(obj: Obj): RubyExpression = defaultResult(Option(obj.toTextSpan))

  private def visitKwRestArg(obj: Obj): RubyExpression = defaultResult(Option(obj.toTextSpan))

  private def visitKwSplat(obj: Obj): RubyExpression = defaultResult(Option(obj.toTextSpan))

  private def visitLocalVariable(obj: Obj): RubyExpression = SimpleIdentifier()(obj.toTextSpan)

  private def visitMatchPattern(obj: Obj): RubyExpression = defaultResult(Option(obj.toTextSpan))

  private def visitMatchPatternP(obj: Obj): RubyExpression = defaultResult(Option(obj.toTextSpan))

  private def visitMatchVariable(obj: Obj): RubyExpression = defaultResult(Option(obj.toTextSpan))

  private def visitMatchWithLocalVariableAssign(obj: Obj): RubyExpression = defaultResult(Option(obj.toTextSpan))

  private def visitMethodDefinition(obj: Obj): RubyExpression = {
    val name       = obj(ParserKeys.Name).str
    val parameters = obj(ParserKeys.Arguments).asInstanceOf[ujson.Obj].visitArray(ParserKeys.Children)
    val body = if (obj.contains(ParserKeys.Body)) visit(obj(ParserKeys.Body)) else StatementList(Nil)(obj.toTextSpan)
    MethodDeclaration(name, parameters, body)(obj.toTextSpan)
  }

  private def visitMultipleLeftHandSide(obj: Obj): RubyExpression = defaultResult(Option(obj.toTextSpan))

  private def visitNil(obj: Obj): RubyExpression = StaticLiteral(getBuiltInType(Defines.NilClass))(obj.toTextSpan)

  private def visitNthRef(obj: Obj): RubyExpression = defaultResult(Option(obj.toTextSpan))

  private def visitObjectInstantiation(obj: Obj): RubyExpression = {
    val callName  = Defines.New
    val receiver  = visit(obj(ParserKeys.Receiver))
    val target    = MemberAccess(receiver, ".", callName)(obj.toTextSpan)
    val arguments = obj.visitArray(ParserKeys.Arguments)

    SimpleObjectInstantiation(target, arguments)(obj.toTextSpan)
  }

  private def visitOperatorAssign(obj: Obj): RubyExpression = {
    /*
      base_map[:lhs] = children[0]
      base_map[:op] = children[1]
      base_map[:rhs] = children[2]
     */
    // TODO: Something is wrong here, investigate `ruby_ast_gen`
    defaultResult(Option(obj.toTextSpan))
  }

  private def visitOr(obj: Obj): RubyExpression = {
    val op  = "||"
    val lhs = visit(obj(ParserKeys.Lhs))
    val rhs = visit(obj(ParserKeys.Rhs))
    BinaryExpression(lhs, op, rhs)(obj.toTextSpan)
  }

  private def visitOrAssign(obj: Obj): RubyExpression = defaultResult(Option(obj.toTextSpan))

  private def visitPair(obj: Obj): RubyExpression = defaultResult(Option(obj.toTextSpan))

  private def visitProcArgument(obj: Obj): RubyExpression = defaultResult(Option(obj.toTextSpan))

  private def visitRaise(obj: Obj): RubyExpression = {
    val callName = obj(ParserKeys.Name).str
    val target   = SimpleIdentifier()(obj.toTextSpan.spanStart(callName))
    val argument = obj.visitArray(ParserKeys.Arguments).head

    val simpleErrorId =
      SimpleIdentifier(Option(s"$builtinPrefix.StandardError"))(argument.span.spanStart("StandardError"))
    val implicitSimpleErrInst = SimpleObjectInstantiation(simpleErrorId, argument :: Nil)(
      argument.span.spanStart(s"StandardError.new(${argument.text})")
    )
    RaiseCall(target, implicitSimpleErrInst :: Nil)(obj.toTextSpan)
  }

  private def visitRational(obj: Obj): RubyExpression = StaticLiteral(getBuiltInType(Defines.Rational))(obj.toTextSpan)

  private def visitRestArg(obj: Obj): RubyExpression = defaultResult(Option(obj.toTextSpan))

  private def visitRequireLike(obj: Obj): RubyExpression = defaultResult(Option(obj.toTextSpan))

  private def visitScopedConstant(obj: Obj): RubyExpression = {
    val identifier = obj(ParserKeys.Name).str
    if (obj.contains(ParserKeys.Base)) {
      val target = visit(obj(ParserKeys.Base))
      MemberAccess(target, ".", identifier)(obj.toTextSpan)
    } else {
      SimpleIdentifier()(obj.toTextSpan)
    }
  }

  private def visitSelf(obj: Obj): RubyExpression = SelfIdentifier()(obj.toTextSpan)

  private def visitSend(obj: Obj): RubyExpression = {
    val callName = obj(ParserKeys.Name).str
    callName match {
      case "new"                                                => visitObjectInstantiation(obj)
      case "raise"                                              => visitRaise(obj)
      case "include"                                            => visitInclude(obj)
      case requireLike if ImportCallNames.contains(requireLike) => visitRequireLike(obj)
      case _ =>
        val target    = SimpleIdentifier()(obj.toTextSpan.spanStart(callName))
        val arguments = obj.visitArray(ParserKeys.Arguments)
        if (obj.contains(ParserKeys.Receiver)) {
          val base = visit(obj(ParserKeys.Receiver))
          MemberCall(base, ".", callName, arguments)(obj.toTextSpan)
        } else {
          SimpleCall(target, arguments)(obj.toTextSpan)
        }
    }
  }

  private def visitShadowArg(obj: Obj): RubyExpression = defaultResult(Option(obj.toTextSpan))

  private def visitSingleAssignment(obj: Obj): RubyExpression = {
    val lhs = visit(obj(ParserKeys.Lhs))
    val rhs = visit(obj(ParserKeys.Rhs))
    SingleAssignment(lhs, "=", rhs)(obj.toTextSpan)
  }

  private def visitSplat(obj: Obj): RubyExpression = SplattingRubyNode(visit(obj(ParserKeys.Value)))(obj.toTextSpan)

  private def visitStaticString(obj: Obj): RubyExpression = {
    val typeFullName = getBuiltInType(Defines.String)
    StaticLiteral(typeFullName)(obj.toTextSpan)
  }

  private def visitStaticSymbol(obj: Obj): RubyExpression = {
    val typeFullName = getBuiltInType(Defines.Symbol)
    StaticLiteral(typeFullName)(obj.toTextSpan)
  }

  private def visitSuper(obj: Obj): RubyExpression = {
    val name      = SimpleIdentifier(Option(getBuiltInType(Defines.Super)))(obj.toTextSpan.spanStart(Defines.Super))
    val arguments = obj.visitArray(ParserKeys.Arguments)
    SimpleCall(name, arguments)(obj.toTextSpan)
  }

  private def visitTopLevelConstant(obj: Obj): RubyExpression = {
    val identifier = obj(ParserKeys.Name).str
    SimpleIdentifier()(obj.toTextSpan.spanStart(identifier))
  }

  private def visitTrue(obj: Obj): RubyExpression = StaticLiteral(getBuiltInType(Defines.TrueClass))(obj.toTextSpan)

  private def visitUnDefine(obj: Obj): RubyExpression = defaultResult(Option(obj.toTextSpan))

  private def visitYield(obj: Obj): RubyExpression = {
    val arguments = obj.visitArray(ParserKeys.Arguments)
    YieldExpr(arguments)(obj.toTextSpan)
  }

}
