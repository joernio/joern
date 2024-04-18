package io.joern.rubysrc2cpg.astcreation

import io.joern.rubysrc2cpg.astcreation.RubyIntermediateAst.{Unknown, Block as RubyBlock, *}
import io.joern.rubysrc2cpg.datastructures.BlockScope
import io.joern.rubysrc2cpg.passes.Defines
import io.joern.rubysrc2cpg.passes.Defines.{RubyOperators, getBuiltInType}
import io.joern.rubysrc2cpg.utils.FreshNameGenerator
import io.joern.x2cpg.{Ast, ValidationMode, Defines as XDefines}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{
  ControlStructureTypes,
  DispatchTypes,
  EdgeTypes,
  Operators,
  PropertyNames
}

trait AstForExpressionsCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  val tmpGen = FreshNameGenerator(i => s"<tmp-$i>")

  protected def astForExpression(node: RubyNode): Ast = node match
    case node: StaticLiteral            => astForStaticLiteral(node)
    case node: HereDocNode              => astForHereDoc(node)
    case node: DynamicLiteral           => astForDynamicLiteral(node)
    case node: UnaryExpression          => astForUnary(node)
    case node: BinaryExpression         => astForBinary(node)
    case node: MemberAccess             => astForMemberAccess(node)
    case node: MemberCall               => astForMemberCall(node)
    case node: ObjectInstantiation      => astForObjectInstantiation(node)
    case node: IndexAccess              => astForIndexAccess(node)
    case node: SingleAssignment         => astForSingleAssignment(node)
    case node: AttributeAssignment      => astForAttributeAssignment(node)
    case node: RubyIdentifier           => astForSimpleIdentifier(node)
    case node: SimpleCall               => astForSimpleCall(node)
    case node: RequireCall              => astForRequireCall(node)
    case node: IncludeCall              => astForIncludeCall(node)
    case node: YieldExpr                => astForYield(node)
    case node: RangeExpression          => astForRange(node)
    case node: ArrayLiteral             => astForArrayLiteral(node)
    case node: HashLiteral              => astForHashLiteral(node)
    case node: Association              => astForAssociation(node)
    case node: IfExpression             => astForIfExpression(node)
    case node: UnlessExpression         => astForUnlessExpression(node)
    case node: RescueExpression         => astForRescueExpression(node)
    case node: MandatoryParameter       => astForMandatoryParameter(node)
    case node: SplattingRubyNode        => astForSplattingRubyNode(node)
    case node: AnonymousTypeDeclaration => astForAnonymousTypeDeclaration(node)
    case node: ProcOrLambdaExpr         => astForProcOrLambdaExpr(node)
    case node: RubyCallWithBlock[_]     => astsForCallWithBlockInExpr(node)
    case node: SelfIdentifier           => astForSelfIdentifier(node)
    case node: DummyNode                => Ast(node.node)
    case _                              => astForUnknown(node)

  protected def astForStaticLiteral(node: StaticLiteral): Ast = {
    Ast(literalNode(node, code(node), node.typeFullName))
  }

  protected def astForHereDoc(node: HereDocNode): Ast = {
    Ast(literalNode(node, code(node), getBuiltInType("String")))
  }

  // Helper for nil literals to put in empty clauses
  protected def astForNilLiteral: Ast = Ast(NewLiteral().code("nil").typeFullName(getBuiltInType(Defines.NilClass)))
  protected def astForNilBlock: Ast   = blockAst(NewBlock(), List(astForNilLiteral))

  protected def astForDynamicLiteral(node: DynamicLiteral): Ast = {
    val fmtValueAsts = node.expressions.map {
      case stmtList: StatementList if stmtList.size == 1 =>
        val expressionAst = astForExpression(stmtList.statements.head)
        val call = callNode(
          node = stmtList,
          code = stmtList.text,
          name = Operators.formattedValue,
          methodFullName = Operators.formattedValue,
          dispatchType = DispatchTypes.STATIC_DISPATCH,
          signature = None,
          typeFullName = Some(node.typeFullName)
        )
        callAst(call, Seq(expressionAst))
      case stmtList: StatementList if stmtList.size > 1 =>
        logger.warn(
          s"Interpolations containing multiple statements are not supported yet: ${stmtList.text} ($relativeFileName), skipping"
        )
        astForUnknown(stmtList)
      case node =>
        logger.warn(s"Unsupported interpolated literal content: ${code(node)} ($relativeFileName), skipping")
        astForUnknown(node)
    }
    callAst(
      callNode(
        node = node,
        code = code(node),
        name = Operators.formatString,
        methodFullName = Operators.formatString,
        dispatchType = DispatchTypes.STATIC_DISPATCH,
        signature = None,
        typeFullName = Some(node.typeFullName)
      ),
      fmtValueAsts
    )
  }

  protected def astForUnary(node: UnaryExpression): Ast = {
    getUnaryOperatorName(node.op) match
      case None =>
        logger.warn(s"Unrecognized unary operator: ${code(node)} ($relativeFileName), skipping")
        astForUnknown(node)
      case Some(op) =>
        val expressionAst = astForExpression(node.expression)
        val call          = callNode(node, code(node), op, op, DispatchTypes.STATIC_DISPATCH)
        callAst(call, Seq(expressionAst))
  }

  protected def astForBinary(node: BinaryExpression): Ast = {
    getBinaryOperatorName(node.op) match
      case None =>
        logger.warn(s"Unrecognized binary operator: ${code(node)} ($relativeFileName), skipping")
        astForUnknown(node)
      case Some("=~") =>
        astForMemberCall(MemberCall(node.lhs, ".", "=~", List(node.rhs))(node.span))
      case Some(op) =>
        val lhsAst = astForExpression(node.lhs)
        val rhsAst = astForExpression(node.rhs)
        val call   = callNode(node, code(node), op, op, DispatchTypes.STATIC_DISPATCH)
        callAst(call, Seq(lhsAst, rhsAst))
  }

  // Member accesses are lowered as calls, i.e. `x.y` is the call of `y` of `x` without any arguments.
  protected def astForMemberAccess(node: MemberAccess): Ast = {
    astForMemberCall(MemberCall(node.target, node.op, node.memberName, List.empty)(node.span))
  }

  /** Attempts to extract a type from the base of a member call.
    */
  protected def typeFromCallTarget(baseNode: RubyNode): Option[String] = {
    scope.lookupVariable(baseNode.text) match {
      // fixme: This should be under type recovery logic
      case Some(decl: NewLocal) if decl.typeFullName != Defines.Any             => Option(decl.typeFullName)
      case Some(decl: NewMethodParameterIn) if decl.typeFullName != Defines.Any => Option(decl.typeFullName)
      case Some(decl: NewLocal) if decl.dynamicTypeHintFullName.nonEmpty => decl.dynamicTypeHintFullName.headOption
      case Some(decl: NewMethodParameterIn) if decl.dynamicTypeHintFullName.nonEmpty =>
        decl.dynamicTypeHintFullName.headOption
      case _ =>
        astForExpression(baseNode).nodes
          .flatMap(_.properties.get(PropertyNames.TYPE_FULL_NAME).map(_.toString))
          .filterNot(_ == XDefines.Any)
          .headOption
    }
  }

  protected def astForMemberCall(node: MemberCall): Ast = {
    // Use the scope type recovery to attempt to obtain a receiver type for the call
    // TODO: Type recovery should potentially resolve this
    val fullName = typeFromCallTarget(node.target)
      .map(x => s"$x:${node.methodName}")
      .getOrElse(XDefines.DynamicCallUnknownFullName)

    val receiver     = astForExpression(node.target)
    val argumentAsts = node.arguments.map(astForMethodCallArgument)

    receiver.root.collect { case x: NewCall => x.typeFullName(fullName) }

    val fieldAccessCall = callNode(node, code(node), node.methodName, fullName, DispatchTypes.DYNAMIC_DISPATCH)

    callAst(fieldAccessCall, argumentAsts, Option(receiver))
  }

  protected def astForIndexAccess(node: IndexAccess): Ast = {
    // Array::[] and Hash::[] looks like an index access to the parser, some other methods may have this name too
    lazy val defaultBehaviour = {
      val indexAsts = node.indices.map(astForExpression)
      val targetAst = astForExpression(node.target)
      val call =
        callNode(node, code(node), Operators.indexAccess, Operators.indexAccess, DispatchTypes.STATIC_DISPATCH)
      callAst(call, targetAst +: indexAsts)
    }
    scope.tryResolveTypeReference(node.target.text).map(_.name) match {
      case Some(typeReference) =>
        scope
          .tryResolveMethodInvocation("[]", List.empty, Option(typeReference))
          .map { m =>
            val expr = astForExpression(MemberCall(node.target, "::", "[]", node.indices)(node.span))
            expr.root.collect { case x: NewCall =>
              x.methodFullName(s"$typeReference:${m.name}")
              scope.tryResolveTypeReference(m.returnType).map(_.name).foreach(x.typeFullName(_))
            }
            expr
          }
          .getOrElse(defaultBehaviour)
      case None => defaultBehaviour
    }
  }

  protected def astForObjectInstantiation(node: RubyNode & ObjectInstantiation): Ast = {
    val className  = node.target.text
    val methodName = XDefines.ConstructorMethodName
    val (receiverTypeFullName, fullName) = scope.tryResolveTypeReference(className) match {
      case Some(typeMetaData) => typeMetaData.name -> s"${typeMetaData.name}:$methodName"
      case None               => XDefines.Any      -> XDefines.DynamicCallUnknownFullName
    }
    /*
      Similarly to some other frontends, we lower the constructor into two operations, e.g.,
      `return Bar.new`, lowered to
      `return {Bar tmp = Bar.<alloc>(); tmp.<init>(); tmp}`
     */
    val block = blockNode(node)
    scope.pushNewScope(BlockScope(block))

    val tmp = SimpleIdentifier(Option(className))(node.span.spanStart(tmpGen.fresh))
    def tmpIdentifier = {
      val tmpAst = astForSimpleIdentifier(tmp)
      tmpAst.root.collect { case x: NewIdentifier => x.typeFullName(receiverTypeFullName) }
      tmpAst
    }

    // Assign tmp to <alloc>
    val receiverAst = Ast(identifierNode(node, className, className, receiverTypeFullName))
    val allocCall   = callNode(node, code(node), Operators.alloc, Operators.alloc, DispatchTypes.STATIC_DISPATCH)
    val allocAst    = callAst(allocCall, Seq.empty, Option(receiverAst))
    val assignmentCall = callNode(
      node,
      s"${tmp.text} = ${code(node)}",
      Operators.assignment,
      Operators.assignment,
      DispatchTypes.STATIC_DISPATCH
    )
    val tmpAssignment = callAst(assignmentCall, Seq(tmpIdentifier, allocAst))

    // Call constructor
    val argumentAsts = node match {
      case x: SimpleObjectInstantiation => x.arguments.map(astForMethodCallArgument)
      case x: ObjectInstantiationWithBlock =>
        val Seq(methodDecl, typeDecl, _, methodRef) = astForDoBlock(x.block): @unchecked
        Ast.storeInDiffGraph(methodDecl, diffGraph)
        Ast.storeInDiffGraph(typeDecl, diffGraph)
        x.arguments.map(astForMethodCallArgument) :+ methodRef
    }

    val constructorCall    = callNode(node, code(node), methodName, fullName, DispatchTypes.DYNAMIC_DISPATCH)
    val constructorCallAst = callAst(constructorCall, argumentAsts, Option(tmpIdentifier))
    scope.popScope()

    // Assemble statements
    blockAst(block, tmpAssignment :: constructorCallAst :: tmpIdentifier :: Nil)
  }

  protected def astForSingleAssignment(node: SingleAssignment): Ast = {
    node.rhs match {
      case x: Unknown if x.span.text == Defines.Undefined =>
        // If the RHS is undefined, then this variable is not defined/placed in the variable table/registry
        Ast()
      case _ =>
        getAssignmentOperatorName(node.op) match {
          case None =>
            logger.warn(s"Unrecognized assignment operator: ${code(node)} ($relativeFileName), skipping")
            astForUnknown(node)
          case Some(op) =>
            node.rhs match {
              case cfNode: ControlFlowExpression =>
                def elseAssignNil(span: TextSpan) = Option {
                  ElseClause(
                    StatementList(
                      SingleAssignment(
                        node.lhs,
                        node.op,
                        StaticLiteral(getBuiltInType(Defines.NilClass))(span.spanStart("nil"))
                      )(span.spanStart(s"${node.lhs.span.text} ${node.op} nil")) :: Nil
                    )(span.spanStart(s"${node.lhs.span.text} ${node.op} nil"))
                  )(span.spanStart(s"else\n\t${node.lhs.span.text} ${node.op} nil\nend"))
                }

                def transform(e: RubyNode & ControlFlowExpression): RubyNode =
                  transformLastRubyNodeInControlFlowExpressionBody(
                    e,
                    x => reassign(node.lhs, node.op, x, transform),
                    elseAssignNil
                  )
                astForExpression(transform(cfNode))
              case _ =>
                // The if the LHS defines a new variable, put the local variable into scope
                val lhsAst = node.lhs match {
                  case x: SimpleIdentifier if scope.lookupVariable(code(x)).isEmpty =>
                    val name  = code(x)
                    val local = localNode(x, name, name, Defines.Any)
                    scope.addToScope(name, local) match {
                      case BlockScope(block) => diffGraph.addEdge(block, local, EdgeTypes.AST)
                      case _                 =>
                    }
                    astForExpression(node.lhs)
                  case _ => astForExpression(node.lhs)
                }
                val rhsAst = astForExpression(node.rhs)

                // If this is a simple object instantiation assignment, we can give the LHS variable a type hint
                if (node.rhs.isInstanceOf[ObjectInstantiation] && lhsAst.root.exists(_.isInstanceOf[NewIdentifier])) {
                  rhsAst.nodes.collectFirst {
                    case tmp: NewIdentifier if tmp.name.startsWith("<tmp") && tmp.typeFullName != Defines.Any =>
                      lhsAst.root.collectFirst { case i: NewIdentifier =>
                        scope.lookupVariable(i.name).foreach {
                          case x: NewLocal =>
                            x.dynamicTypeHintFullName(x.dynamicTypeHintFullName :+ tmp.typeFullName)
                          case x: NewMethodParameterIn =>
                            x.dynamicTypeHintFullName(x.dynamicTypeHintFullName :+ tmp.typeFullName)
                        }
                        i.dynamicTypeHintFullName(i.dynamicTypeHintFullName :+ tmp.typeFullName)
                      }
                  }
                }

                val call = callNode(node, code(node), op, op, DispatchTypes.STATIC_DISPATCH)
                callAst(call, Seq(lhsAst, rhsAst))
            }
        }
    }
  }

  private def reassign(
    lhs: RubyNode,
    op: String,
    rhs: RubyNode,
    transform: (RubyNode & ControlFlowExpression) => RubyNode
  ): RubyNode = {
    def stmtListAssigningLastExpression(stmts: List[RubyNode]): List[RubyNode] = stmts match {
      case (head: ControlFlowClause) :: Nil     => clauseAssigningLastExpression(head) :: Nil
      case (head: ControlFlowExpression) :: Nil => transform(head) :: Nil
      case head :: Nil =>
        SingleAssignment(lhs, op, head)(rhs.span.spanStart(s"${lhs.span.text} $op ${head.span.text}")) :: Nil
      case Nil          => List.empty
      case head :: tail => head :: stmtListAssigningLastExpression(tail)
    }

    def clauseAssigningLastExpression(x: RubyNode & ControlFlowClause): RubyNode = x match {
      case RescueClause(exceptionClassList, assignment, thenClause) =>
        RescueClause(exceptionClassList, assignment, reassign(lhs, op, thenClause, transform))(x.span)
      case EnsureClause(thenClause) => EnsureClause(reassign(lhs, op, thenClause, transform))(x.span)
      case ElsIfClause(condition, thenClause) =>
        ElsIfClause(condition, reassign(lhs, op, thenClause, transform))(x.span)
      case ElseClause(thenClause) => ElseClause(reassign(lhs, op, thenClause, transform))(x.span)
      case WhenClause(matchExpressions, matchSplatExpression, thenClause) =>
        WhenClause(matchExpressions, matchSplatExpression, reassign(lhs, op, thenClause, transform))(x.span)
    }

    rhs match {
      case StatementList(statements)   => StatementList(stmtListAssigningLastExpression(statements))(rhs.span)
      case clause: ControlFlowClause   => clauseAssigningLastExpression(clause)
      case expr: ControlFlowExpression => transform(expr)
      case _ =>
        SingleAssignment(lhs, op, rhs)(rhs.span.spanStart(s"${lhs.span.text} $op ${rhs.span.text}"))
    }
  }

  // `x.y = 1` is lowered as `x.y=(1)`, i.e. as calling `y=` on `x` with argument `1`
  protected def astForAttributeAssignment(node: AttributeAssignment): Ast = {
    val call         = SimpleCall(node, List(node.rhs))(node.span)
    val memberAccess = MemberAccess(node.target, ".", s"${node.attributeName}=")(node.span)
    astForMemberCallWithoutBlock(call, memberAccess)
  }

  protected def astForSimpleIdentifier(node: RubyNode & RubyIdentifier): Ast = {
    val name = code(node)

    scope.lookupVariable(name) match {
      case Some(_) => handleVariableOccurrence(node)
      case None if scope.tryResolveMethodInvocation(node.text, List.empty).isDefined =>
        astForSimpleCall(SimpleCall(node, List())(node.span))
      case None => handleVariableOccurrence(node)
    }
  }

  protected def astForMandatoryParameter(node: RubyNode): Ast = handleVariableOccurrence(node)

  protected def astForSimpleCall(node: SimpleCall): Ast = {
    node.target match
      case targetNode: SimpleIdentifier => astForMethodCallWithoutBlock(node, targetNode)
      case targetNode: MemberAccess     => astForMemberCallWithoutBlock(node, targetNode)
      case targetNode =>
        logger.warn(s"Unrecognized target of call: ${targetNode.text} ($relativeFileName), skipping")
        astForUnknown(targetNode)
  }

  protected def astForRequireCall(node: RequireCall): Ast = {
    val pathOpt = node.argument match {
      case arg: StaticLiteral if arg.isString => Option(arg.innerText)
      case _                                  => None
    }
    pathOpt.foreach(path => scope.addRequire(path, node.isRelative))
    astForSimpleCall(node.asSimpleCall)
  }

  protected def astForIncludeCall(node: IncludeCall): Ast = {
    scope.addInclude(
      node.argument.text.replaceAll("::", ".")
    ) // Maybe generate ast and get name in a more structured approach instead
    astForSimpleCall(node.asSimpleCall)
  }

  protected def astForYield(node: YieldExpr): Ast = {
    scope.useProcParam match {
      case Some(param) =>
        val call = astForExpression(
          SimpleCall(SimpleIdentifier()(node.span.spanStart(param)), node.arguments)(node.span)
        )
        val ret = returnAst(returnNode(node, code(node)))
        val cond = astForExpression(
          SimpleCall(SimpleIdentifier()(node.span.spanStart(tmpGen.fresh)), List())(node.span.spanStart("<nondet>"))
        )
        callAst(
          callNode(node, code(node), Operators.conditional, Operators.conditional, DispatchTypes.STATIC_DISPATCH),
          List(cond, call, ret)
        )
      case None =>
        logger.warn(s"Yield expression outside of method scope: ${code(node)} ($relativeFileName), skipping")
        astForUnknown(node)

    }
  }

  protected def astForRange(node: RangeExpression): Ast = {
    val lbAst = astForExpression(node.lowerBound)
    val ubAst = astForExpression(node.upperBound)
    val call  = callNode(node, code(node), Operators.range, Operators.range, DispatchTypes.STATIC_DISPATCH)
    callAst(call, Seq(lbAst, ubAst))
  }

  protected def astForArrayLiteral(node: ArrayLiteral): Ast = {
    if (node.isDynamic) {
      logger.warn(s"Interpolated array literals are not supported yet: ${code(node)} ($relativeFileName), skipping")
      astForUnknown(node)
    } else {
      val arguments = if (node.text.startsWith("%")) {
        val argumentsType =
          if (node.isStringArray) getBuiltInType(Defines.String)
          else getBuiltInType(Defines.Symbol)
        node.elements.map {
          case element @ StaticLiteral(_) => StaticLiteral(argumentsType)(element.span)
          case element                    => element
        }
      } else {
        node.elements
      }
      val argumentAsts = arguments.map(astForExpression)

      val call =
        callNode(
          node,
          code(node),
          Operators.arrayInitializer,
          Operators.arrayInitializer,
          DispatchTypes.STATIC_DISPATCH
        )
      callAst(call, argumentAsts)
    }
  }

  protected def astForHashLiteral(node: HashLiteral): Ast = {
    val tmp = tmpGen.fresh

    def tmpAst(tmpNode: Option[RubyNode] = None) = astForSimpleIdentifier(
      SimpleIdentifier()(tmpNode.map(_.span).getOrElse(node.span).spanStart(tmp))
    )

    val block = blockNode(node)
    scope.pushNewScope(BlockScope(block))

    val argumentAsts = node.elements.flatMap(elem =>
      elem match
        case associationNode: Association => astForAssociationHash(associationNode, tmp)
        case node =>
          logger.warn(s"Could not represent element: ${code(node)} ($relativeFileName), skipping")
          astForUnknown(node) :: Nil
    )

    val hashInitCall = callNode(
      node,
      code(node),
      RubyOperators.hashInitializer,
      RubyOperators.hashInitializer,
      DispatchTypes.STATIC_DISPATCH
    )

    val assignment =
      callNode(node, code(node), Operators.assignment, Operators.assignment, DispatchTypes.STATIC_DISPATCH)
    val tmpAssignment = callAst(assignment, tmpAst() :: Ast(hashInitCall) :: Nil)

    scope.popScope()
    blockAst(block, tmpAssignment +: argumentAsts :+ tmpAst(node.elements.lastOption))
  }

  protected def astForAssociationHash(node: Association, tmp: String): List[Ast] = {
    node.key match {
      case rangeExpr: RangeExpression =>
        val expandedList = generateStaticLiteralsForRange(rangeExpr).map { x =>
          astForSingleKeyValue(x, node.value, tmp)
        }

        if (expandedList.nonEmpty) {
          expandedList
        } else {
          astForSingleKeyValue(node.key, node.value, tmp) :: Nil
        }

      case _ => astForSingleKeyValue(node.key, node.value, tmp) :: Nil
    }
  }

  protected def generateStaticLiteralsForRange(node: RangeExpression): List[StaticLiteral] = {
    (node.lowerBound, node.upperBound) match {
      case (lb: StaticLiteral, ub: StaticLiteral) =>
        (lb.typeFullName, ub.typeFullName) match {
          case ("__builtin.Integer", "__builtin.Integer") =>
            generateRange(lb.span.text.toInt, ub.span.text.toInt, node.rangeOperator.exclusive)
              .map(x =>
                StaticLiteral(lb.typeFullName)(TextSpan(lb.line, lb.column, lb.lineEnd, lb.columnEnd, x.toString))
              )
              .toList
          case ("__builtin.String", "__builtin.String") =>
            val lbVal = lb.span.text.replaceAll("['\"]", "")
            val ubVal = ub.span.text.replaceAll("['\"]", "")

            // TODO: Also might need to check if one is upper case and other is lower, since in Ruby this would not
            //  create any range but it might with this impl of using ASCII values.
            if (lbVal.length > 1 || ubVal.length > 1) {
              // Not simulating the case where we have something like "ab"..."ad"
              return List.empty
            }

            generateRange(lbVal(0).toInt, ubVal(0).toInt, node.rangeOperator.exclusive)
              .map(x =>
                StaticLiteral(lb.typeFullName)(
                  TextSpan(lb.line, lb.column, lb.lineEnd, lb.columnEnd, s"\'${x.toChar.toString}\'")
                )
              )
              .toList
          case _ =>
            List.empty
        }
      case _ =>
        List.empty
    }
  }

  private def generateRange(lhs: Int, rhs: Int, exclusive: Boolean): Range = {
    if exclusive then lhs until rhs
    else lhs to rhs
  }

  protected def astForAssociation(node: Association): Ast = {
    val key   = astForExpression(node.key)
    val value = astForExpression(node.value)
    val call =
      callNode(node, code(node), RubyOperators.association, RubyOperators.association, DispatchTypes.STATIC_DISPATCH)
    callAst(call, Seq(key, value))
  }

  protected def astForSingleKeyValue(keyNode: RubyNode, valueNode: RubyNode, tmp: String): Ast = {
    astForExpression(
      SingleAssignment(
        IndexAccess(
          SimpleIdentifier()(TextSpan(keyNode.line, keyNode.column, keyNode.lineEnd, keyNode.columnEnd, tmp)),
          List(keyNode)
        )(TextSpan(keyNode.line, keyNode.column, keyNode.lineEnd, keyNode.columnEnd, s"$tmp[${keyNode.span.text}]")),
        "=",
        valueNode
      )(
        TextSpan(
          keyNode.line,
          keyNode.column,
          keyNode.lineEnd,
          keyNode.columnEnd,
          s"$tmp[${keyNode.span.text}] = ${valueNode.span.text}"
        )
      )
    )
  }

  // Recursively lowers into a ternary conditional call
  protected def astForIfExpression(node: IfExpression): Ast = {
    def builder(node: IfExpression, conditionAst: Ast, thenAst: Ast, elseAsts: List[Ast]): Ast = {
      // We want to make sure there's always an «else» clause in a ternary operator.
      // The default value is a `nil` literal.
      val elseAsts_ = if (elseAsts.isEmpty) {
        List(astForNilBlock)
      } else {
        elseAsts
      }

      val call = callNode(node, code(node), Operators.conditional, Operators.conditional, DispatchTypes.STATIC_DISPATCH)
      callAst(call, conditionAst :: thenAst :: elseAsts_)
    }
    foldIfExpression(builder)(node)
  }

  protected def astForUnlessExpression(node: UnlessExpression): Ast = {
    val notConditionAst = UnaryExpression("!", node.condition)(node.condition.span)
    astForExpression(IfExpression(notConditionAst, node.trueBranch, List(), node.falseBranch)(node.span))
  }

  protected def astForRescueExpression(node: RescueExpression): Ast = {
    val tryAst = astForStatementList(node.body.asStatementList)
    val rescueAsts = node.rescueClauses
      .map {
        case x: RescueClause =>
          // TODO: add exception assignment
          astForStatementList(x.thenClause.asStatementList)
        case x => astForUnknown(x)
      }
    val elseAst = node.elseClause.map {
      case x: ElseClause => astForStatementList(x.thenClause.asStatementList)
      case x             => astForUnknown(x)
    }
    val ensureAst = node.ensureClause.map {
      case x: EnsureClause => astForStatementList(x.thenClause.asStatementList)
      case x               => astForUnknown(x)
    }
    tryCatchAst(
      NewControlStructure()
        .controlStructureType(ControlStructureTypes.TRY)
        .code(code(node)),
      tryAst,
      rescueAsts ++ elseAst.toSeq,
      ensureAst
    )
  }

  private def astForSelfIdentifier(node: SelfIdentifier): Ast = {
    val thisIdentifier = identifierNode(node, "this", code(node), scope.surroundingTypeFullName.getOrElse(Defines.Any))
    Ast(thisIdentifier)
  }

  protected def astForUnknown(node: RubyNode): Ast = {
    val className = node.getClass.getSimpleName
    val text      = code(node)
    logger.warn(s"Could not represent expression: $text ($className) ($relativeFileName), skipping")
    Ast(unknownNode(node, text))
  }

  private def astForMemberCallWithoutBlock(node: SimpleCall, memberAccess: MemberAccess): Ast = {
    val receiverAst = astForFieldAccess(memberAccess)
    val methodName  = memberAccess.memberName
    // TODO: Type recovery should potentially resolve this
    val methodFullName = typeFromCallTarget(memberAccess.target)
      .map(x => s"$x:$methodName")
      .getOrElse(XDefines.DynamicCallUnknownFullName)
    val argumentAsts = node.arguments.map(astForMethodCallArgument)
    val call         = callNode(node, code(node), methodName, methodFullName, DispatchTypes.DYNAMIC_DISPATCH)

    callAst(call, argumentAsts, Some(receiverAst))
  }

  private def astForMethodCallWithoutBlock(node: SimpleCall, methodIdentifier: SimpleIdentifier): Ast = {
    val methodName = methodIdentifier.text

    lazy val defaultResult = Defines.Any -> XDefines.DynamicCallUnknownFullName
    val (receiverType, methodFullName) = scope.tryResolveMethodInvocation(methodName, List.empty) match {
      case Some(m) =>
        scope.typeForMethod(m).map(t => t.name -> s"${t.name}:${m.name}").getOrElse(defaultResult)
      case None => defaultResult
    }
    val argumentAst      = node.arguments.map(astForMethodCallArgument)
    val call             = callNode(node, code(node), methodName, methodFullName, DispatchTypes.DYNAMIC_DISPATCH)
    val receiverCallName = identifierNode(node, call.name, call.name, receiverType)

    callAst(call, argumentAst, Option(Ast(receiverCallName)))
  }

  private def astForProcOrLambdaExpr(node: ProcOrLambdaExpr): Ast = {
    val Seq(methodDecl, typeDecl, _, methodRef) = astForDoBlock(node.block): @unchecked

    Ast.storeInDiffGraph(methodDecl, diffGraph)
    Ast.storeInDiffGraph(typeDecl, diffGraph)

    methodRef
  }

  private def astsForCallWithBlockInExpr[C <: RubyCall](node: RubyNode & RubyCallWithBlock[C]): Ast = {
    val Seq(methodDecl, typeDecl, callWithLambdaArg) = astsForCallWithBlock(node): @unchecked

    Ast.storeInDiffGraph(methodDecl, diffGraph)
    Ast.storeInDiffGraph(typeDecl, diffGraph)

    callWithLambdaArg
  }

  private def astForMethodCallArgument(node: RubyNode): Ast = {
    node match
      // Associations in method calls are keyword arguments
      case assoc: Association => astForKeywordArgument(assoc)
      case block: RubyBlock =>
        val Seq(methodDecl, typeDecl, _, methodRef) = astForDoBlock(block)
        Ast.storeInDiffGraph(methodDecl, diffGraph)
        Ast.storeInDiffGraph(typeDecl, diffGraph)

        methodRef
      case _ => astForExpression(node)
  }

  private def astForKeywordArgument(assoc: Association): Ast = {
    val value = astForExpression(assoc.value)
    astForExpression(assoc.key).root match
      case Some(keyNode: NewIdentifier) =>
        value.root.collectFirst { case x: ExpressionNew =>
          x.argumentName_=(Option(keyNode.name))
          x.argumentIndex_=(-1)
        }
        value
      case _ => astForExpression(assoc)
  }

  protected def astForFieldAccess(node: MemberAccess): Ast = {
    val fieldIdentifierAst = Ast(fieldIdentifierNode(node, node.memberName, node.memberName))
    val targetAst          = astForExpression(node.target)
    val code               = s"${node.target.text}${node.op}${node.memberName}"
    val memberType = typeFromCallTarget(node.target)
      .flatMap(scope.tryResolveTypeReference)
      .map(_.fields)
      .getOrElse(List.empty)
      .collectFirst {
        case x if x.name == node.memberName =>
          scope.tryResolveTypeReference(x.typeName).map(_.name).getOrElse(Defines.Any)
      }
      .orElse(Option(Defines.Any))
    val fieldAccess = callNode(
      node,
      code,
      Operators.fieldAccess,
      Operators.fieldAccess,
      DispatchTypes.STATIC_DISPATCH,
      signature = None,
      typeFullName = memberType
    )
    callAst(fieldAccess, Seq(targetAst, fieldIdentifierAst))
  }

  protected def astForSplattingRubyNode(node: SplattingRubyNode): Ast = {
    val splattingCall =
      callNode(node, code(node), RubyOperators.splat, RubyOperators.splat, DispatchTypes.STATIC_DISPATCH)
    val argumentAst = astsForStatement(node.name)
    callAst(splattingCall, argumentAst)
  }

  private def getBinaryOperatorName(op: String): Option[String]     = BinaryOperatorNames.get(op)
  private def getUnaryOperatorName(op: String): Option[String]      = UnaryOperatorNames.get(op)
  private def getAssignmentOperatorName(op: String): Option[String] = AssignmentOperatorNames.get(op)
}
