package io.joern.rust2cpg.astcreation

import io.joern.rust2cpg.parser.RustNodeSyntax.*
import io.joern.x2cpg.datastructures.Stack.*
import io.joern.x2cpg.utils.AstPropertiesUtil.RootProperties
import io.joern.x2cpg.{Ast, Defines, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.{NewFile, NewModifier, NewNamespaceBlock}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, EvaluationStrategies, ModifierTypes, Operators}

trait RustVisitor(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  // SourceFile =
  //  '#shebang'?
  //  '#frontmatter'?
  //  Attr*
  //  Item*
  protected def visitSourceFile(sourceFile: SourceFile): Ast = {
    val fileNode = NewFile().name(parseResult.filename).order(0)
    Option.unless(config.disableFileContent)(parseResult.fileContent).foreach(fileNode.content(_))

    val namespaceBlockNode = globalNamespaceBlockNode()
    val globalMethodAst    = astInFakeMethod(sourceFile, namespaceBlockNode)

    Ast(fileNode).withChild(Ast(namespaceBlockNode).withChild(globalMethodAst))
  }

  private def astInFakeMethod(sourceFile: SourceFile, namespaceBlockNode: NewNamespaceBlock): Ast = {
    val method = globalFakeMethodNode(sourceFile, namespaceBlockNode)

    methodAstParentStack.push(namespaceBlockNode)
    methodAstParentStack.push(method)
    val itemAsts = sourceFile.item.flatMap(visitItem)
    methodAstParentStack.pop()
    methodAstParentStack.pop()

    val block        = blockNode(sourceFile)
    val methodReturn = methodReturnNode(sourceFile, "()")
    val modifiers    = Seq(modifierNode(sourceFile, ModifierTypes.MODULE))
    methodAst(method, parameters = Nil, body = Ast(block).withChildren(itemAsts), methodReturn, modifiers)
  }

  private def visitItem(item: Item): Seq[Ast] = item match {
    case const: Const           => visitConst(const)
    case x: Enum                => notHandledYet(x) :: Nil
    case x: ExternBlock         => notHandledYet(x) :: Nil
    case x: ExternCrate         => notHandledYet(x) :: Nil
    case fn: Fn                 => visitFn(fn) :: Nil
    case impl: Impl             => visitImpl(impl)
    case macroCall: MacroCall   => visitMacroCall(macroCall)
    case macroRules: MacroRules => visitMacroRules(macroRules)
    case macroDef: MacroDef     => visitMacroDef(macroDef)
    case module: Module         => visitModule(module)
    case static: Static         => visitStatic(static)
    case struct: Struct         => visitStruct(struct) :: Nil
    case trait_ : Trait         => visitTrait(trait_) :: Nil
    case x: TypeAlias           => notHandledYet(x) :: Nil
    case x: Union               => notHandledYet(x) :: Nil
    case x: Use                 => notHandledYet(x) :: Nil
    case x: AsmExpr             => notHandledYet(x) :: Nil
  }

  private def visitStmt(stmt: Stmt): Seq[Ast] = stmt match {
    case exprStmt: ExprStmt => visitExprStmt(exprStmt)
    case item: Item         => visitItem(item)
    case letStmt: LetStmt   => visitLetStmt(letStmt)
  }

  // ExprStmt =
  //  Expr ';'?
  private def visitExprStmt(exprStmt: ExprStmt): Seq[Ast] = exprStmt.expr match {
    // MacroExprs in this context may expand into multiple statements, which
    // should be flattened into the enclosing block.
    case macroExpr: MacroExpr => visitMacroCall(macroExpr.macroCall)
    case expr                 => visitExpr(expr) :: Nil
  }

  private def visitExpr(expr: Expr): Ast = {
    val adjustments = expr.adjustments.getOrElse(Nil)
    val exprAst     = dispatchVisitExpr(expr)
    adjustments.foldLeft(exprAst)(lowerAdjustment(expr, _, _))
  }

  private def dispatchVisitExpr(expr: Expr): Ast = expr match {
    case arrayExpr: ArrayExpr           => visitArrayExpr(arrayExpr)
    case x: AsmExpr                     => notHandledYet(x)
    case awaitExpr: AwaitExpr           => visitAwaitExpr(awaitExpr)
    case binExpr: BinExpr               => visitBinExpr(binExpr)
    case blockExpr: BlockExpr           => visitBlockExpr(blockExpr)
    case breakExpr: BreakExpr           => visitBreakExpr(breakExpr)
    case callExpr: CallExpr             => visitCallExpr(callExpr)
    case castExpr: CastExpr             => visitCastExpr(castExpr)
    case x: ClosureExpr                 => notHandledYet(x)
    case continueExpr: ContinueExpr     => visitContinueExpr(continueExpr)
    case fieldExpr: FieldExpr           => visitFieldExpr(fieldExpr)
    case forExpr: ForExpr               => visitForExpr(forExpr)
    case formatArgsExpr: FormatArgsExpr => visitFormatArgsExpr(formatArgsExpr)
    case ifExpr: IfExpr                 => visitIfExpr(ifExpr)
    case indexExpr: IndexExpr           => visitIndexExpr(indexExpr)
    case literal: Literal               => visitLiteral(literal)
    case loopExpr: LoopExpr             => visitLoopExpr(loopExpr)
    case macroExpr: MacroExpr           => visitMacroExpr(macroExpr)
    case x: MatchExpr                   => notHandledYet(x)
    case methodCallExpr: MethodCallExpr => visitMethodCallExpr(methodCallExpr)
    case x: OffsetOfExpr                => notHandledYet(x)
    case expr: ParenExpr                => visitExpr(expr.expr)
    case pathExpr: PathExpr             => visitPathExpr(pathExpr)
    case prefixExpr: PrefixExpr         => visitPrefixExpr(prefixExpr)
    case x: RangeExpr                   => notHandledYet(x)
    case recordExpr: RecordExpr         => visitRecordExpr(recordExpr)
    case refExpr: RefExpr               => visitRefExpr(refExpr)
    case returnExpr: ReturnExpr         => visitReturnExpr(returnExpr)
    case x: BecomeExpr                  => notHandledYet(x)
    case tryExpr: TryExpr               => visitTryExpr(tryExpr)
    case tupleExpr: TupleExpr           => visitTupleExpr(tupleExpr)
    case whileExpr: WhileExpr           => visitWhileExpr(whileExpr)
    case x: YieldExpr                   => notHandledYet(x)
    case x: YeetExpr                    => notHandledYet(x)
    case x: LetExpr                     => notHandledYet(x)
    case x: UnderscoreExpr              => notHandledYet(x)
  }

  private def lowerAdjustment(expr: Expr, exprAst: Ast, adjustment: Adjustment) = adjustment match {
    case deref: Deref                     => lowerDerefAdjustment(expr, exprAst, deref)
    case borrow: Borrow                   => lowerBorrowAdjustment(expr, exprAst, borrow)
    case cast: Cast                       => lowerCastAdjustment(expr, exprAst, cast)
    case overloadedDeref: OverloadedDeref => lowerOverloadedDerefAdjustment(expr, exprAst, overloadedDeref)
  }

  private def lowerDerefAdjustment(expr: Expr, exprAst: Ast, deref: Deref) = {
    val exprCode     = exprAst.rootCodeOrEmpty
    val typeFullName = deref.target
    callAst(operatorCallNode(expr, s"*$exprCode", Operators.indirection, Some(typeFullName)), Seq(exprAst))
  }

  private def lowerBorrowAdjustment(expr: Expr, exprAst: Ast, borrow: Borrow) = {
    val exprCode     = exprAst.rootCodeOrEmpty
    val typeFullName = borrow.target
    callAst(operatorCallNode(expr, s"&$exprCode", Operators.addressOf, Some(typeFullName)), Seq(exprAst))
  }

  private def lowerCastAdjustment(expr: Expr, exprAst: Ast, cast: Cast) = {
    val exprCode     = exprAst.rootCodeOrEmpty
    val typeFullName = cast.target
    val castNode     = operatorCallNode(expr, s"$exprCode as $typeFullName", Operators.cast, Some(typeFullName))
    val typeRefAst   = Ast(typeRefNode(expr, typeFullName, typeFullName))
    callAst(castNode, Seq(typeRefAst, exprAst))
  }

  private def lowerOverloadedDerefAdjustment(expr: Expr, exprAst: Ast, overloadedDeref: OverloadedDeref) = {
    val exprCode     = exprAst.rootCodeOrEmpty
    val typeFullName = overloadedDeref.target
    overloadedDeref.methodFullName match {
      case Some(methodFullName) =>
        val name = methodFullName.split(RustFullNames.PathSep).last
        val call =
          callNode(
            expr,
            s"$exprCode.$name()",
            name,
            methodFullName,
            DispatchTypes.STATIC_DISPATCH,
            None,
            Some(typeFullName)
          )
        callAst(call, arguments = Nil, base = Some(exprAst))
      case None =>
        callAst(operatorCallNode(expr, s"*$exprCode", Operators.indirection, Some(typeFullName)), Seq(exprAst))
    }
  }

  private def visitType(typ: Type): Ast = typ match {
    case x: ArrayType     => notHandledYet(x)
    case x: DynTraitType  => notHandledYet(x)
    case x: FnPtrType     => notHandledYet(x)
    case x: ForType       => notHandledYet(x)
    case x: ImplTraitType => notHandledYet(x)
    case x: InferType     => notHandledYet(x)
    case x: MacroType     => notHandledYet(x)
    case x: NeverType     => notHandledYet(x)
    case x: ParenType     => notHandledYet(x)
    case x: PathType      => notHandledYet(x)
    case x: PtrType       => notHandledYet(x)
    case x: RefType       => notHandledYet(x)
    case x: SliceType     => notHandledYet(x)
    case x: TupleType     => notHandledYet(x)
  }

  // Module =
  //  Attr* Visibility?
  //  'mod' Name (ItemList | ';')
  private def visitModule(module: Module): Seq[Ast] = {
    module.itemList match {
      case None =>
        // It's a forward declaration for which we don't have an AST use.
        Nil
      case Some(itemList) =>
        // NB: two same-named `mod foo {}` declarations in the same file are not valid, e.g.
        // ```
        // mod foo {...}
        // ...
        // mod foo {...}
        // ```
        // So we don't need to disambiguate its occurrence like in other languages.
        val namespaceBlock = moduleNamespaceBlockNode(module)
        methodAstParentStack.push(namespaceBlock)
        val itemAsts = itemList.item.flatMap(visitItem)
        methodAstParentStack.pop()
        Ast(namespaceBlock).withChildren(itemAsts) :: Nil
    }
  }

  // Const =
  //  Attr* Visibility?
  //  'default'?
  //  'const' (Name | '_') GenericParamList? ':' Type
  //  ('=' body:Expr)?
  //  WhereClause? ';'
  private def visitConst(const: Const): Seq[Ast] = {
    (const.name.flatMap(_.identToken), const.expr) match {
      case (Some(identToken), Some(rhsExpr)) =>
        val typeFullName = typeFullNameForType(const.typ)
        lowerIdentifierDecl(identToken, rhsExpr, typeFullName, code(const))
      case _ => notHandledYet(const) :: Nil
    }
  }

  // Static =
  //  Attr* Visibility?
  //  'unsafe'? 'safe'? 'static' 'mut'? Name ':' Type
  //  ('=' Expr)? ';'
  private def visitStatic(static: Static): Seq[Ast] = {
    (static.name.identToken, static.expr) match {
      case (Some(identToken), Some(rhsExpr)) =>
        val typeFullName = typeFullNameForType(static.typ)
        lowerIdentifierDecl(identToken, rhsExpr, typeFullName, code(static))
      case _ => notHandledYet(static) :: Nil
    }
  }

  // LetStmt =
  //  Attr* 'super'? 'let' Pat (':' Type)?
  //  '=' initializer:Expr?
  //  LetElse?
  //  ';'
  private def visitLetStmt(letStmt: LetStmt): Seq[Ast] = {
    letStmt.pat match {
      case identPat: IdentPat =>
        identPat.name.identToken match {
          case Some(identToken) =>
            val typeFullName = letStmt.typ.map(typeFullNameForType).getOrElse(typeFullNameForIdentPat(identPat))
            letStmt.expr match {
              case Some(rhsExpr) => lowerIdentifierDecl(identToken, rhsExpr, typeFullName, code(letStmt))
              case None =>
                val lhsName = code(identToken)
                val local   = localNode(identToken, lhsName, lhsName, typeFullName)
                Ast(local) :: Nil
            }
          case None => notHandledYet(letStmt) :: Nil
        }
      case _ => notHandledYet(letStmt) :: Nil
    }
  }

  // Creates:
  // - LOCAL (lhsToken) with given typeFullName
  // - CALL (assignment) for lhsToken = rhsExpr
  private def lowerIdentifierDecl(
    lhsToken: IdentToken,
    rhsExpr: Expr,
    typeFullName: String,
    declCode: String
  ): Seq[Ast] = {
    val lhsName = code(lhsToken)

    val local = localNode(lhsToken, lhsName, code(lhsToken), typeFullName)
    val ident = identifierNode(lhsToken, lhsName, code(lhsToken), typeFullName)

    val lhsAst        = Ast(ident).withRefEdge(ident, local) // TODO: remove once ref linker is in.
    val rhsAst        = visitExpr(rhsExpr)
    val localAst      = Ast(local)
    val assignmentAst = callAst(assignmentNode(lhsToken, declCode), Seq(lhsAst, rhsAst))

    Seq(localAst, assignmentAst)
  }

  // Name =
  //  '#ident' | 'self'
  private def visitName(name: Name): Ast = {
    name.identToken match {
      case None             => notHandledYet(name)
      case Some(identToken) => Ast()
    }
  }

  private def visitLiteral(lit: Literal): Ast = {
    val typeFullName = typeFullNameForLiteral(lit)
    Ast(literalNode(lit, code(lit), typeFullName))
  }

  extension (lit: Literal) {
    protected def value: Option[RustToken] =
      lit.intNumberToken
        .orElse(lit.floatNumberToken)
        .orElse(lit.stringToken)
        .orElse(lit.byteStringToken)
        .orElse(lit.cStringToken)
        .orElse(lit.charToken)
        .orElse(lit.byteToken)
        .orElse(lit.trueKwToken)
        .orElse(lit.falseKwToken)
  }

  // Fn =
  // Attr* Visibility?
  // 'default'? 'const'? 'async'? 'gen'? 'unsafe'? 'safe'? Abi?
  // 'fn' Name GenericParamList? ParamList RetType? WhereClause?
  // (body:BlockExpr | ';')
  private def visitFn(fn: Fn): Ast = {
    val method          = methodNode(node = fn, name = code(fn.name))
    val retTypeFullName = fn.retType.map(_.typ).map(typeFullNameForType).getOrElse("()")
    val methodRet       = methodReturnNode(fn, retTypeFullName)
    val methodMods      = Seq[NewModifier]()

    methodAstParentStack.push(method)
    val paramAsts = visitParamList(fn.paramList)
    val bodyAst   = fn.blockExpr.map(lowerFnBody).getOrElse(blockAst(blockNode(fn)))
    methodAstParentStack.pop()

    methodAst(method = method, parameters = paramAsts, body = bodyAst, methodReturn = methodRet, modifiers = methodMods)
  }

  // Creates:
  // BLOCK {
  //   <stmts>
  //   RETURN (expr) // if (expr) exists
  // }
  private def lowerFnBody(blockExpr: BlockExpr): Ast = {
    val stmtAsts   = blockExpr.stmtList.stmt.flatMap(visitStmt)
    val retExprAst = blockExpr.stmtList.expr.map(lowerReturnExpr).toList
    Ast(blockNode(blockExpr)).withChildren(stmtAsts ++ retExprAst)
  }

  // Creates:
  // RETURN expr
  private def lowerReturnExpr(expr: Expr): Ast = {
    val exprAst = visitExpr(expr)
    val ret     = returnNode(expr, code(expr))
    returnAst(ret, Seq(exprAst))
  }

  // Impl =
  //  Attr* Visibility?
  //  'default'? 'unsafe'?
  //  'impl' GenericParamList? ('const'? '!'? Type 'for')? Type WhereClause?
  //  AssocItemList
  private def visitImpl(impl: Impl): Seq[Ast] = {
    if (impl.forKwToken.isDefined) {
      impl.typ match {
        case implTrait :: implType :: Nil =>
          val typeDecl = typeDeclForTraitImpl(impl, implTrait, implType)
          methodAstParentStack.push(typeDecl)
          val methodAsts = impl.assocItemList.assocItem.collect { case fn: Fn => visitFn(fn) }
          methodAstParentStack.pop()
          Ast(typeDecl).withChildren(methodAsts) :: Nil
        case _ => notHandledYet(impl) :: Nil
      }
    } else {
      methodAstParentStack.push(typeDeclForImpl(impl))
      val methodAsts = impl.assocItemList.assocItem.collect { case fn: Fn => visitFn(fn) }
      methodAstParentStack.pop()
      // TODO: remove this side-effect once ref-linker/scope is in.
      methodAsts.foreach(Ast.storeInDiffGraph(_, diffGraph))
      Nil
    }
  }

  // Trait =
  //  Attr* Visibility?
  //  'unsafe'? 'auto'?
  //  ImplRestriction?
  //  'trait' Name GenericParamList?
  //  (((':' TypeBoundList?)? WhereClause? AssocItemList) |
  //  ('=' TypeBoundList? WhereClause? ';'))
  private def visitTrait(trait_ : Trait): Ast = {
    val name = code(trait_.name)
    val typeDecl = typeDeclNode(
      node = trait_,
      name = name,
      fullName = composeRustFullName(name),
      filename = parseResult.filename,
      code = code(trait_)
    )
    methodAstParentStack.push(typeDecl)
    val methodAsts = trait_.assocItemList.toSeq.flatMap(_.assocItem).collect { case fn: Fn => visitFn(fn) }
    methodAstParentStack.pop()
    Ast(typeDecl).withChildren(methodAsts)
  }

  // BlockExpr =
  //  Attr* Label? (TryBlockModifier | 'unsafe' | ('async' 'move'?) | ('gen' 'move'?) | 'const') StmtList
  private def visitBlockExpr(blockExpr: BlockExpr): Ast = {
    val stmts = visitStmtList(blockExpr.stmtList)
    val block = blockNode(blockExpr)
    Ast(block).withChildren(stmts)
  }

  // ReturnExpr =
  //  Attr* 'return' Expr?
  private def visitReturnExpr(returnExpr: ReturnExpr): Ast = {
    val ret     = returnNode(returnExpr, code(returnExpr))
    val exprAst = returnExpr.expr.map(visitExpr)
    returnAst(ret, exprAst.toList)
  }

  // CallExpr =
  //  Attr* Expr ArgList
  private def visitCallExpr(callExpr: CallExpr): Ast = {
    viewExprAsPathExpr(callExpr.expr) match {
      case Some(nameRefs) =>
        val name           = code(nameRefs.last)
        val methodFullName = methodFullNameForCallExpr(callExpr, nameRefs)
        val typeFullName   = typeFullNameForExpr(callExpr)
        val dispatch       = DispatchTypes.STATIC_DISPATCH
        val call =
          callNode(callExpr, code(callExpr), name, methodFullName, dispatch, None, Some(typeFullName))
        val args = callExpr.argList.expr.map(visitExpr)
        if (callExpr.hasSelfReceiver.isDefined && args.nonEmpty) {
          callAst(call, args.tail, base = Some(args.head))
        } else {
          callAst(call, args)
        }
      case None => notHandledYet(callExpr)
    }
  }

  private def viewExprAsNameRef(expr: Expr): Option[NameRef] = {
    viewExprAsPathExpr(expr).filter(_.sizeIs == 1).flatMap(_.headOption)
  }

  private def viewExprAsPathExpr(expr: Expr): Option[Seq[NameRef]] = expr match {
    case pathExpr: PathExpr => viewPathAsSequenceOfNameRefs(pathExpr.path)
    case _                  => None
  }

  private def viewPathAsSequenceOfNameRefs(path: Path): Option[Seq[NameRef]] = {
    path.pathSegment.nameRef.flatMap { nameRef =>
      path.path match {
        case Some(qualifier) => viewPathAsSequenceOfNameRefs(qualifier).map(_ :+ nameRef)
        case None            => Some(nameRef :: Nil)
      }
    }
  }

  // ArgList =
  //  '(' args:(Expr (',' Expr)* ','?)? ')'
  private def visitArgList(argList: ArgList): Ast = {
    notHandledYet(argList)
  }

  // StmtList =
  //  '{'
  //    Attr*
  //    statements:Stmt*
  //    tail_expr:Expr?
  //  '}'
  private def visitStmtList(stmtList: StmtList): Seq[Ast] = {
    val stmtAsts    = stmtList.stmt.flatMap(visitStmt)
    val tailExprAst = stmtList.expr.map(visitExpr).toList
    stmtAsts ++ tailExprAst
  }

  // ParamList =
  //  '('(
  //    SelfParam
  //  | (SelfParam ',')? (Param (',' Param)* ','?)?
  //  )')'
  // | '|' (Param (',' Param)* ','?)? '|'
  private def visitParamList(paramList: ParamList): Seq[Ast] = {
    val selfParamAst = paramList.selfParam.map(visitSelfParam).toList
    val paramAsts = paramList.param.zipWithIndex.map { case (param, paramIdx) =>
      val paramName         = param.pat.collect { case x: IdentPat => x }
      val paramTypeFullName = param.typ.map(typeFullNameForType)

      (paramName, paramTypeFullName) match {
        case (Some(name), Some(typeFullName)) =>
          val paramNode = parameterInNode(
            node = param,
            name = code(name),
            code = code(param),
            index = paramIdx + 1,
            isVariadic = false,
            evaluationStrategy = EvaluationStrategies.BY_SHARING,
            typeFullName = typeFullName
          )
          Ast(paramNode)
        case _ => notHandledYet(param)
      }
    }

    selfParamAst ++ paramAsts
  }

  // SelfParam =
  //  Attr* ( ('&' Lifetime?)? 'mut'? Name | 'mut'? Name ':' Type )
  private def visitSelfParam(selfParam: SelfParam): Ast = {
    val enclosingType = enclosingTypeDeclFullName.getOrElse(Defines.Any)
    val typeFullName = selfParam.typ match {
      case Some(typ) => typeFullNameForType(typ)
      case None if selfParam.ampToken.isDefined =>
        val mut = Option.when(selfParam.mutKwToken.isDefined)("mut ").getOrElse("")
        s"&$mut$enclosingType"
      case None => enclosingType
    }
    val evaluationStrategy =
      if (selfParam.ampToken.isDefined) EvaluationStrategies.BY_SHARING else EvaluationStrategies.BY_VALUE
    val paramNode = parameterInNode(
      node = selfParam,
      name = code(selfParam.name),
      code = code(selfParam),
      index = 0,
      isVariadic = false,
      evaluationStrategy = evaluationStrategy,
      typeFullName = typeFullName
    )
    Ast(paramNode)
  }

  // Param =
  //  Attr* (
  //    Pat (':' Type)?
  //  | Type
  //  | '...'
  //  )
  private def visitParam(param: Param): Ast = {
    notHandledYet(param)
  }

  // PathExpr =
  //  Attr* Path
  private def visitPathExpr(pathExpr: PathExpr): Ast = {
    visitPath(pathExpr.path)
  }

  // Path =
  //  (qualifier:Path '::')? segment:PathSegment
  private def visitPath(path: Path): Ast = {
    lowerPathAsFieldAccess(path)
  }

  // TODO
  private def lowerPathAsFieldAccess(path: Path): Ast = {
    val lhs = path.path.map(lowerPathAsFieldAccess)
    lhs match {
      case None      => visitPathSegment(path.pathSegment)
      case Some(lhs) => notHandledYet(path)
    }
  }

  // PathSegment =
  //  '::'? NameRef
  // | NameRef GenericArgList?
  // | NameRef ParenthesizedArgList RetType?
  // | NameRef ReturnTypeSyntax
  // | TypeAnchor
  private def visitPathSegment(pathSegment: PathSegment): Ast = {
    pathSegment.nameRef match {
      case Some(nameRef) => visitNameRef(nameRef)
      case None          => notHandledYet(pathSegment)
    }
  }

  // NameRef =
  //  '#ident' | '@int_number' | 'self' | 'super' | 'crate' | 'Self'
  private def visitNameRef(nameRef: NameRef): Ast = {
    nameRef.token match {
      case Some(ident: IdentToken) =>
        Ast(identifierNode(nameRef, code(ident), code(nameRef), typeFullNameForNameRef(nameRef)))
      case Some(selfKw: SelfKwToken) =>
        Ast(identifierNode(nameRef, code(selfKw), code(nameRef), typeFullNameForNameRef(nameRef)))
      case _ => notHandledYet(nameRef)
    }
  }

  extension (nameRef: NameRef) {
    private def token: Option[RustToken] =
      nameRef.identToken
        .orElse(nameRef.intNumberToken)
        .orElse(nameRef.selfKwToken)
        .orElse(nameRef.superKwToken)
        .orElse(nameRef.crateKwToken)
        .orElse(nameRef.selfTypeKwToken)
  }

  // BinExpr =
  //  Attr*
  //  lhs:Expr
  //  op:(
  //    '||' | '&&'
  //  | '==' | '!=' | '<=' | '>=' | '<' | '>'
  //  | '+' | '*' | '-' | '/' | '%' | '<<' | '>>' | '^' | '|' | '&'
  //  | '=' | '+=' | '/=' | '*=' | '%=' | '>>=' | '<<=' | '-=' | '|=' | '&=' | '^='
  //  )
  //  rhs:Expr
  private def visitBinExpr(binExpr: BinExpr): Ast = {
    operatorNameFor(binExpr) match {
      case Some(opName) =>
        val typeFullName = typeFullNameForExpr(binExpr)
        val callNode     = operatorCallNode(binExpr, code(binExpr), opName, Some(typeFullName))
        val lhsRhs       = binExpr.expr.map(visitExpr)
        callAst(callNode, lhsRhs)
      case None => notHandledYet(binExpr)
    }
  }

  extension (binExpr: BinExpr) {
    protected def op: Option[RustToken] =
      binExpr.pipe2Token
        .orElse(binExpr.amp2Token)
        .orElse(binExpr.eq2Token)
        .orElse(binExpr.neqToken)
        .orElse(binExpr.lteqToken)
        .orElse(binExpr.gteqToken)
        .orElse(binExpr.lAngleToken)
        .orElse(binExpr.rAngleToken)
        .orElse(binExpr.plusToken)
        .orElse(binExpr.starToken)
        .orElse(binExpr.minusToken)
        .orElse(binExpr.slashToken)
        .orElse(binExpr.percentToken)
        .orElse(binExpr.shlToken)
        .orElse(binExpr.shrToken)
        .orElse(binExpr.caretToken)
        .orElse(binExpr.pipeToken)
        .orElse(binExpr.ampToken)
        .orElse(binExpr.eqToken)
        .orElse(binExpr.pluseqToken)
        .orElse(binExpr.slasheqToken)
        .orElse(binExpr.stareqToken)
        .orElse(binExpr.percenteqToken)
        .orElse(binExpr.shreqToken)
        .orElse(binExpr.shleqToken)
        .orElse(binExpr.minuseqToken)
        .orElse(binExpr.pipeeqToken)
        .orElse(binExpr.ampeqToken)
        .orElse(binExpr.careteqToken)
  }

  // PrefixExpr =
  //  Attr* op:('-' | '!' | '*') Expr
  private def visitPrefixExpr(prefixExpr: PrefixExpr): Ast = {
    operatorNameFor(prefixExpr) match {
      case Some(opName) =>
        val typeFullName = typeFullNameForExpr(prefixExpr)
        val callNode     = operatorCallNode(prefixExpr, code(prefixExpr), opName, Some(typeFullName))
        val exprAst      = visitExpr(prefixExpr.expr)
        callAst(callNode, Seq(exprAst))
      case None => notHandledYet(prefixExpr)
    }
  }

  extension (prefixExpr: PrefixExpr) {
    protected def op: Option[RustToken] =
      prefixExpr.minusToken
        .orElse(prefixExpr.bangToken)
        .orElse(prefixExpr.starToken)
  }

  // RefExpr =
  //  Attr* '&' ('raw' 'const' | 'raw' 'mut' | 'mut')? Expr
  private def visitRefExpr(refExpr: RefExpr): Ast = {
    val typeFullName = typeFullNameForExpr(refExpr)
    val callNode     = operatorCallNode(refExpr, code(refExpr), Operators.addressOf, Some(typeFullName))
    val exprAst      = visitExpr(refExpr.expr)
    callAst(callNode, Seq(exprAst))
  }

  // IfExpr =
  //  Attr* 'if' condition:Expr then_branch:BlockExpr
  //  ('else' else_branch:(IfExpr | BlockExpr))?
  private def visitIfExpr(ifExpr: IfExpr): Ast = {
    val conditionAst = visitExpr(ifExpr.expr)
    val thenAst      = visitBlockExpr(ifExpr.thenBranch)
    val elseAst      = ifExpr.elseBranch.map(visitExpr)
    ifThenElseAst(ifExpr, Some(conditionAst), thenAst, elseAst)
  }

  extension (ifExpr: IfExpr) {
    protected def thenBranch: BlockExpr = {
      ifExpr.blockExpr.head
    }

    protected def elseBranch: Option[IfExpr | BlockExpr] = {
      if (ifExpr.ifExpr.isDefined) {
        ifExpr.ifExpr
      } else if (ifExpr.blockExpr.sizeIs > 1) {
        Some(ifExpr.blockExpr.last)
      } else
        None
    }
  }

  // CastExpr =
  //  Attr* Expr 'as' Type
  private def visitCastExpr(castExpr: CastExpr): Ast = {
    val typeFullName = typeFullNameForType(castExpr.typ)
    val castNode     = operatorCallNode(castExpr, code(castExpr), Operators.cast, Some(typeFullName))
    val typeRefAst   = Ast(typeRefNode(castExpr.typ, code(castExpr.typ), typeFullName))
    val exprAst      = visitExpr(castExpr.expr)

    callAst(castNode, Seq(typeRefAst, exprAst))
  }

  // WhileExpr =
  //  Attr* Label? 'while' condition:Expr
  //  loop_body:BlockExpr
  private def visitWhileExpr(whileExpr: WhileExpr): Ast = {
    val conditionAst = visitExpr(whileExpr.expr)
    val bodyAst      = visitBlockExpr(whileExpr.blockExpr)
    whileAst(whileExpr, Some(conditionAst), Seq(bodyAst))
  }

  // LoopExpr =
  //  Attr* Label? 'loop'
  //  loop_body:BlockExpr
  private def visitLoopExpr(loopExpr: LoopExpr): Ast = {
    val conditionAst = Ast(literalNode(loopExpr.loopKwToken, "true", "bool"))
    val bodyAst      = visitBlockExpr(loopExpr.blockExpr)
    whileAst(loopExpr, Some(conditionAst), Seq(bodyAst))
  }

  // ForExpr =
  //  Attr* Label? 'for' Pat 'in' iterable:Expr
  //  loop_body:BlockExpr
  private def visitForExpr(forExpr: ForExpr): Ast = {
    // TODO
    notHandledYet(forExpr)
    visitBlockExpr(forExpr.blockExpr)
  }

  // ContinueExpr =
  //  Attr* 'continue' Lifetime?
  private def visitContinueExpr(continueExpr: ContinueExpr): Ast = {
    continueAst(continueExpr, code(continueExpr))
  }

  // BreakExpr =
  //  Attr* 'break' Lifetime? Expr?
  private def visitBreakExpr(breakExpr: BreakExpr): Ast = {
    // TODO: break expr is meant to return expr
    breakAst(breakExpr, code(breakExpr))
  }

  // IndexExpr =
  //  Attr* base:Expr '[' index:Expr ']'
  private def visitIndexExpr(indexExpr: IndexExpr): Ast = {
    val typeFullName = typeFullNameForExpr(indexExpr)
    val callNode     = operatorCallNode(indexExpr, code(indexExpr), Operators.indexAccess, Some(typeFullName))
    val baseAst      = visitExpr(indexExpr.base)
    val indexAst     = visitExpr(indexExpr.index)
    callAst(callNode, Seq(baseAst, indexAst))
  }

  extension (indexExpr: IndexExpr) {
    protected def base: Expr  = indexExpr.expr.head
    protected def index: Expr = indexExpr.expr.last
  }

  // TupleExpr =
  //  Attr* '(' (Expr (',' Expr)* ','?)? ')'
  private def visitTupleExpr(tupleExpr: TupleExpr): Ast = {
    if (tupleExpr.expr.isEmpty) {
      Ast(literalNode(tupleExpr, code(tupleExpr), "()"))
    } else {
      val typeFullName = typeFullNameForTupleExpr(tupleExpr)
      val callNode     = operatorCallNode(tupleExpr, code(tupleExpr), RustOperators.tupleLiteral, Some(typeFullName))
      val argAsts      = tupleExpr.expr.map(visitExpr)
      callAst(callNode, argAsts)
    }
  }

  // RecordExpr =
  //  Attr* Path RecordExprFieldList
  // Lowers a record expression e.g. `Foo { x: 1, y: 2 }` into an alloc-followed-by-field-assignments-block:
  // BLOCK
  //   LOCAL tmp
  //   tmp = <operator>.alloc
  //   tmp.x = 1
  //   tmp.y = 2
  //   tmp
  // In case of spreads e.g. `Foo { x: y, ..bar }` we include the assignment `tmp = bar` for flow purposes.
  private def visitRecordExpr(recordExpr: RecordExpr): Ast = {
    // LOCAL tmp
    val structType = typeFullNameForExpr(recordExpr)
    val fieldList  = recordExpr.recordExprFieldList
    val tmpName    = "tmp"
    val local      = localNode(recordExpr, tmpName, tmpName, structType)

    // tmp = <operator>.alloc
    val allocAssignAst = {
      val tmp       = identifierNode(recordExpr, tmpName, tmpName, structType)
      val tmpAst    = Ast(tmp).withRefEdge(tmp, local) // TODO: remove once ref linker is in.
      val allocCall = operatorCallNode(recordExpr, Operators.alloc, Operators.alloc, Some(structType))
      callAst(assignmentNode(recordExpr, s"$tmpName = ${Operators.alloc}"), Seq(tmpAst, Ast(allocCall)))
    }

    // tmp = base
    val spreadAssignAst = fieldList.expr.map { base =>
      val tmp    = identifierNode(recordExpr, tmpName, tmpName, structType)
      val tmpAst = Ast(tmp).withRefEdge(tmp, local) // TODO: remove once ref linker is in.
      callAst(assignmentNode(recordExpr, s"$tmpName = ${code(base)}"), Seq(tmpAst, visitExpr(base)))
    }.toSeq

    // tmp.x = 1; tmp.y = 2; etc.
    val fieldAssignAsts = fieldList.recordExprField.map { field =>
      val fieldNameRef = field.nameRef.orElse(viewExprAsNameRef(field.expr)).get
      val fieldName    = code(fieldNameRef)
      val fieldType    = typeFullNameForExpr(field.expr)
      val tmp          = identifierNode(field, tmpName, tmpName, structType)
      val baseAst      = Ast(tmp).withRefEdge(tmp, local) // TODO: remove once ref linker is in.
      val lhs = fieldAccessAst(fieldNameRef, fieldNameRef, baseAst, s"$tmpName.$fieldName", fieldName, fieldType)
      callAst(assignmentNode(field, s"$tmpName.$fieldName = ${code(field.expr)}"), Seq(lhs, visitExpr(field.expr)))
    }

    // tmp
    val retAst = {
      val tmp = identifierNode(recordExpr, tmpName, tmpName, structType)
      Ast(tmp).withRefEdge(tmp, local) // TODO: remove once ref linker is in.
    }

    // BLOCK { ... }
    val block = blockNode(recordExpr, code(recordExpr), structType)
    Ast(block).withChildren(Seq(Ast(local), allocAssignAst) ++ spreadAssignAst ++ fieldAssignAsts ++ Seq(retAst))
  }

  // ArrayExpr =
  //  Attr* '[' (
  //    (Expr (',' Expr)* ','?)?
  //  | Expr ';' Expr
  //  )']'
  private def visitArrayExpr(arrayExpr: ArrayExpr): Ast = {
    val typeFullName = typeFullNameForExpr(arrayExpr)
    val isRepeatForm = arrayExpr.semicolonToken.isDefined
    val operator     = if (isRepeatForm) RustOperators.repeatInArray else Operators.arrayInitializer
    val callNode     = operatorCallNode(arrayExpr, code(arrayExpr), operator, Some(typeFullName))

    callAst(callNode, arrayExpr.expr.map(visitExpr))
  }

  // FieldExpr =
  //  Attr* Expr '.' NameRef
  private def visitFieldExpr(fieldExpr: FieldExpr): Ast = {
    val baseAst      = visitExpr(fieldExpr.expr)
    val typeFullName = typeFullNameForExpr(fieldExpr)
    val nameRef      = fieldExpr.nameRef
    val fieldName    = code(nameRef)
    fieldAccessAst(fieldExpr, nameRef, baseAst, code(fieldExpr), fieldName, typeFullName)
  }

  // MethodCallExpr =
  //  Attr* receiver:Expr '.' NameRef GenericArgList? ArgList
  private def visitMethodCallExpr(methodCallExpr: MethodCallExpr): Ast = {
    val methodName     = code(methodCallExpr.nameRef)
    val methodFullName = methodFullNameForMethodCallExpr(methodCallExpr)
    val typeFullName   = typeFullNameForExpr(methodCallExpr)
    val receiverType   = typeFullNameForExpr(methodCallExpr.expr)
    val dispatch = if (isTraitObject(receiverType)) DispatchTypes.DYNAMIC_DISPATCH else DispatchTypes.STATIC_DISPATCH
    val call =
      callNode(methodCallExpr, code(methodCallExpr), methodName, methodFullName, dispatch, None, Some(typeFullName))
    val receiverAst = visitExpr(methodCallExpr.expr)
    val args        = methodCallExpr.argList.expr.map(visitExpr)
    callAst(call, args, base = Some(receiverAst))
  }

  // TODO: not accounting for the Deref trait. Depending on how it gets supported, this might need to change.
  // Currently, only `dyn` are considered dynamically dispatched. We may want to extend rust_ast_gen with semantic
  // information later.
  private def isTraitObject(receiverType: String): Boolean =
    receiverType.stripPrefix("&").stripPrefix("mut ").startsWith("dyn ")

  // AwaitExpr =
  //  Attr* Expr '.' 'await'
  private def visitAwaitExpr(awaitExpr: AwaitExpr): Ast = {
    val typeFullName = typeFullNameForExpr(awaitExpr)
    val callNode     = operatorCallNode(awaitExpr, code(awaitExpr), RustOperators.await, Some(typeFullName))
    val exprAst      = visitExpr(awaitExpr.expr)
    callAst(callNode, Seq(exprAst))
  }

  // TryExpr =
  //  Attr* Expr '?'
  private def visitTryExpr(tryExpr: TryExpr): Ast = {
    val typeFullName = typeFullNameForExpr(tryExpr)
    val callNode     = operatorCallNode(tryExpr, code(tryExpr), RustOperators.tryUnwrap, Some(typeFullName))
    val exprAst      = visitExpr(tryExpr.expr)
    callAst(callNode, Seq(exprAst))
  }

  // Struct =
  //  Attr* Visibility?
  //  'struct' Name GenericParamList? (
  //    WhereClause? (RecordFieldList | ';')
  //  | TupleFieldList WhereClause? ';'
  //  )
  private def visitStruct(struct: Struct): Ast = {
    (struct.recordFieldList, struct.tupleFieldList) match {
      case (Some(recordFieldList), _) =>
        Ast(typeDeclForStruct(struct)).withChildren(visitRecordFieldList(recordFieldList))
      case (None, Some(tupleFieldList)) =>
        Ast(typeDeclForStruct(struct)).withChildren(visitTupleFieldList(tupleFieldList))
      case (None, None) =>
        Ast(typeDeclForStruct(struct))
    }
  }

  // RecordFieldList =
  // '{' fields:(RecordField (',' RecordField)* ','?)? '}'
  private def visitRecordFieldList(recordFieldList: RecordFieldList): Seq[Ast] = {
    recordFieldList.recordField.map(visitRecordField)
  }

  // RecordField =
  //  Attr* Visibility? 'unsafe'?
  //  Name ':' Type ('=' default_val:ConstArg)?
  private def visitRecordField(recordField: RecordField): Ast = {
    Ast(memberNode(recordField, code(recordField.name), code(recordField), typeFullNameForType(recordField.typ)))
  }

  // TupleFieldList =
  //  '(' fields:(TupleField (',' TupleField)* ','?)? ')'
  private def visitTupleFieldList(tupleFieldList: TupleFieldList): Seq[Ast] = {
    tupleFieldList.tupleField.zipWithIndex.map { case (tupleField, index) => visitTupleField(tupleField, index) }
  }

  // TupleField =
  //  Attr* Visibility?
  //  Type
  private def visitTupleField(tupleField: TupleField, index: Int): Ast = {
    Ast(memberNode(tupleField, index.toString, code(tupleField), typeFullNameForType(tupleField.typ)))
  }

  // Nil on purpose: we don't have a suitable CPG representation for macro declarations.
  // Instead, we rely on rust_ast_gen to expand them and we inline their expansion while visiting MacroCalls.
  private def visitMacroRules(macroRules: MacroRules): Seq[Ast] = Nil
  private def visitMacroDef(macroDef: MacroDef): Seq[Ast]       = Nil

  // MacroExpr =
  //  MacroCall
  private def visitMacroExpr(macroExpr: MacroExpr): Ast = {
    // Even though it wraps a MacroCall, in this context it should always be a single expression, i.e.
    // no MacroStmts/MacroItems. Logging just in case.
    visitMacroCall(macroExpr.macroCall) match {
      case exprAst :: Nil => exprAst
      case _              => notHandledYet(macroExpr.macroCall)
    }
  }

  // MacroCall =
  //  Attr* Path '!' TokenTree ';'?
  private def visitMacroCall(macroCall: MacroCall): Seq[Ast] = macroCall.macroExpansion match {
    case None                         => macroNotExpanded(macroCall) :: Nil
    case Some(macroItems: MacroItems) => visitMacroItems(macroItems)
    case Some(macroStmts: MacroStmts) => visitMacroStmts(macroStmts)
    case Some(expr: Expr)             => visitExpr(expr) :: Nil
    case Some(other)                  => notHandledYet(other) :: Nil
  }

  // MacroStmts =
  //  statements:Stmt*
  //  Expr?
  private def visitMacroStmts(macroStmts: MacroStmts): Seq[Ast] = {
    macroStmts.stmt.flatMap(visitStmt) ++ macroStmts.expr.map(visitExpr).toList
  }

  // MacroItems =
  //  Item*
  private def visitMacroItems(macroItems: MacroItems): Seq[Ast] = {
    macroItems.item.flatMap(visitItem)
  }

  // FormatArgsExpr =
  //  Attr* 'builtin' '#' 'format_args' '('
  //  template:Expr
  //  (',' args:(FormatArgsArg (',' FormatArgsArg)* ','?)? )?
  //  ')'
  // The result of expanding `format!("...", args...)`.
  // Lowered as formatString("...", formattedValue(arg0), ..., formattedValue(arg_n)).
  private def visitFormatArgsExpr(formatArgsExpr: FormatArgsExpr): Ast = {
    val typeFullName = typeFullNameForExpr(formatArgsExpr)
    val callNode    = operatorCallNode(formatArgsExpr, code(formatArgsExpr), Operators.formatString, Some(typeFullName))
    val templateAst = visitExpr(formatArgsExpr.expr)
    val argAsts     = formatArgsExpr.formatArgsArg.map(visitFormatArgsArg)

    callAst(callNode, templateAst +: argAsts)
  }

  // FormatArgsArg =
  //  arg_name:FormatArgsArgName? Expr
  private def visitFormatArgsArg(formatArgsArg: FormatArgsArg): Ast = {
    val typeFullName = typeFullNameForExpr(formatArgsArg.expr)
    val callNode = operatorCallNode(formatArgsArg, code(formatArgsArg), Operators.formattedValue, Some(typeFullName))
    val argAst   = visitExpr(formatArgsArg.expr)

    callAst(callNode, Seq(argAst))
  }

}
