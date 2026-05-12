package io.joern.rust2cpg.astcreation

import io.joern.rust2cpg.parser.RustNodeSyntax
import io.joern.rust2cpg.parser.RustNodeSyntax.*
import io.joern.x2cpg.datastructures.Stack.*
import io.joern.x2cpg.{Ast, Defines, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.{NewFile, NewNamespaceBlock, NewTypeDecl}
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, DispatchTypes, EvaluationStrategies, Operators}
import io.shiftleft.codepropertygraph.generated.{
  ControlStructureTypes,
  DispatchTypes,
  EvaluationStrategies,
  Operators,
  PropertyNames
}

import scala.util.Try
import scala.annotation.tailrec

trait RustVisitor(implicit withValidationMode: ValidationMode) { this: AstCreator =>

  // SourceFile =
  //  '#shebang'?
  //  '#frontmatter'?
  //  Attr*
  //  Item*
  protected def visitSourceFile(sourceFile: SourceFile): Ast = {
    val fileNode = NewFile().name(parseResult.filename).order(0)
    Option.unless(config.disableFileContent)(parseResult.fileContent).foreach(fileNode.content(_))

    // TODO: `name` is the rust fully qualified name. `fullName` needs to be unique - to be fixed.
    val namespaceBlockNode = NewNamespaceBlock()
      .name(namespaceFullName)
      .fullName(namespaceFullName)
      .filename(parseResult.filename)
      .order(1)

    methodAstParentStack.push(namespaceBlockNode)
    val itemAsts = sourceFile.item.flatMap(visitItem)
    methodAstParentStack.pop()

    Ast(fileNode).withChild(Ast(namespaceBlockNode).withChildren(itemAsts))
  }

  private def visitItem(item: Item): Seq[Ast] = item match {
    case const: Const             => visitConst(const)
    case enumSyntax: Enum         => notHandledYet(enumSyntax) :: Nil
    case externBlock: ExternBlock => notHandledYet(externBlock) :: Nil
    case externCrate: ExternCrate => notHandledYet(externCrate) :: Nil
    case fn: Fn                   => visitFn(fn) :: Nil
    case impl: Impl               => visitImpl(impl)
    case macroCall: MacroCall     => notHandledYet(macroCall) :: Nil
    case macroRules: MacroRules   => notHandledYet(macroRules) :: Nil
    case macroDef: MacroDef       => notHandledYet(macroDef) :: Nil
    case module: Module           => notHandledYet(module) :: Nil
    case static: Static           => notHandledYet(static) :: Nil
    case struct: Struct           => visitStruct(struct) :: Nil
    case traitSyntax: Trait       => notHandledYet(traitSyntax) :: Nil
    case typeAlias: TypeAlias     => notHandledYet(typeAlias) :: Nil
    case union: Union             => notHandledYet(union) :: Nil
    case use: Use                 => notHandledYet(use) :: Nil
    case asmExpr: AsmExpr         => notHandledYet(asmExpr) :: Nil
  }

  private def visitStmt(stmt: Stmt): Seq[Ast] = stmt match {
    case exprStmt: ExprStmt => visitExpr(exprStmt.expr) :: Nil
    case item: Item         => visitItem(item)
    case letStmt: LetStmt   => visitLetStmt(letStmt)
  }

  @tailrec
  private def visitExpr(expr: Expr): Ast = expr match {
    case arrayExpr: ArrayExpr           => notHandledYet(arrayExpr)
    case asmExpr: AsmExpr               => notHandledYet(asmExpr)
    case awaitExpr: AwaitExpr           => visitAwaitExpr(awaitExpr)
    case binExpr: BinExpr               => visitBinExpr(binExpr)
    case blockExpr: BlockExpr           => visitBlockExpr(blockExpr)
    case breakExpr: BreakExpr           => visitBreakExpr(breakExpr)
    case callExpr: CallExpr             => visitCallExpr(callExpr)
    case castExpr: CastExpr             => visitCastExpr(castExpr)
    case closureExpr: ClosureExpr       => notHandledYet(closureExpr)
    case continueExpr: ContinueExpr     => visitContinueExpr(continueExpr)
    case fieldExpr: FieldExpr           => visitFieldExpr(fieldExpr)
    case forExpr: ForExpr               => visitForExpr(forExpr)
    case formatArgsExpr: FormatArgsExpr => notHandledYet(formatArgsExpr)
    case ifExpr: IfExpr                 => visitIfExpr(ifExpr)
    case indexExpr: IndexExpr           => visitIndexExpr(indexExpr)
    case literal: Literal               => visitLiteral(literal)
    case loopExpr: LoopExpr             => visitLoopExpr(loopExpr)
    case macroExpr: MacroExpr           => notHandledYet(macroExpr)
    case matchExpr: MatchExpr           => notHandledYet(matchExpr)
    case methodCallExpr: MethodCallExpr => visitMethodCallExpr(methodCallExpr)
    case offsetOfExpr: OffsetOfExpr     => notHandledYet(offsetOfExpr)
    case expr: ParenExpr                => visitExpr(expr.expr)
    case pathExpr: PathExpr             => visitPathExpr(pathExpr)
    case prefixExpr: PrefixExpr         => visitPrefixExpr(prefixExpr)
    case rangeExpr: RangeExpr           => notHandledYet(rangeExpr)
    case recordExpr: RecordExpr         => notHandledYet(recordExpr)
    case refExpr: RefExpr               => notHandledYet(refExpr)
    case returnExpr: ReturnExpr         => visitReturnExpr(returnExpr)
    case becomeExpr: BecomeExpr         => notHandledYet(becomeExpr)
    case tryExpr: TryExpr               => notHandledYet(tryExpr)
    case tupleExpr: TupleExpr           => notHandledYet(tupleExpr)
    case whileExpr: WhileExpr           => visitWhileExpr(whileExpr)
    case yieldExpr: YieldExpr           => notHandledYet(yieldExpr)
    case yeetExpr: YeetExpr             => notHandledYet(yeetExpr)
    case letExpr: LetExpr               => notHandledYet(letExpr)
    case underscoreExpr: UnderscoreExpr => notHandledYet(underscoreExpr)
  }

  private def visitType(typ: Type): Ast = typ match {
    case array: ArrayType         => notHandledYet(array)
    case dynTrait: DynTraitType   => notHandledYet(dynTrait)
    case fnPtr: FnPtrType         => notHandledYet(fnPtr)
    case forType: ForType         => notHandledYet(forType)
    case implTrait: ImplTraitType => notHandledYet(implTrait)
    case infer: InferType         => notHandledYet(infer)
    case macroType: MacroType     => notHandledYet(macroType)
    case never: NeverType         => notHandledYet(never)
    case paren: ParenType         => notHandledYet(paren)
    case path: PathType           => notHandledYet(path)
    case ptr: PtrType             => notHandledYet(ptr)
    case ref: RefType             => notHandledYet(ref)
    case slice: SliceType         => notHandledYet(slice)
    case tuple: TupleType         => notHandledYet(tuple)
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
        val typeFullName = typeFullNameForType(const.`type`)
        lowerIdentifierDecl(identToken, Some(rhsExpr), typeFullName, code(const))
      case _ => notHandledYet(const) :: Nil
    }
  }

  // LetStmt =
  //  Attr* 'super'? 'let' Pat (':' Type)?
  //  '=' initializer:Expr
  //  LetElse?
  //  ';'
  private def visitLetStmt(letStmt: LetStmt): Seq[Ast] = letStmt.pat match {
    case identPat: IdentPat =>
      identPat.name.identToken match {
        case Some(identToken) =>
          val typeFullName = letStmt.`type`.map(typeFullNameForType).getOrElse(typeFullNameForIdentPat(identPat))
          // TODO: there's a mismatch between `rust.ungram` and the auto-generated `SyntaxNode` from rust-analyzer.
          //  The grammar says that `initializer` is mandatory, but the generated `LetStmt` sees it as optional.
          //  In this case, `let x;` is indeed valid.
          //  We should look into xtask again: it looks like all mandatory nodes are generated as optional,
          //  cf. https://github.com/rust-lang/rust-analyzer/blob/7c3fc8671f83f6e46305358b98354f0611ebb3cd/xtask/src/codegen/grammar.rs#L923
          //  and https://github.com/rust-lang/rust-analyzer/blob/7c3fc8671f83f6e46305358b98354f0611ebb3cd/crates/syntax/src/ast/generated/nodes.rs#L923
          //  If so, we should update rust_ast_gen to follow that approach.
          val maybeRhs = Try(letStmt.expr).toOption
          lowerIdentifierDecl(identToken, maybeRhs, typeFullName, code(letStmt))
        case None => notHandledYet(letStmt) :: Nil
      }
    case _ => notHandledYet(letStmt) :: Nil
  }

  // Creates:
  // - LOCAL (lhsToken) with given typeFullName
  // - CALL (assignment) for lhsToken = rhsExpr, when present
  private def lowerIdentifierDecl(
    lhsToken: IdentToken,
    rhsExpr: Option[Expr],
    typeFullName: String,
    declCode: String
  ): Seq[Ast] = {
    val lhsName  = code(lhsToken)
    val local    = localNode(lhsToken, lhsName, code(lhsToken), typeFullName)
    val localAst = Ast(local)

    rhsExpr match {
      case Some(expr) =>
        val ident         = identifierNode(lhsToken, lhsName, code(lhsToken), typeFullName)
        val lhsAst        = Ast(ident).withRefEdge(ident, local)
        val rhsAst        = visitExpr(expr)
        val assignmentAst = callAst(assignmentNode(lhsToken, declCode), Seq(lhsAst, rhsAst))
        Seq(localAst, assignmentAst)
      case None =>
        Seq(localAst)
    }
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
    // TODO: add modifiers
    lowerFn(fn, selfTypeFullName = None)
  }

  private def lowerFn(fn: Fn, selfTypeFullName: Option[String]): Ast = {
    val method          = methodNode(fn, code(fn.name))
    val retTypeFullName = fn.retType.map(_.`type`).map(typeFullNameForType).getOrElse("()")
    val methodRet       = methodReturnNode(fn, retTypeFullName)

    methodAstParentStack.push(method)
    val paramAsts = lowerParamList(fn.paramList, selfTypeFullName)
    val bodyAst   = fn.blockExpr.map(lowerFnBody).getOrElse(blockAst(blockNode(fn)))
    methodAstParentStack.pop()

    methodAst(method = method, parameters = paramAsts, body = bodyAst, methodReturn = methodRet, modifiers = Nil)
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
        callAst(call, args)
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
  private def lowerParamList(paramList: ParamList, selfTypeFullName: Option[String]): Seq[Ast] = {
    val selfAst = paramList.selfParam.zip(selfTypeFullName).map { case (selfParam, ownerType) =>
      lowerSelfParam(selfParam, ownerType)
    }
    val paramAsts = paramList.param.zipWithIndex.map { case (param, paramIdx) =>
      val paramName         = param.pat.collect { case x: IdentPat => x }
      val paramTypeFullName = param.`type`.map(typeFullNameForType)

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
    selfAst.toList ++ paramAsts
  }

  // SelfParam =
  //  Attr* (
  //    ('&' Lifetime?)? 'mut'? Name
  //  | 'mut'? Name ':' Type
  //  )
  private def lowerSelfParam(selfParam: SelfParam, ownerTypeFullName: String): Ast = {
    val evaluationStrategy =
      if (selfParam.ampToken.isDefined) EvaluationStrategies.BY_REFERENCE else EvaluationStrategies.BY_SHARING
    val typeFullName = selfParam.`type`.map(typeFullNameForType).getOrElse(ownerTypeFullName)
    Ast(
      parameterInNode(
        node = selfParam,
        name = code(selfParam.name),
        code = code(selfParam),
        index = 0,
        isVariadic = false,
        evaluationStrategy = evaluationStrategy,
        typeFullName = typeFullName
      )
    )
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
    val rhs = visitPathSegment(path.pathSegment)

    val name         = code(path.pathSegment)
    val typeFullName = typeFullNameForPath(path)

    lhs match {
      case None      => Ast(identifierNode(path.pathSegment, name, code(path), typeFullName))
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
    nameRef.identToken match {
      case Some(ident) =>
        val typeFullName = typeFullNameForNameRef(nameRef)
        val name         = code(ident)
        Ast(identifierNode(nameRef, name, code(nameRef), typeFullName))
      case None => notHandledYet(nameRef)
    }
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

  // IfExpr =
  //  Attr* 'if' condition:Expr then_branch:BlockExpr
  //  ('else' else_branch:(IfExpr | BlockExpr))?
  private def visitIfExpr(ifExpr: IfExpr): Ast = {
    val ifNode       = controlStructureNode(ifExpr, ControlStructureTypes.IF, code(ifExpr))
    val conditionAst = visitExpr(ifExpr.expr)
    val thenAst      = visitBlockExpr(ifExpr.thenBranch)
    val elseAst      = ifExpr.elseBranch.map(visitElseBranch).getOrElse(Ast())

    controlStructureAst(ifNode, Some(conditionAst), Seq(thenAst, elseAst))
  }

  private def visitElseBranch(elseBranch: IfExpr | BlockExpr): Ast = {
    val elseNode = controlStructureNode(elseBranch, ControlStructureTypes.ELSE, "else")
    val bodyAst  = visitExpr(elseBranch)
    Ast(elseNode).withChild(bodyAst)
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
    val typeFullName = typeFullNameForType(castExpr.`type`)
    val castNode     = operatorCallNode(castExpr, code(castExpr), Operators.cast, Some(typeFullName))
    val typeRefAst   = Ast(typeRefNode(castExpr.`type`, code(castExpr.`type`), typeFullName))
    val exprAst      = visitExpr(castExpr.expr)

    callAst(castNode, Seq(typeRefAst, exprAst))
  }

  // WhileExpr =
  //  Attr* Label? 'while' condition:Expr
  //  loop_body:BlockExpr
  private def visitWhileExpr(whileExpr: WhileExpr): Ast = {
    val whileNode    = controlStructureNode(whileExpr, ControlStructureTypes.WHILE, code(whileExpr))
    val conditionAst = visitExpr(whileExpr.expr)
    val bodyAst      = visitBlockExpr(whileExpr.blockExpr)

    controlStructureAst(whileNode, Some(conditionAst), Seq(bodyAst))
  }

  // LoopExpr =
  //  Attr* Label? 'loop'
  //  loop_body:BlockExpr
  private def visitLoopExpr(loopExpr: LoopExpr): Ast = {
    val loopNode     = controlStructureNode(loopExpr, ControlStructureTypes.WHILE, code(loopExpr))
    val conditionAst = Ast(literalNode(loopExpr.loopKwToken, "true", "bool"))
    val bodyAst      = visitBlockExpr(loopExpr.blockExpr)

    controlStructureAst(loopNode, Some(conditionAst), Seq(bodyAst))
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
    Ast(controlStructureNode(continueExpr, ControlStructureTypes.CONTINUE, code(continueExpr)))
  }

  // BreakExpr =
  //  Attr* 'break' Lifetime? Expr?
  private def visitBreakExpr(breakExpr: BreakExpr): Ast = {
    // TODO: break expr is meant to return expr
    val breakNode = controlStructureNode(breakExpr, ControlStructureTypes.BREAK, code(breakExpr))
    controlStructureAst(breakNode, None, Nil)
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
    // TODO dispatch should be STATIC unless the receiver type is `dyn ...`
    val call = callNode(
      methodCallExpr,
      code(methodCallExpr),
      methodName,
      methodFullName,
      DispatchTypes.STATIC_DISPATCH,
      None,
      Some(typeFullName)
    )
    val receiverAst = visitExpr(methodCallExpr.expr)
    val args        = methodCallExpr.argList.expr.map(visitExpr)
    callAst(call, args, base = Some(receiverAst))
  }

  // AwaitExpr =
  //  Attr* Expr '.' 'await'
  private def visitAwaitExpr(awaitExpr: AwaitExpr): Ast = {
    notHandledYet(awaitExpr)
    visitExpr(awaitExpr.expr)
  }

  // Struct =
  //  Attr* Visibility?
  //  'struct' Name GenericParamList? (
  //    WhereClause? (RecordFieldList | ';')
  //  | TupleFieldList WhereClause? ';'
  //  )
  private def visitStruct(struct: Struct): Ast = {
    val name     = code(struct.name)
    val fullName = typeFullNameForStruct(struct)
    val parent   = methodAstParentStack.head
    val typeDecl = typeDeclNode(
      node = struct,
      name = name,
      fullName = fullName,
      filename = parseResult.filename,
      code = code(struct),
      astParentType = parent.label,
      astParentFullName = parent.properties(PropertyNames.FullName).toString,
      inherits = Nil
    )

    methodAstParentStack.push(typeDecl)
    val memberAsts = struct.recordFieldList
      .map(visitRecordFieldList)
      .orElse(struct.tupleFieldList.map(visitTupleFieldList))
      .getOrElse(Nil)
    methodAstParentStack.pop()

    Ast(typeDecl).withChildren(memberAsts)
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
    val name         = code(recordField.name)
    val typeFullName = typeFullNameForType(recordField.`type`)
    Ast(memberNode(recordField, name, code(recordField), typeFullName))
  }

  // TupleFieldList =
  //  '(' fields:(TupleField (',' TupleField)* ','?)? ')'
  private def visitTupleFieldList(tupleFieldList: TupleFieldList): Seq[Ast] = {
    tupleFieldList.tupleField.zipWithIndex.map { case (tupleField, idx) => visitTupleField(tupleField, idx) }
  }

  // TupleField =
  //  Attr* Visibility?
  //  Type
  private def visitTupleField(tupleField: TupleField, index: Int): Ast = {
    val typeFullName = typeFullNameForType(tupleField.`type`)
    Ast(memberNode(tupleField, index.toString, code(tupleField), typeFullName))
  }

  // Impl =
  //  Attr* Visibility?
  //  'default'? 'unsafe'?
  //  'impl' GenericParamList? ('const'? '!'? trait:Type 'for')? self_ty:Type WhereClause?
  //  AssocItemList
  private def visitImpl(impl: Impl): Seq[Ast] = {
    val selfFullName = typeFullNameForImpl(impl)
    val selfName     = simpleNameFromFullName(selfFullName)

    // We compute full names based on what's in methodAstParentStack.
    // And the methods inside an `impl` block should have their "Self" type prefixed.
    // As a workaround, we push a fake TypeDecl onto methodAstParentStack only
    // as a way to store the "Self" name/fullname. Not great...
    val fakeTypeDecl = NewTypeDecl()
      .name(selfName)
      .fullName(selfFullName)

    methodAstParentStack.push(fakeTypeDecl)
    val methodAsts = impl.assocItemList.assocItems.flatMap {
      case fn: Fn => Some(lowerFn(fn, Some(selfFullName)))
      case other =>
        notHandledYet(other)
        None
    }
    methodAstParentStack.pop()
    methodAsts
  }

  // TODO (rust_ast_gen): AssocItem should be a trait. Once it is we can remove this workaround.
  extension (assocItemList: AssocItemList) {
    protected def assocItems: Seq[RustNodeSyntax.RustNode] = {
      val assocKinds = Set("FN", "CONST", "MACRO_CALL", "TYPE_ALIAS")
      assocItemList.children.collect {
        case child if assocKinds(child("nodeKind").str) => RustNodeSyntax.createRustNode(child)
      }
    }
  }

}
