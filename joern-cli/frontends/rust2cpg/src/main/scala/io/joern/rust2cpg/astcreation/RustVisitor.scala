package io.joern.rust2cpg.astcreation

import io.joern.rust2cpg.parser.RustNodeSyntax.*
import io.joern.x2cpg.datastructures.Stack.*
import io.joern.x2cpg.{Ast, Defines, ValidationMode}
import io.shiftleft.codepropertygraph.generated.{
  ControlStructureTypes,
  DispatchTypes,
  EvaluationStrategies,
  ModifierTypes,
  Operators
}
import io.shiftleft.codepropertygraph.generated.nodes.{NewFile, NewModifier}

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

    val namespaceBlockNode = globalNamespaceBlock()
    methodAstParentStack.push(namespaceBlockNode)

    val methodNode = globalMethodNode()
    methodAstParentStack.push(methodNode)

    val itemAsts = sourceFile.item.flatMap(visitItem)

    methodAstParentStack.pop()
    methodAstParentStack.pop()

    val globalMethodAst =
      methodAst(
        method = methodNode,
        parameters = Nil,
        body = Ast(blockNode(sourceFile)).withChildren(itemAsts),
        methodReturn = methodReturnNode(sourceFile, Defines.Any),
        modifiers = Seq(
          modifierNode(sourceFile, ModifierTypes.VIRTUAL).order(0),
          modifierNode(sourceFile, ModifierTypes.MODULE).order(1)
        )
      )

    Ast(fileNode).withChild(Ast(namespaceBlockNode).withChild(globalMethodAst))
  }

  private def visitItem(item: Item): Seq[Ast] = item match {
    case const: Const   => visitConst(const)
    case x: Enum        => notHandledYet(x) :: Nil
    case x: ExternBlock => notHandledYet(x) :: Nil
    case x: ExternCrate => notHandledYet(x) :: Nil
    case fn: Fn         => visitFn(fn) :: Nil
    case x: Impl        => notHandledYet(x) :: Nil
    case x: MacroCall   => notHandledYet(x) :: Nil
    case x: MacroRules  => notHandledYet(x) :: Nil
    case x: MacroDef    => notHandledYet(x) :: Nil
    case x: Module      => notHandledYet(x) :: Nil
    case x: Static      => notHandledYet(x) :: Nil
    case struct: Struct => visitStruct(struct) :: Nil
    case x: Trait       => notHandledYet(x) :: Nil
    case x: TypeAlias   => notHandledYet(x) :: Nil
    case x: Union       => notHandledYet(x) :: Nil
    case x: Use         => notHandledYet(x) :: Nil
    case x: AsmExpr     => notHandledYet(x) :: Nil
  }

  private def visitStmt(stmt: Stmt): Seq[Ast] = stmt match {
    case exprStmt: ExprStmt => visitExpr(exprStmt.expr) :: Nil
    case item: Item         => visitItem(item)
    case letStmt: LetStmt   => visitLetStmt(letStmt)
  }

  @tailrec
  private def visitExpr(expr: Expr): Ast = expr match {
    case x: ArrayExpr                   => notHandledYet(x)
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
    case x: FormatArgsExpr              => notHandledYet(x)
    case ifExpr: IfExpr                 => visitIfExpr(ifExpr)
    case indexExpr: IndexExpr           => visitIndexExpr(indexExpr)
    case literal: Literal               => visitLiteral(literal)
    case loopExpr: LoopExpr             => visitLoopExpr(loopExpr)
    case x: MacroExpr                   => notHandledYet(x)
    case x: MatchExpr                   => notHandledYet(x)
    case methodCallExpr: MethodCallExpr => visitMethodCallExpr(methodCallExpr)
    case x: OffsetOfExpr                => notHandledYet(x)
    case expr: ParenExpr                => visitExpr(expr.expr)
    case pathExpr: PathExpr             => visitPathExpr(pathExpr)
    case prefixExpr: PrefixExpr         => visitPrefixExpr(prefixExpr)
    case x: RangeExpr                   => notHandledYet(x)
    case x: RecordExpr                  => notHandledYet(x)
    case x: RefExpr                     => notHandledYet(x)
    case returnExpr: ReturnExpr         => visitReturnExpr(returnExpr)
    case x: BecomeExpr                  => notHandledYet(x)
    case x: TryExpr                     => notHandledYet(x)
    case x: TupleExpr                   => notHandledYet(x)
    case whileExpr: WhileExpr           => visitWhileExpr(whileExpr)
    case x: YieldExpr                   => notHandledYet(x)
    case x: YeetExpr                    => notHandledYet(x)
    case x: LetExpr                     => notHandledYet(x)
    case x: UnderscoreExpr              => notHandledYet(x)
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
        lowerIdentifierDecl(identToken, rhsExpr, typeFullName, code(const))
      case _ => notHandledYet(const) :: Nil
    }
  }

  // LetStmt =
  //  Attr* 'super'? 'let' Pat (':' Type)?
  //  '=' initializer:Expr
  //  LetElse?
  //  ';'
  private def visitLetStmt(letStmt: LetStmt): Seq[Ast] = {
    viewPatAsIdentPat(letStmt.pat).flatMap(_.name.identToken) match {
      case Some(identToken) =>
        val typeFullName = letStmt.`type`.map(typeFullNameForType).getOrElse(Defines.Any)
        lowerIdentifierDecl(identToken, letStmt.expr, typeFullName, code(letStmt))
      case None => notHandledYet(letStmt) :: Nil
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

    val lhsAst        = Ast(ident).withRefEdge(ident, local)
    val rhsAst        = visitExpr(rhsExpr)
    val localAst      = Ast(local)
    val assignmentAst = callAst(assignmentNode(lhsToken, declCode), Seq(lhsAst, rhsAst))

    Seq(localAst, assignmentAst)
  }

  private def viewPatAsIdentPat(pat: Pat): Option[IdentPat] = pat match {
    case identPat: IdentPat => Some(identPat)
    case _                  => None
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
    val retTypeFullName = fn.retType.map(_.`type`).map(typeFullNameForType).getOrElse("()")
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
        val methodFullName = methodFullNameForCallExpr(nameRefs)
        val dispatch       = DispatchTypes.STATIC_DISPATCH
        val call           = callNode(callExpr, code(callExpr), name, methodFullName, dispatch)
        val args           = callExpr.argList.expr.map(visitExpr)
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
  private def visitParamList(paramList: ParamList): Seq[Ast] = {
    paramList.param.zipWithIndex.map { case (param, paramIdx) =>
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
        val callNode = operatorCallNode(node = binExpr, code = code(binExpr), name = opName, typeFullName = None)
        val lhsRhs   = binExpr.expr.map(visitExpr)
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
        val callNode = operatorCallNode(node = prefixExpr, code = code(prefixExpr), name = opName, typeFullName = None)
        val exprAst  = visitExpr(prefixExpr.expr)
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
    val callNode = operatorCallNode(indexExpr, code(indexExpr), Operators.indexAccess, None)
    val baseAst  = visitExpr(indexExpr.base)
    val indexAst = visitExpr(indexExpr.index)
    callAst(callNode, Seq(baseAst, indexAst))
  }

  extension (indexExpr: IndexExpr) {
    protected def base: Expr  = indexExpr.expr.head
    protected def index: Expr = indexExpr.expr.last
  }

  // FieldExpr =
  //  Attr* Expr '.' NameRef
  private def visitFieldExpr(fieldExpr: FieldExpr): Ast = {
    val baseAst = visitExpr(fieldExpr.expr)
    fieldExpr.nameRef.intNumberToken match {
      case Some(indexNode) =>
        // `x.0` becomes `x[0]`
        // NB: this could change depending on how we lower `struct MyStruct(SomeType1, SomeType2, etc)`
        val literalTypeFullName = typeFullNameForLiteralToken(indexNode)
        val literalIndex        = literalNode(indexNode, code(indexNode), literalTypeFullName)
        // TODO: typeFullName
        val callNode = operatorCallNode(fieldExpr, code(fieldExpr), Operators.indexAccess, None)
        callAst(callNode, Seq(baseAst, Ast(literalIndex)))
      case None =>
        // TODO: typeFullName
        val nameRef   = fieldExpr.nameRef
        val fieldName = code(nameRef)
        fieldAccessAst(fieldExpr, nameRef, baseAst, code(fieldExpr), fieldName, Defines.Any)
    }
  }

  // MethodCallExpr =
  //  Attr* receiver:Expr '.' NameRef GenericArgList? ArgList
  private def visitMethodCallExpr(methodCallExpr: MethodCallExpr): Ast = {
    val methodName     = code(methodCallExpr.nameRef)
    val methodFullName = methodFullNameForMethodCallExpr(methodCallExpr)
    val typeFullName   = typeFullNameForMethodCallExpr(methodCallExpr)
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
    // TODO
    Ast()
  }

  // RecordFieldList =
  // '{' fields:(RecordField (',' RecordField)* ','?)? '}'
  private def visitRecordFieldList(recordFieldList: RecordFieldList): Seq[Ast] = {
    // TODO
    Nil
  }

  // RecordField =
  //  Attr* Visibility? 'unsafe'?
  //  Name ':' Type ('=' default_val:ConstArg)?
  private def visitRecordField(recordField: RecordField): Ast = {
    // TODO
    Ast()
  }

}
