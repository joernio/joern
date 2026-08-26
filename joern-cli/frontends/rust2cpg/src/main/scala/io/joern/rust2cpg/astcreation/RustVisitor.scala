package io.joern.rust2cpg.astcreation

import io.joern.rust2cpg.astcreation.RustFullNames.PathSep
import io.joern.rust2cpg.parser.RustNodeSyntax.*
import io.joern.x2cpg.datastructures.Stack.*
import io.joern.x2cpg.utils.AstPropertiesUtil.RootProperties
import io.joern.x2cpg.{Ast, Defines, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.{
  ExpressionNew,
  NewFile,
  NewImport,
  NewMethod,
  NewModifier,
  NewNamespaceBlock,
  NewTypeDecl
}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, EvaluationStrategies, ModifierTypes, Operators}
import io.joern.rust2cpg.parser.RustNodeSyntaxExtensions.*

import scala.annotation.tailrec

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

    contextStack.pushNamespace(namespaceBlockNode)
    contextStack.pushGlobalMethod(method)
    val itemAsts = sourceFile.item.flatMap(visitItem)
    contextStack.pop()
    contextStack.pop()

    val block        = blockNode(sourceFile)
    val methodReturn = methodReturnNode(sourceFile, "()")
    val modifiers    = Seq(modifierNode(sourceFile, ModifierTypes.MODULE))
    methodAst(method, parameters = Nil, body = Ast(block).withChildren(itemAsts), methodReturn, modifiers)
  }

  private def visitItem(item: Item): Seq[Ast] = item match {
    case const: Const           => visitConst(const)
    case enum_ : Enum           => visitEnum(enum_) :: Nil
    case x: ExternBlock         => notHandledYet(x) :: Nil
    case x: ExternCrate         => notHandledYet(x) :: Nil
    case fn: Fn                 => visitFn(fn) :: Nil
    case impl: Impl             => visitImpl(impl)
    case macroCall: MacroCall   => visitMacroCall(macroCall)
    case macroRules: MacroRules => visitMacroRules(macroRules)
    case macroDef: MacroDef     => visitMacroDef(macroDef)
    case module: Module         => visitModule(module)
    case static: Static         => visitStatic(static)
    case struct: Struct         => visitStruct(struct)
    case trait_ : Trait         => visitTrait(trait_) :: Nil
    case x: TypeAlias           => notHandledYet(x) :: Nil
    case x: Union               => notHandledYet(x) :: Nil
    case use: Use               => visitUse(use)
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
    case closureExpr: ClosureExpr       => visitClosureExpr(closureExpr)
    case continueExpr: ContinueExpr     => visitContinueExpr(continueExpr)
    case fieldExpr: FieldExpr           => visitFieldExpr(fieldExpr)
    case forExpr: ForExpr               => visitForExpr(forExpr)
    case formatArgsExpr: FormatArgsExpr => visitFormatArgsExpr(formatArgsExpr)
    case ifExpr: IfExpr                 => visitIfExpr(ifExpr)
    case indexExpr: IndexExpr           => visitIndexExpr(indexExpr)
    case literal: Literal               => visitLiteral(literal)
    case loopExpr: LoopExpr             => visitLoopExpr(loopExpr)
    case macroExpr: MacroExpr           => visitMacroExpr(macroExpr)
    case matchExpr: MatchExpr           => visitMatchExpr(matchExpr)
    case methodCallExpr: MethodCallExpr => visitMethodCallExpr(methodCallExpr)
    case x: OffsetOfExpr                => notHandledYet(x)
    case expr: ParenExpr                => visitExpr(expr.expr)
    case pathExpr: PathExpr             => visitPathExpr(pathExpr)
    case prefixExpr: PrefixExpr         => visitPrefixExpr(prefixExpr)
    case rangeExpr: RangeExpr           => visitRangeExpr(rangeExpr)
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

    // When we implicitly cast to a trait object, we lose the original concrete type.
    // Save it as a dynamic type hint, to help with dynamic dispatch calls.
    if (isTraitObject(typeFullName) && !isTraitObject(cast.source)) {
      castNode.dynamicTypeHintFullName(Seq(stripReference(cast.source)))
    }

    val typeRefAst = Ast(typeRefNode(expr, typeFullName, typeFullName))
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
        contextStack.pushNamespace(namespaceBlock)
        val itemAsts = itemList.item.flatMap(visitItem)
        contextStack.pop()
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
    (const.name.flatMap(_.identToken), const.underscoreToken, const.expr) match {
      case (Some(identToken), None, Some(rhsExpr)) =>
        val typeFullName = typeFullNameForType(const.typ)
        lowerIdentifierDecl(identToken, code(identToken), visitExpr(rhsExpr), typeFullName, code(const))
      case (None, Some(underscoreToken), Some(rhsExpr)) =>
        val tmpName      = contextStack.nextTmpName()
        val typeFullName = typeFullNameForType(const.typ)
        lowerIdentifierDecl(underscoreToken, tmpName, visitExpr(rhsExpr), typeFullName, code(const))
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
        lowerIdentifierDecl(identToken, code(identToken), visitExpr(rhsExpr), typeFullName, code(static))
      case _ => notHandledYet(static) :: Nil
    }
  }

  // LetStmt =
  //  Attr* 'super'? 'let' Pat (':' Type)?
  //  '=' initializer:Expr?
  //  LetElse?
  //  ';'
  private def visitLetStmt(letStmt: LetStmt): Seq[Ast] = {
    val bindings = collectPatternBindings(letStmt.pat)
    (letStmt.expr, letStmt.letElse) match {
      case (None, _)                      => createLocalsForBindings(bindings)
      case (Some(rhsExpr), Some(letElse)) => lowerLetElse(letStmt, rhsExpr, letElse, bindings)
      case (Some(rhsExpr), None)          =>
        // When there are 2+ bindings in the pattern, e.g. `let (x,y) = e`, assign the RHS to a tmp variable
        // and use that variable to create each binding, e.g. `tmp = e; x = tmp.0; y = tmp.1` .
        // If there's only 1 binding, it's a regular variable declaration, so no need hoist.
        // If there are no bindings, e.g. `let _ = foo();`, we still assign the RHS to a tmp variable.
        if (bindings.length == 1) {
          val rhsAst      = visitExpr(rhsExpr)
          val localAsts   = createLocalsForBindings(bindings)
          val assignments = createAssignmentsForPattern(letStmt.pat, () => rhsAst, Some(code(letStmt)))
          localAsts ++ assignments
        } else {
          val tmpName       = contextStack.nextTmpName()
          val rhsAst        = visitExpr(rhsExpr)
          val typeFullName  = letStmt.typ.map(typeFullNameForType).orElse(rhsAst.rootType).getOrElse(Defines.Any)
          val tmpLocalAst   = localAst(letStmt, tmpName, tmpName, typeFullName)
          val mkTmpIdentAst = () => identifierAst(letStmt, tmpName, tmpName, typeFullName)

          val tmpAssignAst =
            callAst(assignmentNode(letStmt, s"$tmpName = ${code(rhsExpr)}"), Seq(mkTmpIdentAst(), rhsAst))
          val localAsts   = createLocalsForBindings(bindings)
          val assignments = createAssignmentsForPattern(letStmt.pat, mkTmpIdentAst)

          (tmpLocalAst +: localAsts) ++ (tmpAssignAst +: assignments)
        }
    }
  }

  // `let pat = expr else { body };` becomes:
  // LOCAL tmp
  // <createLocalsForBindings(pat)>
  // tmp = expr
  // IF (UNKNOWN) {
  //  <createAssignmentsForPattern(pat, tmp)>
  // }
  // ELSE {
  //  body
  // }
  private def lowerLetElse(
    letStmt: LetStmt,
    rhsExpr: Expr,
    letElse: LetElse,
    bindings: Seq[(IdentToken, String)]
  ): Seq[Ast] = {
    val tmpName       = contextStack.nextTmpName()
    val rhsAst        = visitExpr(rhsExpr)
    val typeFullName  = letStmt.typ.map(typeFullNameForType).orElse(rhsAst.rootType).getOrElse(Defines.Any)
    val tmpLocalAst   = localAst(letStmt, tmpName, tmpName, typeFullName)
    val mkTmpIdentAst = () => identifierAst(letStmt, tmpName, tmpName, typeFullName)

    val tmpAssignAst =
      callAst(assignmentNode(letStmt, s"$tmpName = ${code(rhsExpr)}"), Seq(mkTmpIdentAst(), rhsAst))
    val elseAst      = visitBlockExpr(letElse.blockExpr)
    val localAsts    = createLocalsForBindings(bindings)
    val assignments  = createAssignmentsForPattern(letStmt.pat, mkTmpIdentAst)
    val conditionAst = Ast(unknownNode(letStmt.pat, code(letStmt.pat)))
    val thenAst      = blockAst(blockNode(letStmt.pat), assignments.toList)
    val ifAst        = ifThenElseAst(letStmt, Some(conditionAst), thenAst, Some(elseAst))

    (tmpLocalAst +: localAsts) :+ tmpAssignAst :+ ifAst
  }

  private def createLocalsForBindings(bindings: Seq[(IdentToken, String)]): Seq[Ast] = {
    bindings.map { case (identToken, typeFullName) =>
      val name = code(identToken)
      localAst(identToken, name, name, typeFullName)
    }
  }

  @tailrec
  private def createAssignmentsForPattern(
    pat: Pat,
    mkSourceAst: () => Ast,
    codeOverride: Option[String] = None
  ): Seq[Ast] = {
    pat match {
      case identPat: IdentPat             => createAssignmentsForIdentPattern(identPat, mkSourceAst, codeOverride)
      case parenPat: ParenPat             => createAssignmentsForPattern(parenPat.pat, mkSourceAst, codeOverride)
      case recordPat: RecordPat           => createAssignmentsForRecordPattern(recordPat, mkSourceAst)
      case tuplePat: TuplePat             => createAssignmentsForTuplePattern(tuplePat, mkSourceAst)
      case tupleStructPat: TupleStructPat => createAssignmentsForTupleStructPattern(tupleStructPat, mkSourceAst)
      case wildcardPat: WildcardPat       => Nil
      case literalPat: LiteralPat         => Nil
      case pathPat: PathPat               => Nil
      case refPat: RefPat                 => createAssignmentsForRefPattern(refPat, mkSourceAst)
      case orPat: OrPat                   => createAssignmentsForOrPattern(orPat, mkSourceAst)
      case _                              => notHandledYet(pat) :: Nil
    }
  }

  private def createAssignmentsForTuplePattern(tuplePat: TuplePat, mkSourceAst: () => Ast): Seq[Ast] = {
    createAssignmentsForTupleElements(tuplePat, tuplePat.pat, mkSourceAst)
  }

  private def createAssignmentsForTupleStructPattern(
    tupleStructPat: TupleStructPat,
    mkSourceAst: () => Ast
  ): Seq[Ast] = {
    createAssignmentsForTupleElements(tupleStructPat, tupleStructPat.pat, mkSourceAst)
  }

  // TODO(rust_ast_gen): how many elements the tuple has, so we know how many elements to skip after RestPat.
  private def createAssignmentsForTupleElements(pat: Pat, elements: Seq[Pat], mkSourceAst: () => Ast): Seq[Ast] = {
    val (prefix, fromRest) = elements.span(!_.isInstanceOf[RestPat])
    val assignments = prefix.zipWithIndex.flatMap { case (element, index) =>
      val fieldName = index.toString
      val fieldType = typeFullNameForPat(element)
      def mkFieldAccess(): Ast = {
        val sourceAst       = mkSourceAst()
        val fieldAccessCode = s"${sourceAst.rootCodeOrEmpty}.$fieldName"
        fieldAccessAst(element, element, sourceAst, fieldAccessCode, fieldName, fieldType)
      }
      createAssignmentsForPattern(element, mkFieldAccess)
    }
    val bindingsAfterRest = fromRest.drop(1).flatMap(collectPatternBindings)
    if (bindingsAfterRest.isEmpty) {
      assignments
    } else {
      assignments :+ notHandledYet(pat)
    }
  }

  private def createAssignmentsForRecordPattern(recordPat: RecordPat, mkSourceAst: () => Ast): Seq[Ast] = {
    recordPat.recordPatFieldList.recordPatField.flatMap { field =>
      fieldNameForRecordPatField(field) match {
        case Some(fieldName) =>
          val fieldType = typeFullNameForPat(field.pat)
          def fieldAccess(): Ast = {
            val sourceAst  = mkSourceAst()
            val accessCode = s"${sourceAst.rootCodeOrEmpty}.$fieldName"
            fieldAccessAst(field, field, sourceAst, accessCode, fieldName, fieldType)
          }
          createAssignmentsForPattern(field.pat, fieldAccess)
        case None => notHandledYet(field) :: Nil
      }
    }
  }

  private def fieldNameForRecordPatField(field: RecordPatField): Option[String] = {
    field.nameRef match {
      case Some(nameRef) => Some(code(nameRef))
      case None =>
        field.pat match {
          case identPat: IdentPat => identPat.name.identToken.map(code)
          case _                  => None
        }
    }
  }

  private def createAssignmentsForIdentPattern(
    identPat: IdentPat,
    mkSourceAst: () => Ast,
    codeOverride: Option[String] = None
  ): Seq[Ast] = {
    identPat.name.identToken match {
      case Some(identToken) =>
        val lhsName      = code(identToken)
        val typeFullName = typeFullNameForIdentPat(identPat)
        val identAst     = identifierAst(identToken, lhsName, lhsName, typeFullName)
        val hasRef       = identPat.refKwToken.isDefined
        val rhsAst = if (hasRef) {
          val sourceAst = mkSourceAst()
          val refCode   = s"&${sourceAst.rootCodeOrEmpty}"
          callAst(operatorCallNode(identPat, refCode, Operators.addressOf, Some(typeFullName)), Seq(sourceAst))
        } else {
          mkSourceAst()
        }
        val assignCode = codeOverride.getOrElse(s"$lhsName = ${rhsAst.rootCodeOrEmpty}")
        val assignAst  = callAst(assignmentNode(identToken, assignCode), Seq(identAst, rhsAst))
        assignAst +: identPat.pat.toList.flatMap(createAssignmentsForPattern(_, mkSourceAst))
      case None => notHandledYet(identPat) :: Nil
    }
  }

  private def createAssignmentsForRefPattern(refPat: RefPat, mkSourceAst: () => Ast): Seq[Ast] = {
    def mkIndirection(): Ast = {
      val sourceAst    = mkSourceAst()
      val derefCode    = s"*${sourceAst.rootCodeOrEmpty}"
      val typeFullName = typeFullNameForPat(refPat.pat)
      callAst(operatorCallNode(refPat.pat, derefCode, Operators.indirection, Some(typeFullName)), Seq(sourceAst))
    }
    createAssignmentsForPattern(refPat.pat, mkIndirection)
  }

  private def createAssignmentsForOrPattern(orPat: OrPat, mkSourceAst: () => Ast): Seq[Ast] = {
    if (collectPatternBindings(orPat).isEmpty) {
      Nil
    } else {
      val sourceAst     = mkSourceAst()
      val tmpName       = contextStack.nextTmpName()
      val typeFullName  = sourceAst.rootType.getOrElse(Defines.Any)
      val tmpLocalAst   = localAst(orPat, tmpName, tmpName, typeFullName)
      val mkTmpIdentAst = () => identifierAst(orPat, tmpName, tmpName, typeFullName)

      val tmpAssignAst =
        callAst(assignmentNode(orPat, s"$tmpName = ${sourceAst.rootCodeOrEmpty}"), Seq(mkTmpIdentAst(), sourceAst))

      def mkBranchAst(pat: Pat): Ast = {
        blockAst(blockNode(pat), createAssignmentsForPattern(pat, mkTmpIdentAst).toList)
      }

      val branchesAst = orPat.pat.foldRight(Option.empty[Ast]) { (pat, elseAst) =>
        val conditionAst = Ast(unknownNode(pat, code(pat)))
        Some(ifThenElseAst(pat, Some(conditionAst), mkBranchAst(pat), elseAst))
      }

      tmpLocalAst +: tmpAssignAst +: branchesAst.toList
    }
  }

  private def collectPatternBindings(pat: Pat): Seq[(IdentToken, String)] = pat match {
    case boxPat: BoxPat               => collectPatternBindings(boxPat.pat)
    case constBlockPat: ConstBlockPat => Nil
    case derefPat: DerefPat           => collectPatternBindings(derefPat.pat)
    case identPat: IdentPat =>
      val nameBindings = identPat.name.identToken.toList.map(ident => (ident, typeFullNameForIdentPat(identPat)))
      val patBindings  = identPat.pat.toList.flatMap(collectPatternBindings)
      nameBindings ++ patBindings
    case literalPat: LiteralPat => Nil
    case macroPat: MacroPat     => Nil // TODO: needs to see the macro expansion.
    case orPat: OrPat           => orPat.pat.headOption.map(collectPatternBindings).getOrElse(Nil)
    case parenPat: ParenPat     => collectPatternBindings(parenPat.pat)
    case pathPat: PathPat       => Nil
    case rangePat: RangePat     => Nil
    case recordPat: RecordPat =>
      recordPat.recordPatFieldList.recordPatField.flatMap(field => collectPatternBindings(field.pat))
    case refPat: RefPat                 => collectPatternBindings(refPat.pat)
    case restPat: RestPat               => Nil
    case slicePat: SlicePat             => slicePat.pat.flatMap(collectPatternBindings)
    case tuplePat: TuplePat             => tuplePat.pat.flatMap(collectPatternBindings)
    case tupleStructPat: TupleStructPat => tupleStructPat.pat.flatMap(collectPatternBindings)
    case wildcardPat: WildcardPat       => Nil
  }

  // Creates:
  // - LOCAL (lhsToken) with given typeFullName
  // - CALL (assignment) for lhsToken = rhsAst
  private def lowerIdentifierDecl(
    lhsToken: RustToken,
    lhsName: String,
    rhsAst: Ast,
    typeFullName: String,
    declCode: String
  ): Seq[Ast] = {
    val local         = localAst(lhsToken, lhsName, lhsName, typeFullName)
    val lhsAst        = identifierAst(lhsToken, lhsName, lhsName, typeFullName)
    val assignmentAst = callAst(assignmentNode(lhsToken, declCode), Seq(lhsAst, rhsAst))

    Seq(local, assignmentAst)
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

  // Fn =
  // Attr* Visibility?
  // 'default'? 'const'? 'async'? 'gen'? 'unsafe'? 'safe'? Abi?
  // 'fn' Name GenericParamList? ParamList RetType? WhereClause?
  // (body:BlockExpr | ';')
  private def visitFn(fn: Fn): Ast = {
    val name            = code(fn.name)
    val fullName        = fn.methodFullName.getOrElse(composeRustFullName(name))
    val method          = methodNode(node = fn, name = name).fullName(fullName)
    val retTypeFullName = fn.retType.map(_.typ).map(typeFullNameForType).getOrElse("()")
    val methodRet       = methodReturnNode(fn, retTypeFullName)
    val methodMods      = Seq[NewModifier]()

    contextStack.pushMethod(method)
    val (paramAsts, paramAssignmentAsts) = lowerParamList(fn.paramList)
    val bodyAst = fn.blockExpr
      .map(lowerFnBody(_, paramAssignmentAsts))
      .getOrElse(blockAst(blockNode(fn), paramAssignmentAsts.toList))
    contextStack.pop()

    methodAst(method = method, parameters = paramAsts, body = bodyAst, methodReturn = methodRet, modifiers = methodMods)
  }

  // Creates:
  // BLOCK {
  //   <preStmts>
  //   <stmts>
  //   RETURN (expr) // if (expr) exists
  // }
  private def lowerFnBody(blockExpr: BlockExpr, preStmts: Seq[Ast]): Ast = {
    val stmtAsts   = blockExpr.stmtList.stmt.flatMap(visitStmt)
    val retExprAst = blockExpr.stmtList.expr.map(lowerReturnExpr).toList
    Ast(blockNode(blockExpr)).withChildren(preStmts ++ stmtAsts ++ retExprAst)
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
      lowerImplFor(impl) :: Nil
    } else {
      lowerInherentImplAsDetachedAst(impl)
      Nil
    }
  }

  // There can be at most one `impl X for Y` (for some X, Y) in the same file/project.
  private def lowerImplFor(impl: Impl): Ast = {
    impl.typ match {
      case implTrait :: implType :: Nil =>
        val typeDecl = typeDeclForTraitImpl(impl, implTrait, implType)
        contextStack.pushTypeDecl(typeDecl)
        val methodAsts = impl.assocItemList.assocItem.collect { case fn: Fn =>
          visitFn(fn).withChild(Ast(NewModifier().modifierType(ModifierTypes.VIRTUAL)))
        }
        contextStack.pop()
        addDetachedBindingAsts(typeDecl, methodAsts, signature = typeFullNameForType(implTrait))
        Ast(typeDecl).withChildren(methodAsts)
      case _ => notHandledYet(impl)
    }
  }

  // There can be multiple `impl X` in the same file/project, so we lower their
  // methods as detached Asts. The AstLinker pass shall later create the appropriate Ast edges.
  private def lowerInherentImplAsDetachedAst(impl: Impl): Unit = {
    contextStack.pushTypeDecl(typeDeclForImpl(impl))
    val methodAsts = impl.assocItemList.assocItem.collect { case fn: Fn => visitFn(fn) }
    contextStack.pop()
    methodAsts.foreach(addDetachedAst)
  }

  // Trait =
  //  Attr* Visibility?
  //  'unsafe'? 'auto'?
  //  ImplRestriction?
  //  'trait' Name GenericParamList?
  //  (((':' TypeBoundList?)? WhereClause? AssocItemList) |
  //  ('=' TypeBoundList? WhereClause? ';'))
  private def visitTrait(trait_ : Trait): Ast = {
    val name     = code(trait_.name)
    val fullName = composeRustFullName(name)
    val typeDecl = typeDeclNode(
      node = trait_,
      name = name,
      fullName = fullName,
      filename = parseResult.filename,
      code = code(trait_),
      genericSignature = Some(fullName)
    )
    contextStack.pushTypeDecl(typeDecl)
    val methodAsts = trait_.assocItemList.toSeq.flatMap(_.assocItem).collect { case fn: Fn =>
      visitFn(fn).withChild(Ast(NewModifier().modifierType(ModifierTypes.VIRTUAL)))
    }
    contextStack.pop()
    addDetachedBindingAsts(typeDecl, methodAsts, signature = fullName)
    Ast(typeDecl).withChildren(methodAsts)
  }

  // BlockExpr =
  //  Attr* Label? (TryBlockModifier | 'unsafe' | ('async' 'move'?) | ('gen' 'move'?) | 'const') StmtList
  private def visitBlockExpr(blockExpr: BlockExpr): Ast = {
    contextStack.pushBlock()
    val stmts = visitStmtList(blockExpr.stmtList)
    contextStack.pop()
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
    callExpr.expr match {
      case pathExpr: PathExpr => lowerPathExprCall(callExpr, pathExpr)
      case _                  => notHandledYet(callExpr)
    }
  }

  private def lowerPathExprCall(callExpr: CallExpr, pathExpr: PathExpr): Ast = {
    pathExpr.path.pathSegment.nameRef match {
      case Some(nameRef) =>
        val name = code(nameRef)
        val methodFullName =
          methodFullNameForCallExpr(callExpr, viewPathAsSequenceOfNameRefs(pathExpr.path).getOrElse(Nil), name)
        val typeFullName    = typeFullNameForExpr(callExpr)
        val args            = callExpr.argList.expr.map(visitExpr)
        val hasSelfReceiver = callExpr.hasSelfReceiver.isDefined && args.nonEmpty
        val (dispatch, signature) =
          if (hasSelfReceiver && args.head.rootType.exists(isTraitObject)) {
            // TODO: avoid this trait name extraction from the methodFullName
            (DispatchTypes.DYNAMIC_DISPATCH, Some(methodFullName.split(PathSep).dropRight(1).mkString(PathSep)))
          } else { (DispatchTypes.STATIC_DISPATCH, None) }
        val call =
          callNode(callExpr, code(callExpr), name, methodFullName, dispatch, signature, Some(typeFullName))
        if (hasSelfReceiver) {
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
  private def lowerParamList(paramList: ParamList): (paramAsts: Seq[Ast], paramAssignmentAsts: Seq[Ast]) = {
    val selfParamAst = paramList.selfParam.map(visitSelfParam).toList
    val (paramAsts, paramAssignmentAsts) = paramList.param.zipWithIndex.map { case (param, paramIdx) =>
      val (paramName, paramPattern) = param.pat match {
        case Some(identPat: IdentPat) if identPat.refKwToken.isEmpty => (code(identPat.name), identPat.pat)
        case pat                                                     => (contextStack.nextTmpName(), pat)
      }
      val typeFullName = typeFullNameForParam(param)
      val paramNode = parameterInNode(
        node = param,
        name = paramName,
        code = code(param),
        index = paramIdx + 1,
        isVariadic = false,
        evaluationStrategy = EvaluationStrategies.BY_SHARING,
        typeFullName = typeFullName
      )
      contextStack.declareParameter(paramNode)

      val patternAssignmentAsts = paramPattern.toList.flatMap { pat =>
        val mkParamIdentAst = () => identifierAst(pat, paramName, paramName, typeFullName)
        createLocalsForBindings(collectPatternBindings(pat)) ++ createAssignmentsForPattern(pat, mkParamIdentAst)
      }
      (Ast(paramNode), patternAssignmentAsts)
    }.unzip

    (selfParamAst ++ paramAsts, paramAssignmentAsts.flatten)
  }

  // SelfParam =
  //  Attr* ( ('&' Lifetime?)? 'mut'? Name | 'mut'? Name ':' Type )
  private def visitSelfParam(selfParam: SelfParam): Ast = {
    val typeFullName = typeFullNameForSelfParam(selfParam)
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
    contextStack.declareParameter(paramNode)
    Ast(paramNode)
  }

  // PathExpr =
  //  Attr* Path
  private def visitPathExpr(pathExpr: PathExpr): Ast = {
    // A path has `methodFullName` if it denotes a method reference. Otherwise, it's some other identifier.
    pathExpr.methodFullName match {
      case Some(methodFullName) => Ast(methodRefNode(pathExpr, code(pathExpr), methodFullName, methodFullName))
      case None                 => visitPath(pathExpr.path)
    }
  }

  // Path =
  //  (qualifier:Path '::')? segment:PathSegment
  private def visitPath(path: Path): Ast = {
    path.path match {
      case None            => visitPathSegment(path.pathSegment)
      case Some(qualifier) => lowerQualifiedPath(path, qualifier)
    }
  }

  // TODO: `<Foo as Bar>::baz`, etc.
  private def lowerQualifiedPath(path: Path, qualifier: Path): Ast = {
    qualifier.pathSegment.nameRef.flatMap(_.typeFullName) match {
      case Some(qualifierTypeFullName) =>
        val typeRefAst = Ast(typeRefNode(qualifier, code(qualifier), qualifierTypeFullName))
        val fieldName  = code(path.pathSegment)
        fieldAccessAst(path, path.pathSegment, typeRefAst, code(path), fieldName, typeFullNameForPath(path))
      case None => notHandledYet(path)
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
        identifierAst(nameRef, code(ident), code(nameRef), typeFullNameForNameRef(nameRef))
      case Some(selfKw: SelfKwToken) =>
        identifierAst(nameRef, code(selfKw), code(nameRef), typeFullNameForNameRef(nameRef))
      case _ => notHandledYet(nameRef)
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
    ifExpr.expr match {
      case letExpr: LetExpr =>
        lowerIfLet(ifExpr, letExpr)
      case condition =>
        val conditionAst = visitExpr(condition)
        val thenAst      = visitBlockExpr(ifExpr.thenBranch)
        val elseAst      = ifExpr.elseBranch.map(visitExpr)
        ifThenElseAst(ifExpr, Some(conditionAst), thenAst, elseAst)
    }
  }

  // `if let pat = expr { body-then } else { body-else }` becomes:
  // BLOCK {
  //   LOCAL tmp
  //   tmp = expr
  //   IF (UNKNOWN) {
  //     <createLocalsForBindings(pat)>
  //     <createAssignmentsForPattern(pat, tmp)>
  //     body-then
  //   }
  //   ELSE {
  //     body-else
  //   }
  // }
  private def lowerIfLet(ifExpr: IfExpr, letExpr: LetExpr): Ast = {
    val tmpName       = contextStack.nextTmpName()
    val exprAst       = visitExpr(letExpr.expr)
    val typeFullName  = exprAst.rootType.getOrElse(Defines.Any)
    val tmpLocalAst   = localAst(letExpr, tmpName, tmpName, typeFullName)
    val mkTmpIdentAst = () => identifierAst(letExpr, tmpName, tmpName, typeFullName)
    val tmpAssignAst =
      callAst(assignmentNode(letExpr, s"$tmpName = ${code(letExpr.expr)}"), Seq(mkTmpIdentAst(), exprAst))

    contextStack.pushBlock()
    val localAsts   = createLocalsForBindings(collectPatternBindings(letExpr.pat))
    val assignments = createAssignmentsForPattern(letExpr.pat, mkTmpIdentAst)
    val bodyAsts    = visitStmtList(ifExpr.thenBranch.stmtList)
    contextStack.pop()

    val conditionAst = Ast(unknownNode(letExpr.pat, code(letExpr.pat)))
    val thenAst      = blockAst(blockNode(ifExpr.thenBranch), (localAsts ++ assignments ++ bodyAsts).toList)
    val elseAst      = ifExpr.elseBranch.map(visitExpr)
    val ifAst        = ifThenElseAst(ifExpr, Some(conditionAst), thenAst, elseAst)

    blockAst(blockNode(ifExpr), List(tmpLocalAst, tmpAssignAst, ifAst))
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
    whileExpr.expr match {
      case letExpr: LetExpr =>
        lowerWhileLet(whileExpr, letExpr)
      case condition =>
        val conditionAst = visitExpr(condition)
        val bodyAst      = visitBlockExpr(whileExpr.blockExpr)
        whileAst(whileExpr, Some(conditionAst), Seq(bodyAst))
    }
  }

  // `while let pat = expr { body }` becomes:
  // WHILE (UNKNOWN) {
  //   LOCAL tmp
  //   tmp = expr
  //   <createLocalsForBindings(pat)>
  //   <createAssignmentsForPattern(pat, tmp)>
  //   body
  // }
  private def lowerWhileLet(whileExpr: WhileExpr, letExpr: LetExpr): Ast = {
    contextStack.pushBlock()
    val tmpName       = contextStack.nextTmpName()
    val exprAst       = visitExpr(letExpr.expr)
    val typeFullName  = exprAst.rootType.getOrElse(Defines.Any)
    val tmpLocalAst   = localAst(letExpr, tmpName, tmpName, typeFullName)
    val mkTmpIdentAst = () => identifierAst(letExpr, tmpName, tmpName, typeFullName)
    val tmpAssignAst =
      callAst(assignmentNode(letExpr, s"$tmpName = ${code(letExpr.expr)}"), Seq(mkTmpIdentAst(), exprAst))

    val localAsts   = createLocalsForBindings(collectPatternBindings(letExpr.pat))
    val assignments = createAssignmentsForPattern(letExpr.pat, mkTmpIdentAst)
    val bodyAsts    = visitStmtList(whileExpr.blockExpr.stmtList)
    contextStack.pop()

    val conditionAst = Ast(unknownNode(letExpr.pat, code(letExpr.pat)))
    val bodyAst = blockAst(
      blockNode(whileExpr.blockExpr),
      (tmpLocalAst +: tmpAssignAst +: (localAsts ++ assignments ++ bodyAsts)).toList
    )
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
  //
  // Lowering `for pat in y` as follows:
  // BLOCK {
  //   LOCAL tmp
  //   tmp = y.into_iter()
  //   WHILE (UNKNOWN) {
  //     LOCAL tmp2
  //     <createLocalsForBindings(pat)>
  //     tmp2 = tmp.next()
  //     <createAssignmentsForPattern(pat, tmp2)>
  //     body
  //   }
  // }
  // NB: the condition is currently UNKNOWN. A more faithful lowering would be:
  // WHILE (TRUE) {
  //  match tmp.next() {
  //    Some(pat) => body
  //    None      => break
  //  }
  private def visitForExpr(forExpr: ForExpr): Ast = {
    val iterable     = forExpr.expr
    val tmpName      = contextStack.nextTmpName()
    val tmpLocalAst  = localAst(iterable, tmpName, tmpName, Defines.Any)
    val typeFullName = typeFullNameForPat(forExpr.pat)

    // tmp = y.into_iter()
    val intoIterAssignAst = {
      val intoIterName = "into_iter"
      val receiverAst  = visitExpr(iterable)
      val intoIterCode = s"${receiverAst.rootCodeOrEmpty}.$intoIterName()"
      val intoIterCall = callNode(
        node = iterable,
        code = intoIterCode,
        name = intoIterName,
        methodFullName = combineRustFullName(Defines.UnresolvedNamespace, intoIterName),
        dispatchType = DispatchTypes.STATIC_DISPATCH,
        signature = None,
        // TODO(rust_ast_gen): include what would be the type for this into_iter() call.
        typeFullName = Some(Defines.Any)
      )
      val tmpIdentAst = identifierAst(iterable, tmpName, tmpName, Defines.Any)
      callAst(
        assignmentNode(iterable, s"$tmpName = $intoIterCode"),
        Seq(tmpIdentAst, callAst(intoIterCall, Seq.empty, base = Some(receiverAst)))
      )
    }

    // tmp.next()
    def mkNextAst(): Ast = {
      val nextName = "next"
      val nextTypeFullName = typeFullName match {
        case Defines.Any => Defines.Any
        case other       => s"core::option::Option<$other>"
      }
      val nextCall = callNode(
        node = forExpr.pat,
        code = s"$tmpName.$nextName()",
        name = nextName,
        methodFullName = combineRustFullName(Defines.UnresolvedNamespace, nextName),
        dispatchType = DispatchTypes.STATIC_DISPATCH,
        signature = None,
        typeFullName = Some(nextTypeFullName)
      )
      callAst(nextCall, Seq.empty, base = Some(identifierAst(forExpr.pat, tmpName, tmpName, Defines.Any)))
    }

    val bindings  = collectPatternBindings(forExpr.pat)
    val localAsts = createLocalsForBindings(bindings)
    val bindingAsts = if (bindings.length == 1) {
      localAsts ++ createAssignmentsForPattern(forExpr.pat, mkNextAst)
    } else {
      val itemName       = contextStack.nextTmpName()
      val itemLocalAst   = localAst(forExpr.pat, itemName, itemName, typeFullName)
      val mkItemIdentAst = () => identifierAst(forExpr.pat, itemName, itemName, typeFullName)
      val nextAst        = mkNextAst()
      val itemAssignAst =
        callAst(assignmentNode(forExpr.pat, s"$itemName = ${nextAst.rootCodeOrEmpty}"), Seq(mkItemIdentAst(), nextAst))
      val assignments = createAssignmentsForPattern(forExpr.pat, mkItemIdentAst)

      (itemLocalAst +: localAsts) ++ (itemAssignAst +: assignments)
    }

    val stmts        = visitStmtList(forExpr.blockExpr.stmtList)
    val bodyAst      = Ast(blockNode(forExpr.blockExpr)).withChildren(bindingAsts ++ stmts)
    val conditionAst = Ast(unknownNode(forExpr, code(forExpr)))
    val whileLoopAst = whileAst(forExpr, Some(conditionAst), Seq(bodyAst))

    Ast(blockNode(forExpr)).withChildren(Seq(tmpLocalAst, intoIterAssignAst, whileLoopAst))
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
  private def visitRecordExpr(recordExpr: RecordExpr): Ast = {
    recordExpr.recordExprFieldList.expr match {
      case None       => recordCtorCallAst(recordExpr)
      case Some(base) => recordCtorCallWithSpreadAst(recordExpr, base)
    }
  }

  // Lowers a record expression e.g. `Foo { x: 1, y: 2 }` as follows:
  // BLOCK
  //   LOCAL tmp
  //   tmp = <operator>.alloc
  //   Foo::<init>(&tmp, x:1, y:2)
  //   tmp
  private def recordCtorCallAst(recordExpr: RecordExpr): Ast = {
    val structType = typeFullNameForExpr(recordExpr)
    val tmpName    = contextStack.nextTmpName()

    allocBlockAst(recordExpr, tmpName, structType) { mkTmp =>

      val initCall = callNode(
        recordExpr,
        code(recordExpr),
        Defines.ConstructorMethodName,
        combineRustFullName(structType, Defines.ConstructorMethodName),
        DispatchTypes.STATIC_DISPATCH,
        None,
        Some("()")
      )

      val addressOfTmp = {
        val addressOf = operatorCallNode(recordExpr, s"&$tmpName", Operators.addressOf, Some(s"&$structType"))
        callAst(addressOf, Seq(mkTmp(recordExpr)))
      }

      val fieldArgs = recordExpr.recordExprFieldList.recordExprField.map { field =>
        val fieldName = field.nameRef.orElse(viewExprAsNameRef(field.expr)).map(code)
        val argAst    = visitExpr(field.expr)
        argAst.root.foreach { case expr: ExpressionNew => expr.argumentName(fieldName) }
        argAst
      }
      callAst(initCall, fieldArgs, base = Some(addressOfTmp)) :: Nil
    }
  }

  // Lowers a spread record expression e.g. `Foo { x: 1, ..base }` as follows:
  // BLOCK
  //   LOCAL tmp
  //   tmp = <operator>.alloc
  //   tmp = base
  //   tmp.x = y
  //   tmp
  private def recordCtorCallWithSpreadAst(recordExpr: RecordExpr, base: Expr): Ast = {
    val structType = typeFullNameForExpr(recordExpr)
    val tmpName    = contextStack.nextTmpName()

    allocBlockAst(recordExpr, tmpName, structType) { mkTmp =>
      // tmp = base
      val spreadAssignAst = {
        val tmpAst = mkTmp(recordExpr)
        callAst(assignmentNode(recordExpr, s"$tmpName = ${code(base)}"), Seq(tmpAst, visitExpr(base)))
      }

      // tmp.x = 1; etc.
      val fieldAssignAsts = recordExpr.recordExprFieldList.recordExprField.map { field =>
        val fieldNameRef = field.nameRef.orElse(viewExprAsNameRef(field.expr)).get
        val fieldName    = code(fieldNameRef)
        val fieldType    = typeFullNameForExpr(field.expr)
        val baseAst      = mkTmp(field)
        val lhs = fieldAccessAst(fieldNameRef, fieldNameRef, baseAst, s"$tmpName.$fieldName", fieldName, fieldType)
        callAst(assignmentNode(field, s"$tmpName.$fieldName = ${code(field.expr)}"), Seq(lhs, visitExpr(field.expr)))
      }

      spreadAssignAst +: fieldAssignAsts
    }
  }

  // BLOCK
  //  LOCAL "tmp"
  //  tmp = <operator>.alloc
  //  <body>
  //  tmp
  // The extra RustNode => Ast in <body> is an helper to create tmpName identifier nodes.
  private def allocBlockAst(node: RustNode, tmpName: String, typeFullName: String)(
    body: (RustNode => Ast) => Seq[Ast]
  ): Ast = {
    contextStack.pushBlock()

    // LOCAL tmp
    val localAst_ = localAst(node, tmpName, tmpName, typeFullName)

    def mkTmpIdent(node: RustNode): Ast = {
      identifierAst(node, tmpName, tmpName, typeFullName)
    }

    // tmp = <operator>.alloc
    val allocAssignAst = {
      val tmpAst    = mkTmpIdent(node)
      val allocCall = operatorCallNode(node, Operators.alloc, Operators.alloc, Some(typeFullName))
      callAst(assignmentNode(node, s"$tmpName = ${Operators.alloc}"), Seq(tmpAst, Ast(allocCall)))
    }

    // <body>
    val bodyAst = body(mkTmpIdent)

    // tmp
    val retAst = mkTmpIdent(node)

    // BLOCK { ... }
    val block    = blockNode(node, code(node), typeFullName)
    val blockAst = Ast(block).withChildren(Seq(localAst_, allocAssignAst) ++ bodyAst ++ Seq(retAst))

    contextStack.pop()
    blockAst
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
    val receiverAst    = visitExpr(methodCallExpr.expr)
    val (dispatch, signature) = if (receiverAst.rootType.exists(isTraitObject)) {
      // TODO: avoid this trait name extraction from the methodFullName
      (DispatchTypes.DYNAMIC_DISPATCH, Some(methodFullName.split(PathSep).dropRight(1).mkString(PathSep)))
    } else { (DispatchTypes.STATIC_DISPATCH, None) }
    val call =
      callNode(
        methodCallExpr,
        code(methodCallExpr),
        methodName,
        methodFullName,
        dispatch,
        signature,
        Some(typeFullName)
      )
    val args = methodCallExpr.argList.expr.map(visitExpr)
    callAst(call, args, base = Some(receiverAst))
  }

  // Currently, only `dyn` are considered dynamically dispatched. We may want to extend rust_ast_gen with semantic
  // information later.
  private def isTraitObject(typeFullName: String): Boolean = {
    stripReference(typeFullName).startsWith("dyn ")
  }

  private def stripReference(typeFullName: String): String = {
    typeFullName.stripPrefix("&").stripPrefix("mut ")
  }

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

  // Enum =
  //  Attr* Visibility?
  //  'enum' Name GenericParamList? WhereClause?
  //  VariantList
  private def visitEnum(enum_ : Enum): Ast = {
    val implementedTraits = enum_.implementedTraits.getOrElse(Nil)
    val enumFullName      = typeFullNameForEnum(enum_)
    val inheritsFrom      = implementedTraits.map(traitFullName => s"<$enumFullName as $traitFullName>")
    val typeDecl          = typeDeclForEnum(enum_, inheritsFrom)

    contextStack.pushTypeDecl(typeDecl)
    val variantAsts = enum_.variantList.variant.flatMap(lowerVariant(_, typeDecl.fullName))
    contextStack.pop()

    Ast(typeDecl).withChildren(variantAsts)
  }

  // Variant =
  //  Attr* Visibility?
  //  Name FieldList? ('=' ConstArg)?
  private def lowerVariant(variant: Variant, enumFullName: String): Seq[Ast] = variant.fieldList match {
    case None                                   => lowerUnitVariant(variant, enumFullName) :: Nil
    case Some(recordFieldList: RecordFieldList) => lowerRecordVariant(variant, enumFullName, recordFieldList) :: Nil
    case Some(tupleFieldList: TupleFieldList)   => lowerTupleVariant(variant, enumFullName, tupleFieldList)
  }

  private def lowerUnitVariant(variant: Variant, enumFullName: String): Ast = {
    Ast(memberNode(variant, code(variant.name), code(variant), enumFullName))
  }

  private def lowerRecordVariant(variant: Variant, enumFullName: String, recordFieldList: RecordFieldList): Ast = {
    val typeDecl = typeDeclForVariant(variant, enumFullName)

    contextStack.pushTypeDecl(typeDecl)
    val ctorAst = structCtorMethodAst(variant, typeDecl, recordFieldData(recordFieldList))
    contextStack.pop()

    Ast(typeDecl).withChildren(visitRecordFieldList(recordFieldList) :+ ctorAst)
  }

  private def lowerTupleVariant(variant: Variant, enumFullName: String, tupleFieldList: TupleFieldList): Seq[Ast] = {
    val typeDecl     = typeDeclForVariant(variant, enumFullName)
    val fields       = tupleStructFieldData(tupleFieldList)
    val ctorFullName = variant.methodFullName.getOrElse(typeDecl.fullName)

    contextStack.pushTypeDecl(typeDecl)
    val ctorAst = structCtorMethodAst(variant, typeDecl, fields)
    contextStack.pop()

    val typeDeclAst    = Ast(typeDecl).withChildren(visitTupleFieldList(tupleFieldList) :+ ctorAst)
    val ctorWrapperAst = tupleStructCtorWrapperAst(variant, typeDecl, ctorFullName, enumFullName, fields)

    Seq(typeDeclAst, ctorWrapperAst)
  }

  // Struct =
  //  Attr* Visibility?
  //  'struct' Name GenericParamList? (
  //    WhereClause? (RecordFieldList | ';')
  //  | TupleFieldList WhereClause? ';'
  //  )
  private def visitStruct(struct: Struct): Seq[Ast] = {
    (struct.recordFieldList, struct.tupleFieldList) match {
      case (Some(recordFieldList), _)   => lowerRecordStruct(struct, recordFieldList) :: Nil
      case (None, Some(tupleFieldList)) => lowerTupleStruct(struct, tupleFieldList)
      case (None, None)                 => lowerUnitStruct(struct) :: Nil
    }
  }

  // `struct Foo;` becomes:
  // TYPE_DECL Foo
  //   CONSTRUCTOR <init>(&self: Foo) -> () {}
  private def lowerUnitStruct(struct: Struct): Ast = {
    val implementedTraits = struct.implementedTraits.getOrElse(Nil)
    val structFullName    = typeFullNameForStruct(struct)
    val inheritsFrom      = implementedTraits.map(traitFullName => s"<$structFullName as $traitFullName>")
    val typeDecl          = typeDeclForStruct(struct, inheritsFrom)

    contextStack.pushTypeDecl(typeDecl)
    val ctorAst = structCtorMethodAst(struct, typeDecl, Nil)
    contextStack.pop()

    Ast(typeDecl).withChild(ctorAst)
  }

  // `struct Foo { x: T, ... }` becomes:
  // TYPE_DECL Foo
  //   MEMBER x: T
  //   ...
  //   CONSTRUCTOR <init>(&self: Foo, x: T, ...) -> () {
  //     (*self).x = x
  //     ...
  //   }
  private def lowerRecordStruct(struct: Struct, recordFieldList: RecordFieldList): Ast = {
    val implementedTraits = struct.implementedTraits.getOrElse(Nil)
    val structFullName    = typeFullNameForStruct(struct)
    val inheritsFrom      = implementedTraits.map(traitFullName => s"<$structFullName as $traitFullName>")
    val typeDecl          = typeDeclForStruct(struct, inheritsFrom)

    contextStack.pushTypeDecl(typeDecl)
    val ctorAst = structCtorMethodAst(struct, typeDecl, recordFieldData(recordFieldList))
    contextStack.pop()

    Ast(typeDecl).withChildren(visitRecordFieldList(recordFieldList) :+ ctorAst)
  }

  // `struct Foo(T1, T2, ...);` becomes:
  //  TYPE_DECL Foo
  //    MEMBER 0: T1
  //    MEMBER 1: T2
  //    ...
  //    CONSTRUCTOR <init>(&self: Foo, 0: T1, 1: T2, ...)` -> () {
  //      (*self).0 = 0
  //      (*self).1 = 1
  //      ...
  //    }
  //  METHOD Foo(0: T1, 1: T2, ...) -> Foo {
  //    LOCAL tmp
  //    tmp = <operator>.alloc
  //    Foo::<init>(&tmp, 0, 1)
  //    return tmp
  //  }
  // NB: tuple struct literals, e.g. `Foo(1, 2)`, are regular calls, hence the extra method.
  private def lowerTupleStruct(struct: Struct, tupleFieldList: TupleFieldList): Seq[Ast] = {
    val implementedTraits = struct.implementedTraits.getOrElse(Nil)
    val structFullName    = typeFullNameForStruct(struct)
    val inheritsFrom      = implementedTraits.map(traitFullName => s"<$structFullName as $traitFullName>")
    val typeDecl          = typeDeclForStruct(struct, inheritsFrom)
    val fields            = tupleStructFieldData(tupleFieldList)

    contextStack.pushTypeDecl(typeDecl)
    val ctorAst = structCtorMethodAst(struct, typeDecl, fields)
    contextStack.pop()

    val typeDeclAst = Ast(typeDecl).withChildren(visitTupleFieldList(tupleFieldList) :+ ctorAst)
    val ctorWrapperAst = tupleStructCtorWrapperAst(
      struct,
      typeDecl,
      struct.methodFullName.getOrElse(composeRustFullName(typeDecl.name)),
      typeDecl.fullName,
      fields
    )

    Seq(typeDeclAst, ctorWrapperAst)
  }

  private def tupleStructCtorWrapperAst(
    node: VariantDef,
    typeDecl: NewTypeDecl,
    fullName: String,
    returnTypeFullName: String,
    fields: Seq[(node: RustNode, name: String, typ: String)]
  ): Ast = {
    val fnName = typeDecl.name
    val method = methodNode(node, fnName).code(fnName).fullName(fullName)

    contextStack.pushMethod(method)
    val tmpName = contextStack.nextTmpName()

    val fieldParams = fields.zipWithIndex.map { case (fieldData, index) =>
      parameterInNode(
        node = fieldData.node,
        name = fieldData.name,
        code = fieldData.name,
        index = index + 1,
        isVariadic = false,
        evaluationStrategy = EvaluationStrategies.BY_SHARING,
        typeFullName = fieldData.typ
      )
    }

    val local = localNode(node, tmpName, tmpName, typeDecl.fullName)

    // tmp = <operator>.alloc
    val allocAssignAst = {
      val allocCall   = operatorCallNode(node, Operators.alloc, Operators.alloc, Some(typeDecl.fullName))
      val tmpIdentAst = Ast(identifierNode(node, tmpName, tmpName, typeDecl.fullName))
      callAst(assignmentNode(node, s"$tmpName = ${Operators.alloc}"), Seq(tmpIdentAst, Ast(allocCall)))
    }

    // Foo::<init>(&tmp, 0, 1, ...)
    val initCallAst = {
      val initName = Defines.ConstructorMethodName
      val initCode = s"$fnName::$initName(${(s"&$tmpName" +: fields.map(_.name)).mkString(", ")})"
      val initCall = callNode(
        node = node,
        code = initCode,
        name = initName,
        methodFullName = combineRustFullName(typeDecl.fullName, initName),
        dispatchType = DispatchTypes.STATIC_DISPATCH,
        signature = None,
        typeFullName = Some("()")
      )
      val addressOfTmp = {
        val addressOf   = operatorCallNode(node, s"&$tmpName", Operators.addressOf, Some(s"&${typeDecl.fullName}"))
        val tmpIdentAst = Ast(identifierNode(node, tmpName, tmpName, typeDecl.fullName))
        callAst(addressOf, Seq(tmpIdentAst))
      }
      val fieldArgs = fields.map { fieldData =>
        Ast(identifierNode(fieldData.node, fieldData.name, fieldData.name, fieldData.typ))
      }

      callAst(initCall, fieldArgs, base = Some(addressOfTmp))
    }

    // return tmp
    val retAst = returnAst(
      returnNode(node, s"return $tmpName"),
      Seq(Ast(identifierNode(node, tmpName, tmpName, typeDecl.fullName)))
    )

    val bodyAst = blockAst(blockNode(node), List(Ast(local), allocAssignAst, initCallAst, retAst))
    contextStack.pop()

    methodAst(method, fieldParams.map(Ast(_)), bodyAst, methodReturnNode(node, returnTypeFullName))
  }

  private def tupleStructFieldData(tupleFieldList: TupleFieldList): Seq[(node: RustNode, name: String, typ: String)] = {
    tupleFieldList.tupleField.zipWithIndex.map { case (field, index) =>
      (node = field, name = index.toString, typ = typeFullNameForType(field.typ))
    }
  }

  private def recordFieldData(recordFieldList: RecordFieldList): Seq[(node: RustNode, name: String, typ: String)] = {
    recordFieldList.recordField.map { field =>
      (node = field, name = code(field.name), typ = typeFullNameForType(field.typ))
    }
  }

  private def structCtorMethodAst(
    node: VariantDef,
    typeDecl: NewTypeDecl,
    fields: Seq[(node: RustNode, name: String, typ: String)]
  ): Ast = {
    val selfName = "self"
    val method   = methodNode(node, Defines.ConstructorMethodName).code(Defines.ConstructorMethodName)
    val selfParam = parameterInNode(
      node = node,
      name = selfName,
      code = selfName,
      index = 0,
      isVariadic = false,
      evaluationStrategy = EvaluationStrategies.BY_SHARING,
      typeFullName = typeDecl.fullName
    )
    val fieldParams = fields.zipWithIndex.map { case (fieldData, idx) =>
      parameterInNode(
        node = fieldData.node,
        name = fieldData.name,
        code = fieldData.name,
        index = idx + 1,
        isVariadic = false,
        evaluationStrategy = EvaluationStrategies.BY_SHARING,
        typeFullName = fieldData.typ
      )
    }

    // (*self).x = x; etc.
    val fieldAssignAsts = fields.zip(fieldParams).map { case (fieldData, fieldParam) =>
      val selfAst = identifierAst(fieldData.node, selfName, selfName, s"&${typeDecl.fullName}")
      val derefSelf = callAst(
        operatorCallNode(fieldData.node, s"*$selfName", Operators.indirection, Some(typeDecl.fullName)),
        Seq(selfAst)
      )
      val lhs = fieldAccessAst(
        fieldData.node,
        fieldData.node,
        derefSelf,
        s"(*$selfName).${fieldData.name}",
        fieldData.name,
        fieldData.typ
      )
      val rhs = identifierAst(fieldData.node, fieldData.name, fieldData.name, fieldData.typ)
      callAst(assignmentNode(fieldData.node, s"(*$selfName).${fieldData.name} = ${fieldData.name}"), Seq(lhs, rhs))
    }

    val paramAsts = (selfParam +: fieldParams).map(Ast(_))
    val bodyAst   = blockAst(blockNode(node), fieldAssignAsts.toList)
    methodAst(
      method = method,
      parameters = paramAsts,
      body = bodyAst,
      methodReturn = methodReturnNode(node, "()"),
      modifiers = Seq(modifierNode(node, ModifierTypes.CONSTRUCTOR))
    )
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
    visitMacroCall(macroExpr.macroCall) match {
      case Nil            => macroNotExpanded(macroExpr.macroCall)
      case exprAst :: Nil => exprAst
      case asts => blockAst(blockNode(macroExpr, code(macroExpr), typeFullNameForExpr(macroExpr)), asts.toList)
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
    val stmtAsts = macroStmts.stmt.flatMap(visitStmt)

    // When the tail expr is itself another macro call, expand and inline it.
    val tailExprAst = macroStmts.expr.toList.flatMap {
      case macroExpr: MacroExpr => visitMacroCall(macroExpr.macroCall)
      case expr                 => visitExpr(expr) :: Nil
    }

    stmtAsts ++ tailExprAst
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

  // RangeExpr =
  //  Attr* start:Expr? op:('..' | '..=') end:Expr?
  private def visitRangeExpr(rangeExpr: RangeExpr): Ast = {
    (rangeExpr.start, rangeExpr.end, rangeExpr.isInclusive) match {
      // NB: RangeInclusive (a..=b) has private start/end fields + a `new` constructor, whereas
      // the remaining ranges have no constructor but public start/end fields. Hence the special
      // treatment here.
      case (Some(start), Some(end), true) =>
        // TODO(rust_ast_gen) matches the stdlib type, but it ideally would come from rust_ast_gen.
        val methodFullName = "core::ops::range::RangeInclusive<Idx>::new"
        val call = callNode(
          rangeExpr,
          code(rangeExpr),
          "new",
          methodFullName,
          DispatchTypes.STATIC_DISPATCH,
          None,
          Some(typeFullNameForExpr(rangeExpr))
        )
        callAst(call, Seq(visitExpr(start), visitExpr(end)))
      case (start, end, _) =>
        val rangeType = typeFullNameForExpr(rangeExpr)
        val tmpName   = contextStack.nextTmpName()
        allocBlockAst(rangeExpr, tmpName, rangeType) { mktmpIdent =>
          def mkAssign(fieldName: String, expr: Expr): Ast = {
            val fieldType = typeFullNameForExpr(expr)
            val lhs       = fieldAccessAst(expr, expr, mktmpIdent(expr), s"$tmpName.$fieldName", fieldName, fieldType)
            callAst(assignmentNode(expr, s"$tmpName.$fieldName = ${code(expr)}"), Seq(lhs, visitExpr(expr)))
          }
          (start.map(mkAssign("start", _)) ++ end.map(mkAssign("end", _))).toSeq
        }
    }
  }

  // MatchExpr =
  //  Attr* 'match' Expr MatchArmList
  //
  // `match e { pat => body, ... }` becomes:
  //
  // BLOCK {
  //  LOCAL tmp
  //  tmp = e
  //  MATCH (tmp) {
  //    JUMP_TARGET
  //    BLOCK {
  //      <createLocalsForBindings(pat)>
  //      <createAssignmentsForPattern(pat, tmp)>
  //      body
  //    }
  //    ...
  //  }
  // }
  private def visitMatchExpr(matchExpr: MatchExpr): Ast = {
    val tmpName       = contextStack.nextTmpName()
    val typeFullName  = typeFullNameForExpr(matchExpr.expr)
    val tmpLocalAst   = Ast(localNode(matchExpr.expr, tmpName, tmpName, typeFullName))
    val mkTmpIdentAst = () => Ast(identifierNode(matchExpr.expr, tmpName, tmpName, typeFullName))
    val tmpAssignAst = callAst(
      assignmentNode(matchExpr.expr, s"$tmpName = ${code(matchExpr.expr)}"),
      Seq(mkTmpIdentAst(), visitExpr(matchExpr.expr))
    )
    val armAsts      = matchExpr.matchArmList.matchArm.flatMap(lowerMatchArm(_, mkTmpIdentAst))
    val matchBodyAst = blockAst(blockNode(matchExpr.matchArmList), armAsts.toList)
    val matchExprAst = matchAst(matchExpr, Some(mkTmpIdentAst()), Seq(matchBodyAst))
    Ast(blockNode(matchExpr)).withChildren(Seq(tmpLocalAst, tmpAssignAst, matchExprAst))
  }

  private def lowerMatchArm(matchArm: MatchArm, tmpIdentAst: () => Ast): Seq[Ast] = {
    val bindingAsts = createLocalsForBindings(collectPatternBindings(matchArm.pat)) ++ createAssignmentsForPattern(
      matchArm.pat,
      tmpIdentAst
    )
    val bodyAst = matchArm.matchGuard match {
      case Some(matchGuard) =>
        val conditionAst = visitExpr(matchGuard.expr)
        ifThenElseAst(matchGuard, Some(conditionAst), visitExpr(matchArm.expr), None)
      case None =>
        visitExpr(matchArm.expr)
    }
    val caseCode = matchArm.matchGuard match {
      case Some(matchGuard) => s"${code(matchArm.pat)} ${code(matchGuard)}"
      case None             => code(matchArm.pat)
    }
    val matchArmBlock = blockAst(blockNode(matchArm), (bindingAsts :+ bodyAst).toList)
    val jumpTargetAst = Ast(jumpTargetNode(matchArm.pat, s"case $caseCode", caseCode))
    Seq(jumpTargetAst, matchArmBlock)
  }

  // ClosureExpr =
  //  Attr* ForBinder? 'const'? 'static'? 'async'? 'gen'? 'move'?  ParamList RetType?
  //  body:Expr
  private def visitClosureExpr(closureExpr: ClosureExpr): Ast = {
    val closureName = nextClosureName()
    val method      = methodNode(closureExpr, closureName)

    contextStack.pushMethod(method)
    lowerClosureExprAsDetachedAst(closureExpr, method)
    val methodRefAst = Ast(methodRefNode(closureExpr, code(closureExpr), method.fullName, method.fullName))
    contextStack.pop()

    methodRefAst
  }

  private def lowerClosureExprAsDetachedAst(closureExpr: ClosureExpr, method: NewMethod): Unit = {
    val (paramAsts, paramAssignmentAsts) = lowerParamList(closureExpr.paramList)
    val bodyAst = closureExpr.expr match {
      case blockExpr: BlockExpr => lowerFnBody(blockExpr, paramAssignmentAsts)
      case expr                 => Ast(blockNode(expr)).withChildren(paramAssignmentAsts :+ lowerReturnExpr(expr))
    }
    val retTypeFullName = closureExpr.retType match {
      case None          => typeFullNameForExpr(closureExpr.expr)
      case Some(retType) => typeFullNameForType(retType.typ)
    }
    val methodRet = methodReturnNode(closureExpr, retTypeFullName)
    val modifiers = Seq(ModifierTypes.VIRTUAL, ModifierTypes.LAMBDA).map(modifierNode(closureExpr, _))
    addDetachedAst(methodAst(method, paramAsts, bodyAst, methodRet, modifiers))
  }

  // Use =
  //  Attr* Visibility?
  //  'use' UseTree ';'
  //
  // UseTree =
  //  (Path? '::')? ('*' | UseTreeList)
  // | Path Rename?
  //
  // UseTreeList =
  //  '{' (UseTree (',' UseTree)* ','?)? '}'
  private def visitUse(use: Use): Seq[Ast] = {
    lowerUse(use, use.useTree, Nil).map(Ast(_))
  }

  private def lowerUse(use: Use, tree: UseTree, prefix: Seq[Path]): Seq[NewImport] = {
    val path = prefix ++ tree.path
    tree.useTreeList match {
      case Some(useTreeList)                => useTreeList.useTree.flatMap(lowerUse(use, _, path))
      case None if tree.starToken.isDefined => lowerWildcardImport(use, path).toList
      case None                             => lowerNamedImport(use, path, tree.rename).toList
    }
  }

  private def lowerWildcardImport(use: Use, prefix: Seq[Path]): Option[NewImport] = {
    if (prefix.isEmpty) {
      None
    } else {
      Some(mkImport(use, prefix, "*").isWildcard(true))
    }
  }

  private def lowerNamedImport(use: Use, prefix: Seq[Path], alias: Option[Rename]): Option[NewImport] = {
    prefix.lastOption.flatMap { last =>
      code(last.pathSegment) match {
        // `use a::b::{self}` means `use a::b as b`
        case "self" => lowerNamedImport(use, prefix.init ++ last.path, alias)
        case name =>
          val renamedAs = alias.flatMap(rename => rename.name.orElse(rename.underscoreToken)).map(code)
          Some(mkImport(use, prefix, renamedAs.getOrElse(name)))
      }
    }
  }

  private def mkImport(use: Use, path: Seq[Path], importedAs: String): NewImport = {
    newImportNode(code(use), path.map(code).mkString(PathSep), importedAs, use)
  }
}
