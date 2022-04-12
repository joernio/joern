package io.joern.jimple2cpg.passes

import io.joern.x2cpg.Ast.storeInDiffGraph
import io.joern.x2cpg.datastructures.Global
import io.joern.x2cpg.{Ast, AstCreatorBase}
import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import org.slf4j.LoggerFactory
import overflowdb.BatchedUpdate.DiffGraphBuilder
import soot.jimple._
import soot.tagkit.Host
import soot.{Local => _, _}

import scala.collection.mutable
import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.util.{Failure, Success, Try}

class AstCreator(filename: String, cls: SootClass, global: Global) extends AstCreatorBase(filename) {

  import AstCreator._

  private val logger         = LoggerFactory.getLogger(classOf[AstCreationPass])
  private val unitToAsts     = mutable.HashMap[soot.Unit, Seq[Ast]]()
  private val controlTargets = mutable.HashMap[Seq[Ast], soot.Unit]()

  /** Add `typeName` to a global map and return it. The map is later passed to a pass that creates TYPE nodes for each
    * key in the map.
    */
  private def registerType(typeName: String): String = {
    global.usedTypes.put(typeName, true)
    typeName
  }

  /** Entry point of AST creation. Translates a compilation unit created by JavaParser into a DiffGraph containing the
    * corresponding CPG AST.
    */
  def createAst(): DiffGraphBuilder = {
    val astRoot = astForCompilationUnit(cls)
    storeInDiffGraph(astRoot, diffGraph)
    diffGraph
  }

  /** Translate compilation unit into AST
    */
  private def astForCompilationUnit(cls: SootClass): Ast = {
    val ast = astForPackageDeclaration(cls.getPackageName)
    val namespaceBlockFullName =
      ast.root.collect { case x: NewNamespaceBlock => x.fullName }.getOrElse("none")
    ast.withChild(astForTypeDecl(cls.getType, namespaceBlockFullName))
  }

  /** Translate package declaration into AST consisting of a corresponding namespace block.
    */
  private def astForPackageDeclaration(packageDecl: String): Ast = {
    val absolutePath = new java.io.File(filename).toPath.toAbsolutePath.normalize().toString
    val name         = packageDecl.split("\\.").lastOption.getOrElse("")
    val namespaceBlock = NewNamespaceBlock()
      .name(name)
      .fullName(packageDecl)
    Ast(namespaceBlock.filename(absolutePath).order(1))
  }

  /** Creates a list of all inherited classes and implemented interfaces. If there are none then a list with a single
    * element 'java.lang.Object' is returned by default.
    */
  private def listOfSuperClasses(clazz: SootClass): List[String] = {
    val implementsTypeFullName = clazz.getInterfaces.asScala.map { (i: SootClass) =>
      registerType(i.getType.toQuotedString)
    }.toList
    val inheritsFromTypeFullName =
      if (clazz.hasSuperclass && clazz.getSuperclass.getType.toQuotedString != "java.lang.Object") {
        List(registerType(clazz.getSuperclass.getType.toQuotedString))
      } else if (implementsTypeFullName.isEmpty) {
        List(registerType("java.lang.Object"))
      } else List()

    inheritsFromTypeFullName ++ implementsTypeFullName
  }

  /** Creates the AST root for type declarations and acts as the entry point for method generation.
    */
  private def astForTypeDecl(typ: RefType, namespaceBlockFullName: String): Ast = {
    val fullName  = registerType(typ.toQuotedString)
    val shortName = typ.getSootClass.getShortJavaStyleName

    val typeDecl = NewTypeDecl()
      .name(shortName)
      .fullName(fullName)
      .order(1) // Jimple always has 1 class per file
      .filename(filename)
      .code(shortName)
      .inheritsFromTypeFullName(listOfSuperClasses(typ.getSootClass))
      .astParentType(NodeTypes.NAMESPACE_BLOCK)
      .astParentFullName(namespaceBlockFullName)
    val methodAsts = withOrder(typ.getSootClass.getMethods.asScala.toList.sortWith((x, y) => x.getName > y.getName)) {
      (m, order) =>
        astForMethod(m, typ, order)
    }

    val memberAsts = typ.getSootClass.getFields.asScala
      .filter(_.isDeclared)
      .zipWithIndex
      .map { case (v, i) =>
        astForField(v, i + methodAsts.size + 1)
      }
      .toList

    Ast(typeDecl)
      .withChildren(memberAsts)
      .withChildren(methodAsts)
  }

  private def astForField(v: SootField, order: Int): Ast = {
    val typeFullName = registerType(v.getType.toQuotedString)
    val name         = v.getName
    val code         = if (v.getDeclaration.contains("enum")) name else s"$typeFullName $name"
    Ast(
      NewMember()
        .name(name)
        .lineNumber(line(v))
        .columnNumber(column(v))
        .typeFullName(typeFullName)
        .order(order)
        .code(code)
    )
  }

  private def astForMethod(methodDeclaration: SootMethod, typeDecl: RefType, childNum: Int): Ast = {
    val methodNode = createMethodNode(methodDeclaration, typeDecl, childNum)
    val lastOrder  = 2 + methodDeclaration.getParameterCount
    try {
      if (!methodDeclaration.isConcrete) {
        Ast(methodNode)
          .withChildren(astsForModifiers(methodDeclaration))
          .withChild(astForMethodReturn(methodDeclaration))
      } else {
        val methodBody = Try(methodDeclaration.getActiveBody) match {
          case Failure(_)    => methodDeclaration.retrieveActiveBody()
          case Success(body) => body
        }
        val parameterAsts =
          Seq(createThisNode(methodDeclaration, NewMethodParameterIn())) ++ withOrder(methodBody.getParameterLocals) {
            (p, order) =>
              astForParameter(p, order, methodDeclaration)
          }
        Ast(methodNode.lineNumberEnd(methodBody.toString.split('\n').filterNot(_.isBlank).length))
          .withChildren(astsForModifiers(methodDeclaration))
          .withChildren(parameterAsts)
          .withChild(astForMethodBody(methodBody, lastOrder))
          .withChild(astForMethodReturn(methodDeclaration))
      }
    } catch {
      case e: RuntimeException =>
        logger.warn(s"Unexpected exception while parsing method body! Will stub the method ${methodNode.fullName}", e)
        Ast(methodNode)
          .withChildren(astsForModifiers(methodDeclaration))
          .withChild(astForMethodReturn(methodDeclaration))
    } finally {
      // Join all targets with CFG edges - this seems to work from what is seen on DotFiles
      controlTargets.foreach({ case (asts, units) =>
        asts.headOption match {
          case Some(value) =>
            diffGraph.addEdge(value.root.get, unitToAsts(units).last.root.get, EdgeTypes.CFG)
          case None =>
        }
      })
      // Clear these maps
      controlTargets.clear()
      unitToAsts.clear()
    }
  }

  private def getEvaluationStrategy(typ: soot.Type): String =
    typ match {
      case _: PrimType    => EvaluationStrategies.BY_VALUE
      case _: VoidType    => EvaluationStrategies.BY_VALUE
      case _: NullType    => EvaluationStrategies.BY_VALUE
      case _: RefLikeType => EvaluationStrategies.BY_REFERENCE
      case _              => EvaluationStrategies.BY_SHARING
    }

  private def astForParameter(parameter: soot.Local, childNum: Int, methodDeclaration: SootMethod): Ast = {
    val typeFullName = registerType(parameter.getType.toQuotedString)
    val parameterNode = NewMethodParameterIn()
      .name(parameter.getName)
      .code(s"$typeFullName ${parameter.getName}")
      .typeFullName(typeFullName)
      .order(childNum)
      .index(childNum)
      .lineNumber(line(methodDeclaration))
      .columnNumber(column(methodDeclaration))
      .evaluationStrategy(getEvaluationStrategy(parameter.getType))
    Ast(parameterNode)
  }

  private def astForMethodBody(body: Body, order: Int): Ast = {
    val block        = NewBlock().order(order).lineNumber(line(body)).columnNumber(column(body))
    val jimpleParams = body.getParameterLocals.asScala.toList
    // Don't let parameters also become locals (avoiding duplication)
    val jimpleLocals = body.getLocals.asScala.filterNot(jimpleParams.contains).toList
    val locals = withOrder(jimpleLocals) { case (l, order) =>
      val name         = l.getName
      val typeFullName = registerType(l.getType.toQuotedString)
      val code         = s"$typeFullName $name"
      Ast(NewLocal().name(name).code(code).typeFullName(typeFullName).order(order))
    }
    Ast(block)
      .withChildren(locals)
      .withChildren(withOrder(body.getUnits.asScala.filterNot(isIgnoredUnit)) { (x, order) =>
        astsForStatement(x, order + locals.size)
      }.flatten)
  }

  private def isIgnoredUnit(unit: soot.Unit): Boolean = {
    unit match {
      case _: IdentityStmt => true
      case _: NopStmt      => true
      case _               => false
    }
  }

  private def astsForStatement(statement: soot.Unit, order: Int): Seq[Ast] = {
    val stmt = statement match {
      case x: AssignStmt       => astsForDefinition(x, order)
      case x: InvokeStmt       => astsForExpression(x.getInvokeExpr, order, statement)
      case x: ReturnStmt       => astsForReturnNode(x, order)
      case x: ReturnVoidStmt   => astsForReturnVoidNode(x, order)
      case x: IfStmt           => astsForIfStmt(x, order)
      case x: GotoStmt         => astsForGotoStmt(x, order)
      case x: LookupSwitchStmt => astsForLookupSwitchStmt(x, order)
      case x: TableSwitchStmt  => astsForTableSwitchStmt(x, order)
      case x: ThrowStmt        => astsForThrowStmt(x, order)
      case x: MonitorStmt      => astsForMonitorStmt(x, order)
      case _: IdentityStmt     => Seq() // Identity statements redefine parameters as locals
      case _: NopStmt          => Seq() // Ignore NOP statements
      case x =>
        logger.warn(s"Unhandled soot.Unit type ${x.getClass}")
        Seq(astForUnknownStmt(x, None, order))
    }
    unitToAsts.put(statement, stmt)
    stmt
  }

  private def astForBinOpExpr(binOp: BinopExpr, order: Int, parentUnit: soot.Unit): Ast = {
    val operatorName = binOp match {
      case _: AddExpr  => Operators.addition
      case _: SubExpr  => Operators.subtraction
      case _: MulExpr  => Operators.multiplication
      case _: DivExpr  => Operators.division
      case _: RemExpr  => Operators.modulo
      case _: GeExpr   => Operators.greaterEqualsThan
      case _: GtExpr   => Operators.greaterThan
      case _: LeExpr   => Operators.lessEqualsThan
      case _: LtExpr   => Operators.lessThan
      case _: ShlExpr  => Operators.shiftLeft
      case _: ShrExpr  => Operators.logicalShiftRight
      case _: UshrExpr => Operators.arithmeticShiftRight
      case _: CmpExpr  => Operators.compare
      case _: CmpgExpr => Operators.compare
      case _: CmplExpr => Operators.compare
      case _: AndExpr  => Operators.and
      case _: OrExpr   => Operators.or
      case _: XorExpr  => Operators.xor
      case _: EqExpr   => Operators.equals
      case _           => ""
    }

    val callNode = NewCall()
      .name(operatorName)
      .methodFullName(operatorName)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .code(binOp.toString)
      .argumentIndex(order)
      .order(order)

    val args =
      astsForValue(binOp.getOp1, 1, parentUnit) ++ astsForValue(binOp.getOp2, 2, parentUnit)
    callAst(callNode, args)
  }

  private def astsForExpression(expr: Expr, order: Int, parentUnit: soot.Unit): Seq[Ast] = {
    expr match {
      case x: BinopExpr  => Seq(astForBinOpExpr(x, order, parentUnit))
      case x: InvokeExpr => Seq(astForInvokeExpr(x, order, parentUnit))
      case x: AnyNewExpr => Seq(astForNewExpr(x, order, parentUnit))
      case x: CastExpr   => Seq(astForUnaryExpr(Operators.cast, x, x.getOp, order, parentUnit))
      case x: InstanceOfExpr =>
        Seq(astForUnaryExpr(Operators.instanceOf, x, x.getOp, order, parentUnit))
      case x: LengthExpr =>
        Seq(astForUnaryExpr(Operators.lengthOf, x, x.getOp, order, parentUnit))
      case x: NegExpr => Seq(astForUnaryExpr(Operators.minus, x, x.getOp, order, parentUnit))
      case x =>
        logger.warn(s"Unhandled soot.Expr type ${x.getClass}")
        Seq()
    }
  }

  private def astsForValue(value: soot.Value, order: Int, parentUnit: soot.Unit): Seq[Ast] = {
    value match {
      case x: Expr               => astsForExpression(x, order, parentUnit)
      case x: soot.Local         => Seq(astForLocal(x, order, parentUnit))
      case x: CaughtExceptionRef => Seq(astForCaughtExceptionRef(x, order, parentUnit))
      case x: Constant           => Seq(astForConstantExpr(x, order))
      case x: FieldRef           => Seq(astForFieldRef(x, order, parentUnit))
      case x: ThisRef            => Seq(createThisNode(x))
      case x: ParameterRef       => Seq(createParameterNode(x, order))
      case x: IdentityRef        => Seq(astForIdentityRef(x, order, parentUnit))
      case x: ArrayRef           => Seq(astForArrayRef(x, order, parentUnit))
      case x =>
        logger.warn(s"Unhandled soot.Value type ${x.getClass}")
        Seq()
    }
  }

  private def astForArrayRef(arrRef: ArrayRef, order: Int, parentUnit: soot.Unit): Ast = {
    val indexAccess = NewCall()
      .name(Operators.indexAccess)
      .methodFullName(Operators.indexAccess)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .code(arrRef.toString())
      .order(order)
      .argumentIndex(order)
      .lineNumber(line(parentUnit))
      .columnNumber(column(parentUnit))
      .typeFullName(registerType(arrRef.getType.toQuotedString))

    val astChildren = astsForValue(arrRef.getBase, 1, parentUnit) ++ astsForValue(arrRef.getIndex, 2, parentUnit)
    Ast(indexAccess)
      .withChildren(astChildren)
      .withArgEdges(indexAccess, astChildren.flatMap(_.root))
  }

  private def astForLocal(local: soot.Local, order: Int, parentUnit: soot.Unit): Ast = {
    val name         = local.getName
    val typeFullName = registerType(local.getType.toQuotedString)
    Ast(
      NewIdentifier()
        .name(name)
        .lineNumber(line(parentUnit))
        .columnNumber(column(parentUnit))
        .order(order)
        .argumentIndex(order)
        .code(name)
        .typeFullName(typeFullName)
    )
  }

  private def astForIdentityRef(x: IdentityRef, order: Int, parentUnit: soot.Unit): Ast = {
    Ast(
      NewIdentifier()
        .code(x.toString())
        .name(x.toString())
        .order(order)
        .argumentIndex(order)
        .typeFullName(registerType(x.getType.toQuotedString))
        .lineNumber(line(parentUnit))
        .columnNumber(column(parentUnit))
    )
  }

  private def astForInvokeExpr(invokeExpr: InvokeExpr, order: Int, parentUnit: soot.Unit): Ast = {
    val callee = invokeExpr.getMethodRef
    val dispatchType = invokeExpr match {
      case _ if callee.isConstructor => DispatchTypes.STATIC_DISPATCH
      case _: DynamicInvokeExpr      => DispatchTypes.DYNAMIC_DISPATCH
      case _: InstanceInvokeExpr     => DispatchTypes.DYNAMIC_DISPATCH
      case _                         => DispatchTypes.STATIC_DISPATCH
    }

    val signature =
      s"${registerType(callee.getReturnType.toQuotedString)}(${(for (i <- 0 until callee.getParameterTypes.size())
          yield registerType(callee.getParameterType(i).toQuotedString)).mkString(",")})"
    val thisAsts = invokeExpr match {
      case expr: InstanceInvokeExpr => astsForValue(expr.getBase, 0, parentUnit)
      case _                        => Seq(createThisNode(callee, NewIdentifier()))
    }

    val methodName =
      if (callee.isConstructor)
        registerType(callee.getDeclaringClass.getType.getClassName)
      else
        callee.getName

    val calleeType = registerType(callee.getDeclaringClass.getType.toQuotedString)
    val callType =
      if (callee.isConstructor) "void"
      else calleeType

    val code = invokeExpr match {
      case expr: InstanceInvokeExpr =>
        s"${expr.getBase}.$methodName(${invokeExpr.getArgs.asScala.mkString(", ")})"
      case _ => s"$methodName(${invokeExpr.getArgs.asScala.mkString(", ")})"
    }

    val callNode = NewCall()
      .name(callee.getName)
      .code(code)
      .dispatchType(dispatchType)
      .order(order)
      .argumentIndex(order)
      .methodFullName(s"$calleeType.${callee.getName}:$signature")
      .signature(signature)
      .typeFullName(callType)
      .lineNumber(line(parentUnit))
      .columnNumber(column(parentUnit))

    val argAsts = withOrder(invokeExpr match {
      case x: DynamicInvokeExpr => x.getArgs.asScala ++ x.getBootstrapArgs.asScala
      case x                    => x.getArgs.asScala
    }) { case (arg, order) =>
      astsForValue(arg, order, parentUnit)
    }.flatten

    val callAst = Ast(callNode)
      .withChildren(thisAsts)
      .withChildren(argAsts)
      .withArgEdges(callNode, thisAsts.flatMap(_.root))
      .withArgEdges(callNode, argAsts.flatMap(_.root))

    thisAsts.flatMap(_.root).headOption match {
      case Some(thisAst) => callAst.withReceiverEdge(callNode, thisAst)
      case None          => callAst
    }
  }

  private def astForNewExpr(x: AnyNewExpr, order: Int, parentUnit: soot.Unit): Ast = {
    x match {
      case u: NewArrayExpr =>
        astForArrayCreateExpr(x, List(u.getSize), order, parentUnit)
      case u: NewMultiArrayExpr =>
        astForArrayCreateExpr(x, u.getSizes.asScala, order, parentUnit)
      case _ =>
        val parentType = registerType(x.getType.toQuotedString)
        Ast(
          NewCall()
            .name(Operators.alloc)
            .methodFullName(Operators.alloc)
            .typeFullName(parentType)
            .code(s"new ${x.getType}")
            .dispatchType(DispatchTypes.STATIC_DISPATCH)
            .order(order)
            .argumentIndex(order)
            .lineNumber(line(parentUnit))
            .columnNumber(column(parentUnit))
        )
    }
  }

  private def astForArrayCreateExpr(
    arrayInitExpr: Expr,
    sizes: Iterable[Value],
    order: Int,
    parentUnit: soot.Unit
  ): Ast = {
    // Jimple does not have Operators.arrayInitializer
    // to enforce 3 address code form
    val arrayBaseType = registerType(arrayInitExpr.getType.toQuotedString)
    val code = s"new ${arrayBaseType.substring(0, arrayBaseType.indexOf('['))}${sizes.map(s => s"[$s]").mkString}"
    val callBlock = NewCall()
      .name(Operators.alloc)
      .methodFullName(Operators.alloc)
      .code(code)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .order(order)
      .typeFullName(arrayBaseType)
      .argumentIndex(order)
      .lineNumber(line(parentUnit))
      .columnNumber(column(parentUnit))
    val valueAsts = withOrder(sizes) { (s, o) =>
      astsForValue(s, o, parentUnit)
    }.flatten
    Ast(callBlock)
      .withChildren(valueAsts)
      .withArgEdges(callBlock, valueAsts.flatMap(_.root))
  }

  private def astForUnaryExpr(
    methodName: String,
    unaryExpr: Expr,
    op: Value,
    order: Int,
    parentUnit: soot.Unit
  ): Ast = {
    val callBlock = NewCall()
      .name(methodName)
      .methodFullName(methodName)
      .code(unaryExpr.toString())
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .order(order)
      .typeFullName(registerType(unaryExpr.getType.toQuotedString))
      .argumentIndex(order)
      .lineNumber(line(parentUnit))
      .columnNumber(column(parentUnit))

    def astForTypeRef(t: String, order: Int) = {
      Seq(
        Ast(
          NewTypeRef()
            .code(if (t.contains('.')) t.substring(t.lastIndexOf('.') + 1, t.length) else t)
            .order(order)
            .argumentIndex(order)
            .lineNumber(line(parentUnit))
            .columnNumber(column(parentUnit))
            .typeFullName(t)
        )
      )
    }

    val valueAsts = unaryExpr match {
      case instanceOfExpr: InstanceOfExpr =>
        val t = registerType(instanceOfExpr.getCheckType.toQuotedString)
        astsForValue(op, 1, parentUnit) ++ astForTypeRef(t, 2)
      case castExpr: CastExpr =>
        val t = registerType(castExpr.getCastType.toQuotedString)
        astForTypeRef(t, 1) ++ astsForValue(op, 2, parentUnit)
      case _ => astsForValue(op, 1, parentUnit)
    }

    Ast(callBlock)
      .withChildren(valueAsts)
      .withArgEdges(callBlock, valueAsts.flatMap(_.root))
  }

  private def createThisNode(method: ThisRef): Ast = {
    Ast(
      NewIdentifier()
        .name("this")
        .code("this")
        .typeFullName(registerType(method.getType.toQuotedString))
        .dynamicTypeHintFullName(Seq(registerType(method.getType.toQuotedString)))
        .order(0)
        .argumentIndex(0)
    )
  }

  private def createThisNode(method: SootMethod, builder: NewNode): Ast = createThisNode(method.makeRef(), builder)

  private def createThisNode(method: SootMethodRef, builder: NewNode): Ast = {
    if (!method.isStatic || method.isConstructor) {
      val parentType = registerType(method.getDeclaringClass.getType.toQuotedString)
      Ast(builder match {
        case x: NewIdentifier =>
          x.name("this")
            .code("this")
            .typeFullName(parentType)
            .order(0)
            .argumentIndex(0)
            .dynamicTypeHintFullName(Seq(parentType))
        case x: NewMethodParameterIn =>
          x.name("this")
            .code("this")
            .lineNumber(line(method.tryResolve()))
            .typeFullName(parentType)
            .order(0)
            .evaluationStrategy(EvaluationStrategies.BY_SHARING)
            .dynamicTypeHintFullName(Seq(parentType))
        case x => x
      })
    } else {
      Ast()
    }
  }

  private def createParameterNode(parameterRef: ParameterRef, order: Int): Ast = {
    val name = s"@parameter${parameterRef.getIndex}"
    Ast(
      NewIdentifier()
        .name(name)
        .code(name)
        .typeFullName(registerType(parameterRef.getType.toQuotedString))
        .order(order)
        .argumentIndex(order)
    )
  }

  /** Creates the AST for assignment statements keeping in mind Jimple is a 3-address code language.
    */
  private def astsForDefinition(assignStmt: DefinitionStmt, order: Int): Seq[Ast] = {
    val initializer = assignStmt.getRightOp
    val leftOp      = assignStmt.getLeftOp

    val identifier = leftOp match {
      case x: soot.Local => Seq(astForLocal(x, 1, assignStmt))
      case x: FieldRef   => Seq(astForFieldRef(x, 1, assignStmt))
      case x             => astsForValue(x, 1, assignStmt)
    }
    val lhsCode = identifier.flatMap(_.root).flatMap(_.properties.get(PropertyNames.CODE)).mkString

    val initAsts = astsForValue(initializer, 2, assignStmt)
    val rhsCode = initAsts
      .flatMap(_.root)
      .map(_.properties.getOrElse(PropertyNames.CODE, ""))
      .mkString(", ")

    val assignment = NewCall()
      .name(Operators.assignment)
      .methodFullName(Operators.assignment)
      .code(s"$lhsCode = $rhsCode")
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .order(order)
      .argumentIndex(order)
      .typeFullName(registerType(assignStmt.getLeftOp.getType.toQuotedString))
    val initializerAst = Seq(callAst(assignment, identifier ++ initAsts))
    initializerAst.toList
  }

  private def astsForIfStmt(ifStmt: IfStmt, order: Int): Seq[Ast] = {
    // bytecode/jimple ASTs are flat so there will not be nested bodies
    val condition = astsForValue(ifStmt.getCondition, order, ifStmt)
    controlTargets.put(condition, ifStmt.getTarget)
    condition
  }

  private def astsForGotoStmt(gotoStmt: GotoStmt, order: Int): Seq[Ast] = {
    // bytecode/jimple ASTs are flat so there will not be nested bodies
    val gotoAst = Seq(
      Ast(
        NewUnknown()
          .code(s"goto ${line(gotoStmt.getTarget).getOrElse(gotoStmt.getTarget.toString())}")
          .order(order)
          .argumentIndex(order)
          .lineNumber(line(gotoStmt))
          .columnNumber(column(gotoStmt))
      )
    )
    controlTargets.put(gotoAst, gotoStmt.getTarget)
    gotoAst
  }

  private def astForSwitchWithDefaultAndCondition(switchStmt: SwitchStmt, order: Int): Ast = {
    val jimple    = switchStmt.toString()
    val totalTgts = switchStmt.getTargets.size()
    val switch = NewControlStructure()
      .controlStructureType(ControlStructureTypes.SWITCH)
      .code(jimple.substring(0, jimple.indexOf("{") - 1))
      .lineNumber(line(switchStmt))
      .columnNumber(column(switchStmt))
      .order(order)
      .argumentIndex(order)

    val conditionalAst = astsForValue(switchStmt.getKey, totalTgts + 1, switchStmt)
    val defaultAst = Seq(
      Ast(
        NewJumpTarget()
          .name("default")
          .code("default:")
          .order(totalTgts + 2)
          .argumentIndex(totalTgts + 2)
          .lineNumber(line(switchStmt.getDefaultTarget))
          .columnNumber(column(switchStmt.getDefaultTarget))
      )
    )
    Ast(switch)
      .withConditionEdge(switch, conditionalAst.flatMap(_.root).head)
      .withChildren(conditionalAst ++ defaultAst)
  }

  private def astsForLookupSwitchStmt(lookupSwitchStmt: LookupSwitchStmt, order: Int): Seq[Ast] = {
    val totalTgts = lookupSwitchStmt.getTargets.size()
    val switchAst = astForSwitchWithDefaultAndCondition(lookupSwitchStmt, order)

    val tgts = for {
      i <- 0 until totalTgts
      if lookupSwitchStmt.getTarget(i) != lookupSwitchStmt.getDefaultTarget
    } yield (lookupSwitchStmt.getLookupValue(i), lookupSwitchStmt.getTarget(i))
    val tgtAsts = tgts.map { case (lookup, target) =>
      Ast(
        NewJumpTarget()
          .name(s"case $lookup")
          .code(s"case $lookup:")
          .argumentIndex(lookup)
          .order(lookup)
          .lineNumber(line(target))
          .columnNumber(column(target))
      )
    }

    Seq(
      switchAst
        .withChildren(tgtAsts)
    )
  }

  private def astsForTableSwitchStmt(tableSwitchStmt: SwitchStmt, order: Int): Seq[Ast] = {
    val switchAst = astForSwitchWithDefaultAndCondition(tableSwitchStmt, order)
    val tgtAsts = tableSwitchStmt.getTargets.asScala
      .filter(x => tableSwitchStmt.getDefaultTarget != x)
      .zipWithIndex
      .map({ case (tgt, i) =>
        Ast(
          NewJumpTarget()
            .name(s"case $i")
            .code(s"case $i:")
            .argumentIndex(i)
            .order(i)
            .lineNumber(line(tgt))
            .columnNumber(column(tgt))
        )
      })
      .toSeq

    Seq(
      switchAst
        .withChildren(tgtAsts)
    )
  }

  private def astsForThrowStmt(throwStmt: ThrowStmt, order: Int): Seq[Ast] = {
    val opAst = astsForValue(throwStmt.getOp, 1, throwStmt)
    val throwNode = NewCall()
      .name("<operator>.throw")
      .methodFullName("<operator>.throw")
      .lineNumber(line(throwStmt))
      .columnNumber(column(throwStmt))
      .code(s"throw new ${throwStmt.getOp.getType}()")
      .order(order)
      .argumentIndex(order)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
    Seq(
      Ast(throwNode)
        .withChildren(opAst)
    )
  }

  private def astsForMonitorStmt(monitorStmt: MonitorStmt, order: Int): Seq[Ast] = {
    val opAst      = astsForValue(monitorStmt.getOp, 1, monitorStmt)
    val typeString = opAst.flatMap(_.root).map(_.properties(PropertyNames.CODE)).mkString
    val code = monitorStmt match {
      case _: EnterMonitorStmt => s"entermonitor $typeString"
      case _: ExitMonitorStmt  => s"exitmonitor $typeString"
      case _                   => s"<unknown>monitor $typeString"
    }
    Seq(
      Ast(
        NewUnknown()
          .order(order)
          .argumentIndex(order)
          .code(code)
          .lineNumber(line(monitorStmt))
          .columnNumber(column(monitorStmt))
      ).withChildren(opAst)
    )
  }

  private def astForUnknownStmt(stmt: Unit, maybeOp: Option[Value], order: Int): Ast = {
    val opAst = maybeOp match {
      case Some(op) => astsForValue(op, 1, stmt)
      case None     => Seq()
    }
    val unknown = NewUnknown()
      .order(order)
      .code(stmt.toString())
      .lineNumber(line(stmt))
      .columnNumber(column(stmt))
      .typeFullName(registerType("void"))
    Ast(unknown)
      .withChildren(opAst)
  }

  private def astsForReturnNode(returnStmt: ReturnStmt, order: Int): Seq[Ast] = {
    val astChildren = astsForValue(returnStmt.getOp, 1, returnStmt)
    val returnNode = NewReturn()
      .argumentIndex(order)
      .order(order)
      .code(s"return ${astChildren.flatMap(_.root).map(_.properties(PropertyNames.CODE)).mkString(" ")};")
      .lineNumber(line(returnStmt))
      .columnNumber(column(returnStmt))

    Seq(
      Ast(returnNode)
        .withChildren(astChildren)
        .withArgEdges(returnNode, astChildren.flatMap(_.root))
    )
  }

  private def astsForReturnVoidNode(returnVoidStmt: ReturnVoidStmt, order: Int): Seq[Ast] = {
    Seq(
      Ast(
        NewReturn()
          .argumentIndex(order)
          .order(order)
          .code(s"return;")
          .lineNumber(line(returnVoidStmt))
          .columnNumber(column(returnVoidStmt))
      )
    )
  }

  private def astForFieldRef(fieldRef: FieldRef, order: Int, parentUnit: soot.Unit): Ast = {
    val leftOpString = fieldRef match {
      case x: StaticFieldRef   => x.getFieldRef.declaringClass().toString
      case x: InstanceFieldRef => x.getBase.toString()
      case _                   => fieldRef.getFieldRef.declaringClass().toString
    }
    val leftOpType = fieldRef match {
      case x: StaticFieldRef   => x.getFieldRef.declaringClass().getType
      case x: InstanceFieldRef => x.getBase.getType
      case _                   => fieldRef.getFieldRef.declaringClass().getType
    }

    val fieldAccessBlock = NewCall()
      .name(Operators.fieldAccess)
      .code(s"$leftOpString.${fieldRef.getFieldRef.name()}")
      .typeFullName(registerType(fieldRef.getType.toQuotedString))
      .methodFullName(Operators.fieldAccess)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .order(order)
      .argumentIndex(order)
      .lineNumber(line(parentUnit))
      .columnNumber(column(parentUnit))

    val argAsts = Seq(
      NewIdentifier()
        .order(1)
        .argumentIndex(1)
        .lineNumber(line(parentUnit))
        .columnNumber(column(parentUnit))
        .name(leftOpString)
        .code(leftOpString)
        .typeFullName(registerType(leftOpType.toQuotedString)),
      NewFieldIdentifier()
        .order(2)
        .argumentIndex(2)
        .lineNumber(line(parentUnit))
        .columnNumber(column(parentUnit))
        .canonicalName(fieldRef.getFieldRef.name())
        .code(fieldRef.getFieldRef.name())
    ).map(Ast(_))

    Ast(fieldAccessBlock)
      .withChildren(argAsts)
      .withArgEdges(fieldAccessBlock, argAsts.flatMap(_.root))
  }

  private def astForCaughtExceptionRef(caughtException: CaughtExceptionRef, order: Int, parentUnit: soot.Unit): Ast = {
    Ast(
      NewIdentifier()
        .order(order)
        .argumentIndex(order)
        .lineNumber(line(parentUnit))
        .columnNumber(column(parentUnit))
        .name(caughtException.toString())
        .code(caughtException.toString())
        .typeFullName(registerType(caughtException.getType.toQuotedString))
    )
  }

  private def astForConstantExpr(constant: Constant, order: Int): Ast = {
    constant match {
      case _: ClassConstant => Ast()
      case _: NullConstant  => Ast()
      case _: IntConstant =>
        Ast(
          NewLiteral()
            .order(order)
            .argumentIndex(order)
            .code(constant.toString)
            .typeFullName(registerType("int"))
        )
      case _: LongConstant =>
        Ast(
          NewLiteral()
            .order(order)
            .argumentIndex(order)
            .code(constant.toString)
            .typeFullName(registerType("long"))
        )
      case _: DoubleConstant =>
        Ast(
          NewLiteral()
            .order(order)
            .argumentIndex(order)
            .code(constant.toString)
            .typeFullName(registerType("double"))
        )
      case _: FloatConstant =>
        Ast(
          NewLiteral()
            .order(order)
            .argumentIndex(order)
            .code(constant.toString)
            .typeFullName(registerType("float"))
        )
      case _: StringConstant =>
        Ast(
          NewLiteral()
            .order(order)
            .argumentIndex(order)
            .code(constant.toString)
            .typeFullName(registerType("java.lang.String"))
        )
    }
  }

  private def callAst(rootNode: NewNode, args: Seq[Ast]): Ast = {
    Ast(rootNode)
      .withChildren(args)
      .withArgEdges(rootNode, args.flatMap(_.root))
  }

  private def astsForModifiers(methodDeclaration: SootMethod): Seq[Ast] = {
    Seq(
      if (methodDeclaration.isStatic) Some(ModifierTypes.STATIC) else None,
      if (methodDeclaration.isPublic) Some(ModifierTypes.PUBLIC) else None,
      if (methodDeclaration.isProtected) Some(ModifierTypes.PROTECTED) else None,
      if (methodDeclaration.isPrivate) Some(ModifierTypes.PRIVATE) else None,
      if (methodDeclaration.isAbstract) Some(ModifierTypes.ABSTRACT) else None,
      if (methodDeclaration.isConstructor) Some(ModifierTypes.CONSTRUCTOR) else None,
      if (!methodDeclaration.isFinal && !methodDeclaration.isStatic && methodDeclaration.isPublic)
        Some(ModifierTypes.VIRTUAL)
      else None,
      if (methodDeclaration.isSynchronized) Some("SYNCHRONIZED") else None
    ).flatten.map { modifier =>
      Ast(NewModifier().modifierType(modifier).code(modifier.toLowerCase))
    }
  }

  private def astForMethodReturn(methodDeclaration: SootMethod): Ast = {
    val typeFullName = registerType(methodDeclaration.getReturnType.toQuotedString)
    val methodReturnNode =
      NewMethodReturn()
        .order(methodDeclaration.getParameterCount + 2)
        .typeFullName(typeFullName)
        .code(typeFullName)
        .lineNumber(line(methodDeclaration))
    Ast(methodReturnNode)
  }

  private def createMethodNode(methodDeclaration: SootMethod, typeDecl: RefType, childNum: Int) = {
    val fullName       = methodFullName(typeDecl, methodDeclaration)
    val methodDeclType = registerType(methodDeclaration.getReturnType.toQuotedString)
    val code = if (!methodDeclaration.isConstructor) {
      s"$methodDeclType ${methodDeclaration.getName}${paramListSignature(methodDeclaration, withParams = true)}"
    } else {
      s"${typeDecl.getClassName}${paramListSignature(methodDeclaration, withParams = true)}"
    }
    NewMethod()
      .name(methodDeclaration.getName)
      .fullName(fullName)
      .code(code)
      .signature(methodDeclType + paramListSignature(methodDeclaration))
      .isExternal(false)
      .order(childNum)
      .filename(filename)
      .lineNumber(line(methodDeclaration))
      .columnNumber(column(methodDeclaration))
  }

  private def methodFullName(typeDecl: RefType, methodDeclaration: SootMethod): String = {
    val typeName   = registerType(typeDecl.toQuotedString)
    val returnType = registerType(methodDeclaration.getReturnType.toQuotedString)
    val methodName = methodDeclaration.getName
    s"$typeName.$methodName:$returnType${paramListSignature(methodDeclaration)}"
  }

  private def paramListSignature(methodDeclaration: SootMethod, withParams: Boolean = false) = {
    val paramTypes = methodDeclaration.getParameterTypes.asScala.map(x => registerType(x.toQuotedString))

    val paramNames =
      if (!methodDeclaration.isPhantom && Try(methodDeclaration.retrieveActiveBody()).isSuccess)
        methodDeclaration.retrieveActiveBody().getParameterLocals.asScala.map(_.getName)
      else
        paramTypes.zipWithIndex.map(x => { s"param${x._2 + 1}" })
    if (!withParams) {
      "(" + paramTypes.mkString(",") + ")"
    } else {
      "(" + paramTypes.zip(paramNames).map(x => s"${x._1} ${x._2}").mkString(", ") + ")"
    }
  }
}

object AstCreator {
  def line(node: Host): Option[Integer] = {
    if (node == null) None
    else if (node.getJavaSourceStartLineNumber == -1) None
    else Option(node.getJavaSourceStartLineNumber)
  }

  def column(node: Host): Option[Integer] = {
    if (node == null) None
    else if (node.getJavaSourceStartColumnNumber == -1) None
    else Option(node.getJavaSourceStartColumnNumber)
  }

  def withOrder[T <: Any, X](nodeList: java.util.List[T])(f: (T, Int) => X): Seq[X] = {
    nodeList.asScala.zipWithIndex.map { case (x, i) =>
      f(x, i + 1)
    }.toSeq
  }

  def withOrder[T <: Any, X](nodeList: Iterable[T])(f: (T, Int) => X): Seq[X] = {
    nodeList.zipWithIndex.map { case (x, i) =>
      f(x, i + 1)
    }.toSeq
  }
}
