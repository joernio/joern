package io.joern.jimple2cpg.astcreation.statements

import io.joern.jimple2cpg.astcreation.AstCreator
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, DispatchTypes, Operators, PropertyNames}
import org.slf4j.LoggerFactory
import soot.jimple.*
import soot.{Unit, Value}

import scala.jdk.CollectionConverters.CollectionHasAsScala
trait AstForStatementsCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  private val logger = LoggerFactory.getLogger(getClass)

  protected def astsForStatement(statement: soot.Unit, order: Int): Seq[Ast] = {
    val stmt = statement match {
      case x: AssignStmt       => astsForDefinition(x, order)
      case x: InvokeStmt       => astsForExpression(x.getInvokeExpr, order, statement)
      case x: ReturnStmt       => astsForReturnStmt(x, order)
      case x: ReturnVoidStmt   => astsForReturnVoidStmt(x, order)
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

  private def astsForReturnStmt(returnStmt: ReturnStmt, order: Int): Seq[Ast] = {
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

  private def astsForReturnVoidStmt(returnVoidStmt: ReturnVoidStmt, order: Int): Seq[Ast] = {
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

}
