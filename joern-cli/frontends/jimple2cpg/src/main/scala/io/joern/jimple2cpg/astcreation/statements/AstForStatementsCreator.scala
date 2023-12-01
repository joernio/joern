package io.joern.jimple2cpg.astcreation.statements

import io.joern.jimple2cpg.astcreation.AstCreator
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, DispatchTypes, Operators, PropertyNames}
import org.slf4j.LoggerFactory
import soot.jimple.*
import soot.tagkit.Host
import soot.{Unit, Value}

import scala.collection.mutable
import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.collection.mutable.ArrayBuffer

trait AstForStatementsCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  private val logger = LoggerFactory.getLogger(getClass)

  protected def astsForStatement(statement: soot.Unit, info: BodyControlInfo): Seq[Ast] = {
    val stmt = statement match {
      case x: AssignStmt       => astsForDefinition(x)
      case x: InvokeStmt       => astsForExpression(x.getInvokeExpr, statement)
      case x: ReturnStmt       => astsForReturnStmt(x)
      case x: ReturnVoidStmt   => astsForReturnVoidStmt(x)
      case x: IfStmt           => astsForIfStmt(x, info)
      case x: GotoStmt         => astsForGotoStmt(x, info)
      case x: LookupSwitchStmt => astsForLookupSwitchStmt(x)
      case x: TableSwitchStmt  => astsForTableSwitchStmt(x)
      case x: ThrowStmt        => astsForThrowStmt(x)
      case x: MonitorStmt      => astsForMonitorStmt(x)
      case x: IdentityStmt     => astsForIdentityStmt(x)
      case _: NopStmt          => Seq() // Ignore NOP statements
      case x =>
        logger.warn(s"Unhandled soot.Unit type ${x.getClass}")
        Seq(astForUnknownStmt(x, None))
    }
    // Populate standard control-flow information
    info.unitToAsts.put(statement, stmt)
    statement.getBoxesPointingToThis.asScala
      .filterNot(_.getUnit == statement)
      .foreach(y => info.edges.addOne(y.getUnit -> statement))
    stmt
  }

  /** Helper method for operator nodes.
    */
  private def operatorNode(
    node: Host,
    operation: String,
    code: String,
    typeFullName: Option[String] = None
  ): NewCall = {
    callNode(node, code, operation, operation, DispatchTypes.STATIC_DISPATCH, None, typeFullName = typeFullName)
  }

  /** Creates the AST for assignment statements keeping in mind Jimple is a 3-address code language.
    */
  private def astsForDefinition(assignStmt: DefinitionStmt): Seq[Ast] = {
    val initializer = assignStmt.getRightOp
    val leftOp      = assignStmt.getLeftOp

    val identifier = leftOp match {
      case x: soot.Local => Seq(astForLocal(x, assignStmt))
      case x: FieldRef   => Seq(astForFieldRef(x, assignStmt))
      case x             => astsForValue(x, assignStmt)
    }
    val lhsCode = identifier.flatMap(_.root).flatMap(_.properties.get(PropertyNames.CODE)).mkString

    val initAsts = astsForValue(initializer, assignStmt)
    val rhsCode = initAsts
      .flatMap(_.root)
      .map(_.properties.getOrElse(PropertyNames.CODE, ""))
      .mkString(", ")

    val assignment = operatorNode(
      assignStmt,
      Operators.assignment,
      s"$lhsCode = $rhsCode",
      Option(registerType(leftOp.getType.toQuotedString))
    )
    Seq(callAst(assignment, identifier ++ initAsts))
  }

  private def astsForLookupSwitchStmt(lookupSwitchStmt: LookupSwitchStmt): Seq[Ast] = {
    val totalTgts = lookupSwitchStmt.getTargets.size()
    val switchAst = astForSwitchWithDefaultAndCondition(lookupSwitchStmt)

    val tgts = for {
      i <- 0 until totalTgts
      if lookupSwitchStmt.getTarget(i) != lookupSwitchStmt.getDefaultTarget
    } yield (lookupSwitchStmt.getLookupValue(i), lookupSwitchStmt.getTarget(i))
    val tgtAsts = tgts.map { case (lookup, target) =>
      Ast(
        NewJumpTarget()
          .name(s"case $lookup")
          .code(s"case $lookup:")
          .lineNumber(line(target))
          .columnNumber(column(target))
      )
    }

    Seq(switchAst.withChildren(tgtAsts))
  }

  private def astsForTableSwitchStmt(tableSwitchStmt: SwitchStmt): Seq[Ast] = {
    val switchAst = astForSwitchWithDefaultAndCondition(tableSwitchStmt)
    val tgtAsts = tableSwitchStmt.getTargets.asScala
      .filter(x => tableSwitchStmt.getDefaultTarget != x)
      .zipWithIndex
      .map { case (tgt, i) =>
        Ast(
          NewJumpTarget()
            .name(s"case $i")
            .code(s"case $i:")
            .lineNumber(line(tgt))
            .columnNumber(column(tgt))
        )
      }
      .toSeq

    Seq(
      switchAst
        .withChildren(tgtAsts)
    )
  }

  private def astsForThrowStmt(throwStmt: ThrowStmt): Seq[Ast] = {
    val opAst = astsForValue(throwStmt.getOp, throwStmt)
    val throwNode = NewCall()
      .name("<operator>.throw")
      .methodFullName("<operator>.throw")
      .lineNumber(line(throwStmt))
      .columnNumber(column(throwStmt))
      .code(s"throw new ${throwStmt.getOp.getType}()")
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
    Seq(
      Ast(throwNode)
        .withChildren(opAst)
    )
  }

  private def astsForMonitorStmt(monitorStmt: MonitorStmt): Seq[Ast] = {
    val opAst      = astsForValue(monitorStmt.getOp, monitorStmt)
    val typeString = opAst.flatMap(_.root).map(_.properties(PropertyNames.CODE)).mkString
    val code = monitorStmt match {
      case _: EnterMonitorStmt => s"entermonitor $typeString"
      case _: ExitMonitorStmt  => s"exitmonitor $typeString"
      case _                   => s"<unknown>monitor $typeString"
    }
    Seq(
      Ast(
        NewUnknown()
          .code(code)
          .lineNumber(line(monitorStmt))
          .columnNumber(column(monitorStmt))
      ).withChildren(opAst)
    )
  }

  private def astsForIdentityStmt(x: IdentityStmt): Seq[Ast] = {
    x.getRightOp match
      case _: CaughtExceptionRef => astsForDefinition(x)
      case _                     => Seq.empty // Other identity statements redefine parameters as locals
  }

  private def astForUnknownStmt(stmt: Unit, maybeOp: Option[Value]): Ast = {
    val opAst = maybeOp match {
      case Some(op) => astsForValue(op, stmt)
      case None     => Seq()
    }
    val unknown = NewUnknown()
      .code(stmt.toString())
      .lineNumber(line(stmt))
      .columnNumber(column(stmt))
      .typeFullName(registerType("void"))
    Ast(unknown)
      .withChildren(opAst)
  }

  private def astsForReturnStmt(returnStmt: ReturnStmt): Seq[Ast] = {
    val astChildren = astsForValue(returnStmt.getOp, returnStmt)
    val returnNode = NewReturn()
      .code(s"return ${astChildren.flatMap(_.root).map(_.properties(PropertyNames.CODE)).mkString(" ")};")
      .lineNumber(line(returnStmt))
      .columnNumber(column(returnStmt))

    Seq(
      Ast(returnNode)
        .withChildren(astChildren)
        .withArgEdges(returnNode, astChildren.flatMap(_.root), 1)
    )
  }

  private def astsForReturnVoidStmt(returnVoidStmt: ReturnVoidStmt): Seq[Ast] = {
    Seq(
      Ast(
        NewReturn()
          .code(s"return;")
          .lineNumber(line(returnVoidStmt))
          .columnNumber(column(returnVoidStmt))
      )
    )
  }

  private def astsForIfStmt(ifStmt: IfStmt, info: BodyControlInfo): Seq[Ast] = {
    // bytecode/jimple ASTs are flat so there will not be nested bodies
    val condition = astsForValue(ifStmt.getCondition, ifStmt)
    info.targets.put(condition, ifStmt.getTarget)
    condition
  }

  private def astsForGotoStmt(gotoStmt: GotoStmt, info: BodyControlInfo): Seq[Ast] = {
    // bytecode/jimple ASTs are flat so there will not be nested bodies
    val gotoAst = Seq(
      Ast(
        NewUnknown()
          .code(s"goto ${line(gotoStmt.getTarget).getOrElse("<unknown>")}")
          .lineNumber(line(gotoStmt))
          .columnNumber(column(gotoStmt))
      )
    )
    info.targets.put(gotoAst, gotoStmt.getTarget)
    gotoAst
  }

  private def astForSwitchWithDefaultAndCondition(switchStmt: SwitchStmt): Ast = {
    val jimple = switchStmt.toString()
    val switch = NewControlStructure()
      .controlStructureType(ControlStructureTypes.SWITCH)
      .code(jimple.substring(0, jimple.indexOf("{") - 1))
      .lineNumber(line(switchStmt))
      .columnNumber(column(switchStmt))

    val conditionalAst = astsForValue(switchStmt.getKey, switchStmt)
    val defaultAst = Seq(
      Ast(
        NewJumpTarget()
          .name("default")
          .code("default:")
          .lineNumber(line(switchStmt.getDefaultTarget))
          .columnNumber(column(switchStmt.getDefaultTarget))
      )
    )
    Ast(switch)
      .withConditionEdge(switch, conditionalAst.flatMap(_.root).head)
      .withChildren(conditionalAst ++ defaultAst)
  }

}

class BodyControlInfo(
  val unitToAsts: mutable.HashMap[soot.Unit, Seq[Ast]] = mutable.HashMap.empty,
  val targets: mutable.HashMap[Seq[Ast], soot.Unit] = mutable.HashMap.empty,
  val edges: ArrayBuffer[(soot.Unit, soot.Unit)] = mutable.ArrayBuffer.empty
)
