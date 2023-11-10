package io.joern.jimple2cpg.astcreation.expressions

import io.joern.jimple2cpg.astcreation.AstCreator
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.{NewCall, NewIdentifier, NewTypeRef}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import org.slf4j.LoggerFactory
import soot.{Unit as SUnit, *}
import soot.jimple.*

import scala.jdk.CollectionConverters.CollectionHasAsScala

trait AstForExpressionsCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  private val logger = LoggerFactory.getLogger(getClass)

  protected def astsForExpression(expr: Expr, parentUnit: soot.Unit): Seq[Ast] = {
    expr match {
      case x: BinopExpr  => Seq(astForBinOpExpr(x, parentUnit))
      case x: InvokeExpr => Seq(astForInvokeExpr(x, parentUnit))
      case x: AnyNewExpr => Seq(astForNewExpr(x, parentUnit))
      case x: CastExpr   => Seq(astForUnaryExpr(Operators.cast, x, x.getOp, parentUnit))
      case x: InstanceOfExpr =>
        Seq(astForUnaryExpr(Operators.instanceOf, x, x.getOp, parentUnit))
      case x: LengthExpr =>
        Seq(astForUnaryExpr(Operators.lengthOf, x, x.getOp, parentUnit))
      case x: NegExpr => Seq(astForUnaryExpr(Operators.minus, x, x.getOp, parentUnit))
      case x =>
        logger.warn(s"Unhandled soot.Expr type ${x.getClass}")
        Seq()
    }
  }

  private def astForBinOpExpr(binOp: BinopExpr, parentUnit: soot.Unit): Ast = {
    // https://javadoc.io/static/org.soot-oss/soot/4.3.0/soot/jimple/BinopExpr.html
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
      case _: NeExpr   => Operators.notEquals
      case _ =>
        logger.warn(s"Unhandled binary operator ${binOp.getSymbol} (${binOp.getClass}). This is unexpected behaviour.")
        "<operator>.unknown"
    }

    val call = callNode(parentUnit, binOp.toString, operatorName, operatorName, DispatchTypes.STATIC_DISPATCH)

    val args =
      astsForValue(binOp.getOp1, parentUnit) ++ astsForValue(binOp.getOp2, parentUnit)
    callAst(call, args)
  }

  private def astForInvokeExpr(invokeExpr: InvokeExpr, parentUnit: soot.Unit): Ast = {
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
      case expr: InstanceInvokeExpr => astsForValue(expr.getBase, parentUnit)
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
      .methodFullName(s"$calleeType.${callee.getName}:$signature")
      .signature(signature)
      .typeFullName(callType)
      .lineNumber(line(parentUnit))
      .columnNumber(column(parentUnit))

    val argAsts = (invokeExpr match {
      case x: DynamicInvokeExpr => x.getArgs.asScala ++ x.getBootstrapArgs.asScala
      case x                    => x.getArgs.asScala
    }).flatMap(astsForValue(_, parentUnit)).toSeq

    val callAst = Ast(callNode)
      .withChildren(thisAsts)
      .withChildren(argAsts)
      .withArgEdges(callNode, thisAsts.flatMap(_.root), 0)
      .withArgEdges(callNode, argAsts.flatMap(_.root), 1)

    thisAsts.flatMap(_.root).headOption match {
      case Some(thisAst) => callAst.withReceiverEdge(callNode, thisAst)
      case None          => callAst
    }
  }

  private def astForNewExpr(x: AnyNewExpr, parentUnit: soot.Unit): Ast = {
    x match {
      case u: NewArrayExpr =>
        astForArrayCreateExpr(x, List(u.getSize), parentUnit)
      case u: NewMultiArrayExpr =>
        astForArrayCreateExpr(x, u.getSizes.asScala, parentUnit)
      case _ =>
        val parentType = registerType(x.getType.toQuotedString)
        Ast(
          NewCall()
            .name(Operators.alloc)
            .methodFullName(Operators.alloc)
            .typeFullName(parentType)
            .code(s"new ${x.getType}")
            .dispatchType(DispatchTypes.STATIC_DISPATCH)
            .lineNumber(line(parentUnit))
            .columnNumber(column(parentUnit))
        )
    }
  }

  private def astForArrayCreateExpr(arrayInitExpr: Expr, sizes: Iterable[Value], parentUnit: soot.Unit): Ast = {
    // Jimple does not have Operators.arrayInitializer
    // to enforce 3 address code form
    val arrayBaseType = registerType(arrayInitExpr.getType.toQuotedString)
    val code = s"new ${arrayBaseType.substring(0, arrayBaseType.indexOf('['))}${sizes.map(s => s"[$s]").mkString}"
    val callBlock = NewCall()
      .name(Operators.alloc)
      .methodFullName(Operators.alloc)
      .code(code)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .lineNumber(line(parentUnit))
      .columnNumber(column(parentUnit))
    val valueAsts = sizes.flatMap(astsForValue(_, parentUnit)).toSeq
    Ast(callBlock)
      .withChildren(valueAsts)
      .withArgEdges(callBlock, valueAsts.flatMap(_.root), 1)
  }

  private def astForUnaryExpr(methodName: String, unaryExpr: Expr, op: Value, parentUnit: soot.Unit): Ast = {
    val callBlock = NewCall()
      .name(methodName)
      .methodFullName(methodName)
      .code(unaryExpr.toString())
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .typeFullName(registerType(unaryExpr.getType.toQuotedString))
      .lineNumber(line(parentUnit))
      .columnNumber(column(parentUnit))

    def astForTypeRef(t: String) = {
      Seq(
        Ast(
          NewTypeRef()
            .code(if (t.contains('.')) t.substring(t.lastIndexOf('.') + 1, t.length) else t)
            .lineNumber(line(parentUnit))
            .columnNumber(column(parentUnit))
            .typeFullName(t)
        )
      )
    }

    val valueAsts = unaryExpr match {
      case instanceOfExpr: InstanceOfExpr =>
        val t = registerType(instanceOfExpr.getCheckType.toQuotedString)
        astsForValue(op, parentUnit) ++ astForTypeRef(t)
      case castExpr: CastExpr =>
        val t = registerType(castExpr.getCastType.toQuotedString)
        astForTypeRef(t) ++ astsForValue(op, parentUnit)
      case _ => astsForValue(op, parentUnit)
    }

    Ast(callBlock)
      .withChildren(valueAsts)
      .withArgEdges(callBlock, valueAsts.flatMap(_.root), 1)
  }

}
