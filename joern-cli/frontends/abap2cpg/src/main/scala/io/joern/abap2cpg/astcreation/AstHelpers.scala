package io.joern.abap2cpg.astcreation

import io.joern.abap2cpg.parser.AbapIntermediateAst.*
import io.joern.abap2cpg.passes.AstCreator
import io.shiftleft.codepropertygraph.generated.Operators

/** Helper methods for code generation and utility functions */
trait AstHelpers { this: AstCreator =>

  /** Get code string from expression */
  protected def codeFromExpr(expr: AbapNode): String = expr match {
    case ident: IdentifierExpr => ident.name
    case lit: LiteralExpr      => lit.value
    case call: CallExpr =>
      call.methodName match {
        case Some(method) =>
          val arrow = if (call.isStatic) "=>" else "->"
          s"${call.targetName}${arrow}${method}()"
        case None => s"${call.targetName}()"
      }
    case fieldAccess: FieldAccessExpr =>
      s"${codeFromExpr(fieldAccess.target)}-${fieldAccess.fieldName}"
    case op: OperatorCall => operatorCode(op)
    case _                => "?"
  }

  /** Generate code string for operator calls */
  protected def operatorCode(op: OperatorCall): String = {
    val args = op.arguments.map(codeFromExpr)
    op.operatorName match {
      case Operators.indirectFieldAccess if args.size == 2 => s"${args(0)}->${args(1)}"
      case Operators.indirection if args.size == 1         => s"${args(0)}->*"
      case Operators.fieldAccess if args.size == 2         => s"${args(0)}-${args(1)}"
      case Operators.addition if args.size == 2            => s"${args(0)} + ${args(1)}"
      case Operators.subtraction if args.size == 2         => s"${args(0)} - ${args(1)}"
      case Operators.multiplication if args.size == 2      => s"${args(0)} * ${args(1)}"
      case Operators.division if args.size == 2            => s"${args(0)} / ${args(1)}"
      case _                                               => args.mkString(" ")
    }
  }
}
