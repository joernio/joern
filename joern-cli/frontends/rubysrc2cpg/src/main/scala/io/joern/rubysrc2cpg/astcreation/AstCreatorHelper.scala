package io.joern.rubysrc2cpg.astcreation
import io.joern.rubysrc2cpg.astcreation.GlobalTypes.{builtinFunctions, builtinPrefix}
import io.joern.rubysrc2cpg.astcreation.RubyIntermediateAst.{
  ClassFieldIdentifier,
  DummyNode,
  InstanceFieldIdentifier,
  MemberAccess,
  RubyFieldIdentifier,
  RubyNode
}
import io.joern.rubysrc2cpg.datastructures.{BlockScope, FieldDecl}
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, EdgeTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.joern.rubysrc2cpg.passes.Defines
import io.joern.rubysrc2cpg.passes.Defines.RubyOperators

trait AstCreatorHelper(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  protected def computeClassFullName(name: String): String  = s"${scope.surroundingScopeFullName.head}.$name"
  protected def computeMethodFullName(name: String): String = s"${scope.surroundingScopeFullName.head}:$name"

  override def column(node: RubyNode): Option[Integer]    = node.column
  override def columnEnd(node: RubyNode): Option[Integer] = node.columnEnd
  override def line(node: RubyNode): Option[Integer]      = node.line
  override def lineEnd(node: RubyNode): Option[Integer]   = node.lineEnd
  override def code(node: RubyNode): String               = shortenCode(node.text)

  protected def isBuiltin(x: String): Boolean      = builtinFunctions.contains(x)
  protected def prefixAsBuiltin(x: String): String = s"$builtinPrefix$pathSep$x"
  protected def pathSep                            = "."

  private def astForFieldInstance(name: String, node: RubyNode & RubyFieldIdentifier): Ast = {
    val identName = node match {
      case _: InstanceFieldIdentifier => Defines.This
      case _: ClassFieldIdentifier    => scope.surroundingTypeFullName.map(_.split("[.]").last).getOrElse(Defines.Any)
    }

    astForFieldAccess(
      MemberAccess(
        DummyNode(identifierNode(node, identName, identName, Defines.Any))(node.span.spanStart(identName)),
        ".",
        name
      )(node.span)
    )
  }

  protected def handleVariableOccurrence(node: RubyNode): Ast = {
    val name       = code(node)
    val identifier = identifierNode(node, name, name, Defines.Any)
    val typeRef    = scope.tryResolveTypeReference(name)

    node match {
      case fieldVariable: RubyFieldIdentifier =>
        scope.findFieldInScope(name) match {
          case None =>
            scope.pushField(FieldDecl(name, Defines.Any, false, false, fieldVariable))
            astForFieldInstance(name, fieldVariable)
          case Some(field) =>
            astForFieldInstance(name, field.node)
        }
      case _ =>
        scope.lookupVariable(name) match {
          case None if typeRef.isDefined =>
            Ast(identifier.typeFullName(typeRef.get.name))
          case None =>
            val local = localNode(node, name, name, Defines.Any)
            scope.addToScope(name, local) match {
              case BlockScope(block) => diffGraph.addEdge(block, local, EdgeTypes.AST)
              case _                 =>
            }
            Ast(identifier).withRefEdge(identifier, local)
          case Some(local) =>
            local match {
              case x: NewLocal             => identifier.dynamicTypeHintFullName(x.dynamicTypeHintFullName)
              case x: NewMethodParameterIn => identifier.dynamicTypeHintFullName(x.dynamicTypeHintFullName)
            }
            Ast(identifier).withRefEdge(identifier, local)
        }
    }

  }

  protected def astForAssignment(
    lhs: NewNode,
    rhs: NewNode,
    lineNumber: Option[Integer],
    columnNumber: Option[Integer]
  ): Ast = {
    val code = Seq(lhs, rhs).collect { case x: AstNodeNew => x.code }.mkString(" = ")
    val assignment = NewCall()
      .name(Operators.assignment)
      .methodFullName(Operators.assignment)
      .code(code)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .lineNumber(lineNumber)
      .columnNumber(columnNumber)

    callAst(assignment, Seq(Ast(lhs), Ast(rhs)))
  }

  protected val UnaryOperatorNames: Map[String, String] = Map(
    "!"   -> Operators.logicalNot,
    "not" -> Operators.logicalNot,
    "~"   -> Operators.not,
    "+"   -> Operators.plus,
    "-"   -> Operators.minus
  )

  protected val BinaryOperatorNames: Map[String, String] =
    Map(
      "+"   -> Operators.addition,
      "-"   -> Operators.subtraction,
      "*"   -> Operators.multiplication,
      "/"   -> Operators.division,
      "%"   -> Operators.modulo,
      "**"  -> Operators.exponentiation,
      "=="  -> Operators.equals,
      "===" -> Operators.equals,
      "!="  -> Operators.notEquals,
      "<"   -> Operators.lessThan,
      "<="  -> Operators.lessEqualsThan,
      ">"   -> Operators.greaterThan,
      ">="  -> Operators.greaterEqualsThan,
      "<=>" -> Operators.compare,
      "&&"  -> Operators.logicalAnd,
      "and" -> Operators.logicalAnd,
      "or"  -> Operators.logicalOr,
      "||"  -> Operators.logicalOr,
      "&"   -> Operators.and,
      "|"   -> Operators.or,
      "^"   -> Operators.xor,
      "<<"  -> Operators.shiftLeft,
      ">>"  -> Operators.logicalShiftRight,
      "=~"  -> RubyOperators.regexpMatch
    )

  protected val AssignmentOperatorNames: Map[String, String] = Map(
    "="   -> Operators.assignment,
    "+="  -> Operators.assignmentPlus,
    "-="  -> Operators.assignmentMinus,
    "*="  -> Operators.assignmentMultiplication,
    "/="  -> Operators.assignmentDivision,
    "%="  -> Operators.assignmentModulo,
    "**=" -> Operators.assignmentExponentiation,
    // Strictly speaking, `a ||= b` means `a || a = b`, but I reckon we wouldn't gain much representing it that way.
    "||=" -> Operators.assignmentOr,
    "&&=" -> Operators.assignmentAnd
  )
}

// TODO: Move this to a more appropriate place?
object GlobalTypes {
  val builtinPrefix = "__builtin"

  /* Sources:
   * https://ruby-doc.org/docs/ruby-doc-bundle/Manual/man-1.4/function.html
   * https://ruby-doc.org/3.2.2/Kernel.html
   *
   * We comment-out methods that require an explicit "receiver" (target of member access.)
   */
  val builtinFunctions: Set[String] = Set(
    "Array",
    "Complex",
    "Float",
    "Hash",
    "Integer",
    "Rational",
    "String",
    "__callee__",
    "__dir__",
    "__method__",
    "abort",
    "at_exit",
    "autoload",
    "autoload?",
    "binding",
    "block_given?",
    "callcc",
    "caller",
    "caller_locations",
    "catch",
    "chomp",
    "chomp!",
    "chop",
    "chop!",
    // "class",
    // "clone",
    "eval",
    "exec",
    "exit",
    "exit!",
    "fail",
    "fork",
    "format",
    // "frozen?",
    "gets",
    "global_variables",
    "gsub",
    "gsub!",
    "iterator?",
    "lambda",
    "load",
    "local_variables",
    "loop",
    "open",
    "p",
    "print",
    "printf",
    "proc",
    "putc",
    "puts",
    "raise",
    "rand",
    "readline",
    "readlines",
    "require",
    "require_relative",
    "select",
    "set_trace_func",
    "sleep",
    "spawn",
    "sprintf",
    "srand",
    "sub",
    "sub!",
    "syscall",
    "system",
    "tap",
    "test",
    // "then",
    "throw",
    "trace_var",
    // "trap",
    "untrace_var",
    "warn"
    // "yield_self",
  )

}
