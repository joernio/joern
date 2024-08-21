package io.joern.rubysrc2cpg.astcreation
import io.joern.rubysrc2cpg.astcreation.RubyIntermediateAst.{
  ClassFieldIdentifier,
  DummyNode,
  InstanceFieldIdentifier,
  MemberAccess,
  RubyFieldIdentifier,
  RubyNode
}
import io.joern.rubysrc2cpg.datastructures.{BlockScope, FieldDecl}
import io.joern.rubysrc2cpg.passes.Defines
import io.joern.rubysrc2cpg.passes.GlobalTypes
import io.joern.rubysrc2cpg.passes.GlobalTypes.{kernelFunctions, kernelPrefix}
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, EdgeTypes, Operators}

import scala.collection.mutable

trait AstCreatorHelper(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  private val usedFullNames = mutable.Set.empty[String]

  /** Ensures a unique full name is assigned based on the current scope.
    * @param name
    *   the name of the entity.
    * @param counter
    *   an optional counter, used to create unique instances in the case of redefinitions.
    * @return
    *   a unique full name.
    */
  protected def computeFullName(name: String, counter: Option[Int] = None): String = {
    val candidate = counter match {
      case Some(cnt) => s"${scope.surroundingScopeFullName.head}.$name$cnt"
      case None      => s"${scope.surroundingScopeFullName.head}.$name"
    }
    if (usedFullNames.contains(candidate)) {
      computeFullName(name, counter.map(_ + 1).orElse(Option(0)))
    } else {
      usedFullNames.add(candidate)
      candidate
    }
  }

  override def column(node: RubyNode): Option[Int]    = node.column
  override def columnEnd(node: RubyNode): Option[Int] = node.columnEnd
  override def line(node: RubyNode): Option[Int]      = node.line
  override def lineEnd(node: RubyNode): Option[Int]   = node.lineEnd

  override def code(node: RubyNode): String = shortenCode(node.text)

  protected def isBuiltin(x: String): Boolean            = kernelFunctions.contains(x)
  protected def prefixAsKernelDefined(x: String): String = s"$kernelPrefix$pathSep$x"
  protected def prefixAsBundledType(x: String): String   = s"${GlobalTypes.builtinPrefix}.$x"
  protected def isBundledClass(x: String): Boolean       = GlobalTypes.bundledClasses.contains(x)
  protected def pathSep                                  = "."

  private def astForFieldInstance(name: String, node: RubyNode & RubyFieldIdentifier): Ast = {
    val identName = node match {
      case _: InstanceFieldIdentifier => Defines.Self
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
    lineNumber: Option[Int],
    columnNumber: Option[Int]
  ): Ast = {
    astForAssignment(Ast(lhs), Ast(rhs), lineNumber, columnNumber)
  }

  protected def astForAssignment(
    lhs: Ast,
    rhs: Ast,
    lineNumber: Option[Int],
    columnNumber: Option[Int],
    code: Option[String] = None
  ): Ast = {
    val _code =
      code.getOrElse(Seq(lhs, rhs).flatMap(_.root).collect { case x: ExpressionNew => x.code }.mkString(" = "))
    val assignment = NewCall()
      .name(Operators.assignment)
      .methodFullName(Operators.assignment)
      .code(_code)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .lineNumber(lineNumber)
      .columnNumber(columnNumber)

    callAst(assignment, Seq(lhs, rhs))
  }

  protected def memberForMethod(
    method: NewMethod,
    astParentType: Option[String] = None,
    astParentFullName: Option[String] = None
  ): NewMember = {
    val member = NewMember().name(method.name).code(method.name).dynamicTypeHintFullName(Seq(method.fullName))
    astParentType.foreach(member.astParentType(_))
    astParentFullName.foreach(member.astParentFullName(_))
    member
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
//      "<<"  -> Operators.shiftLeft,  Note: Generally Ruby abstracts this as an append operator based on the LHS
      ">>" -> Operators.logicalShiftRight
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
