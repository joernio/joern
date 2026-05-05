package io.joern.abap2cpg.parser

object AbapIntermediateAst {

  sealed trait AbapNode {
    def span: TextSpan
  }

  case class Position(row: Int, col: Int)

  case class TextSpan(start: Option[Position] = None, end: Option[Position] = None, code: String = "")

  case class Parameter(name: String, typeName: String, isValue: Boolean = false, isOptional: Boolean = false)

  case class MethodParameters(
    importing: Seq[Parameter] = Seq.empty,
    exporting: Seq[Parameter] = Seq.empty,
    changing: Seq[Parameter] = Seq.empty,
    returning: Option[Parameter] = None
  )

  case class MethodDef(
    name: String,
    visibility: Option[String],
    isStatic: Boolean,
    parameters: MethodParameters,
    body: Option[StatementList] = None,
    span: TextSpan
  ) extends AbapNode

  case class ClassDef(
    name: String,
    visibility: String,
    isFinal: Boolean = false,
    methods: Seq[MethodDef] = Seq.empty,
    span: TextSpan
  ) extends AbapNode

  case class StatementList(statements: Seq[AbapNode], span: TextSpan) extends AbapNode

  case class CallExpr(
    targetName: String,
    methodName: Option[String] = None,
    arguments: Seq[Argument] = Seq.empty,
    isStatic: Boolean = false,
    span: TextSpan
  ) extends AbapNode

  case class Argument(name: Option[String], value: AbapNode)

  case class IdentifierExpr(name: String, span: TextSpan)                                 extends AbapNode
  case class LiteralExpr(value: String, literalType: String, span: TextSpan)              extends AbapNode
  case class AssignmentStmt(target: AbapNode, value: AbapNode, span: TextSpan)            extends AbapNode
  case class OperatorCall(operatorName: String, arguments: Seq[AbapNode], span: TextSpan) extends AbapNode
  case class FieldAccessExpr(target: AbapNode, fieldName: String, span: TextSpan)         extends AbapNode
  case class DataDeclaration(name: String, typeName: String, initialValue: Option[AbapNode] = None, span: TextSpan)
      extends AbapNode

  case class ProgramRoot(
    fileName: String,
    objectType: String,
    classes: Seq[ClassDef] = Seq.empty,
    methods: Seq[MethodDef] = Seq.empty,
    span: TextSpan
  ) extends AbapNode

  case class UnknownNode(nodeType: String, span: TextSpan) extends AbapNode
}
