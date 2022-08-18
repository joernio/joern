package io.joern.solidity2cpg.domain
import spray.json.{DefaultJsonProtocol, JsBoolean, JsNull, JsString, JsValue, JsonFormat}

/** Represents objects in the Surya JSON. This allows them to be easily grouped together and used in match statements.
  */
sealed trait SuryaObject

/** Contains the case classes for expected objects parseable from Surya JSON files. The BaseASTNode is the default class
  * if an unhandled type occurred. It contains the type name so that it can be added at a later stage.
  */
object SuryaObject {

  /** If the type is not supported, it will at least hold it's type name so that it can be added in later.
    * @param `type`
    *   the AST node type this represents.
    */
  class BaseASTNode(private var `type`: String = "Unknown") extends SuryaObject {

    /** Overrides the default toString which will look something like the class name followed by a memory address. This
      * happens to override the rest of the case class toString methods which is why they have been overridden by
      * scala.runtime.ScalaRunTime._toString calls.
      * @return
      *   a "case class"-like toString representation that includes the type of the node.
      */
    override def toString: String = s"BaseASTNode(${`type`})"

    def getType: String = `type`
  }

  trait LocInfo {
    def lineNumber: Option[Int]   = None
    def columnNumber: Option[Int] = None
  }

  trait FunctionOrModifierDefinition {
    def name: String
    def parameters: List[BaseASTNode]
    def body: BaseASTNode
    def isVirtual: Boolean
    def `override`: BaseASTNode
  }

  case class SourceUnit(children: List[BaseASTNode]) extends BaseASTNode("SourceUnit") {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class PragmaDirective(name: String, value: String) extends BaseASTNode("PragmaDirective") {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class ImportDirective(
    path: String,
    unitAlias: String,
    unitAliasIdentifier: String,
    symbolAliases: String,
    symbolAliasesIdentifiers: String
  ) extends BaseASTNode("ImportDirective") {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class ContractDefinition(
    name: String,
    baseContracts: List[BaseASTNode],
    subNodes: List[BaseASTNode],
    kind: String
  ) extends BaseASTNode("ContractDefinition") {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class InheritanceSpecifier(baseName: BaseName, arguments: List[BaseASTNode])
      extends BaseASTNode("InheritanceSpecifier") {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class BaseName(`type`: String, namePath: String) extends BaseASTNode("BaseName") {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class UserDefinedTypeName(namePath: String) extends BaseASTNode("UserDefinedTypeName") {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class ModifierDefinition(
    name: String,
    parameters: List[BaseASTNode],
    body: BaseASTNode,
    isVirtual: Boolean,
    `override`: BaseASTNode
  ) extends BaseASTNode("ModifierDefinition") with FunctionOrModifierDefinition {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class VariableDeclaration(
    typeName: BaseASTNode,
    name: String,
    identifier: BaseASTNode,
    expression: BaseASTNode,
    visibility: String,
    storageLocation: String,
    isStateVar: Boolean,
    isDeclaredConst: Boolean,
    isIndexed: Boolean,
    isImmutable: Boolean,
    `override`: BaseASTNode,
//    override val lineNumber: Option[Int] = None,
//    override val columnNumber: Option[Int] = None
  ) extends BaseASTNode("VariableDeclaration")
      with LocInfo {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class ElementaryTypeName(name: String, stateMutability: String) extends BaseASTNode("ElementaryTypeName") {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class Identifier(name: String) extends BaseASTNode("Identifier") {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class Block(statements: List[BaseASTNode]) extends BaseASTNode("Block") {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class ExpressionStatement(expression: BaseASTNode) extends BaseASTNode("ExpressionStatement") {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class FunctionCall(
    expression: BaseASTNode,
    arguments: List[BaseASTNode],
    names: List[String],
    identifiers: List[String],
    methodFullName: String
  ) extends BaseASTNode("FunctionCall") {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class MemberAccess(expression: BaseASTNode, memberName: String) extends BaseASTNode("MemberAccess") {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class IndexAccess(base: BaseASTNode, index: BaseASTNode) extends BaseASTNode("IndexAccess") {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class BinaryOperation(operator: String, left: BaseASTNode, right: BaseASTNode)
      extends BaseASTNode("BinaryOperation") {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class StringLiteral(value: String, parts: List[String], isUnicode: List[JsBoolean])
      extends BaseASTNode("StringLiteral") {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class EventDefinition(name: String, parameters: List[BaseASTNode], isAnonymous: Boolean)
      extends BaseASTNode("EventDefinition") {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class FunctionDefinition(
    name: String,
    parameters: List[BaseASTNode],
    returnParameters: List[BaseASTNode],
    body: BaseASTNode,
    visibility: String,
    modifiers: List[BaseASTNode],
    `override`: BaseASTNode,
    isConstructor: Boolean,
    isReceiveEther: Boolean,
    isFallback: Boolean,
    isVirtual: Boolean,
    stateMutability: String
  ) extends BaseASTNode("FunctionDefinition") with FunctionOrModifierDefinition {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class ModifierInvocation(name: String, arguments: List[BaseASTNode]) extends BaseASTNode("ModifierInvocation") {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class EmitStatement(eventCall: BaseASTNode) extends BaseASTNode("EmitStatement") {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class ForStatement(
    initExpression: BaseASTNode,
    conditionExpression: BaseASTNode,
    loopExpression: BaseASTNode,
    body: BaseASTNode
  ) extends BaseASTNode("ForStatement") {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class VariableDeclarationStatement(variables: List[BaseASTNode], initialValue: BaseASTNode)
      extends BaseASTNode("VariableDeclarationStatement") {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class UnaryOperation(operator: String, subExpression: BaseASTNode, isPrefix: Boolean)
      extends BaseASTNode("UnaryOperation") {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class IfStatement(condition: BaseASTNode, trueBody: BaseASTNode, falseBody: BaseASTNode)
      extends BaseASTNode("IfStatement") {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class BooleanLiteral(value: Boolean) extends BaseASTNode("BooleanLiteral") {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class ArrayTypeName(baseTypeName: BaseASTNode, length: String) extends BaseASTNode("ArrayTypeName") {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class NumberLiteral(number: String, subdenomination: String) extends BaseASTNode("NumberLiteral") {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class StateVariableDeclaration(variables: List[BaseASTNode], initialValue: BaseASTNode) extends BaseASTNode("StateVariableDeclaration") {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class Mapping(keyType: BaseASTNode, valueType: BaseASTNode) extends BaseASTNode("Mapping") {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class StructDefinition(name: String, members: List[BaseASTNode]) extends BaseASTNode("StructDefinition") {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class UsingForDeclaration(typeName: BaseASTNode, libraryName: String)
      extends BaseASTNode("UsingForDeclaration") {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class ReturnStatement(expression: BaseASTNode) extends BaseASTNode("ReturnStatement") {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class TupleExpression(components: List[BaseASTNode], isArray: Boolean) extends BaseASTNode("TupleExpression") {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class FunctionTypeName(
    parameterTypes: List[BaseASTNode],
    returnTypes: List[BaseASTNode],
    visibility: String,
    stateMutability: String
  ) extends BaseASTNode("  FunctionTypeName") {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class NewExpression(typeName: BaseASTNode) extends BaseASTNode("  NewExpression") {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class TypeNameExpression(typeName: BaseASTNode) extends BaseASTNode("  TypeNameExpression") {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class InlineAssemblyStatement(language: BaseASTNode, body: BaseASTNode) extends BaseASTNode("  InlineAssemblyStatement") {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class AssemblyBlock(operations: List[BaseASTNode]) extends BaseASTNode("  AssemblyBlock") {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class AssemblyAssignment(names: List[BaseASTNode], expression: BaseASTNode) extends BaseASTNode("  AssemblyAssignment") {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class AssemblyCall(functionName: String, arguments: List[BaseASTNode]) extends BaseASTNode("  AssemblyCall") {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }
  case class DecimalNumber(value: String) extends  BaseASTNode("  DecimalNumber") {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }
  case class ThrowStatement() extends BaseASTNode(" ThrowStatement") {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }
  case class Conditional(condition: BaseASTNode) extends BaseASTNode(" Conditional") {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

}
