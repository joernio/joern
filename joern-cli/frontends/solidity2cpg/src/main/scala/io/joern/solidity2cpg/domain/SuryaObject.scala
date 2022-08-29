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
    def lineNumber: Option[Int]   = None
    def columnNumber: Option[Int] = None
  }

  case class SourceUnit(
    children: List[BaseASTNode],
    override val lineNumber: Option[Int] = None,
    override val columnNumber: Option[Int] = None
  ) extends BaseASTNode("SourceUnit")
      with LocInfo {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class PragmaDirective(
    name: String,
    value: String,
    override val lineNumber: Option[Int] = None,
    override val columnNumber: Option[Int] = None
  ) extends BaseASTNode("PragmaDirective")
      with LocInfo {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class ImportDirective(
    path: String,
    unitAlias: String,
    unitAliasIdentifier: String,
    symbolAliases: String,
    symbolAliasesIdentifiers: String,
    override val lineNumber: Option[Int] = None,
    override val columnNumber: Option[Int] = None
  ) extends BaseASTNode("ImportDirective")
      with LocInfo {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class ContractDefinition(
    name: String,
    baseContracts: List[BaseASTNode],
    subNodes: List[BaseASTNode],
    kind: String,
    override val lineNumber: Option[Int] = None,
    override val columnNumber: Option[Int] = None
  ) extends BaseASTNode("ContractDefinition")
      with LocInfo {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class InheritanceSpecifier(
    baseName: BaseName,
    arguments: List[BaseASTNode],
    override val lineNumber: Option[Int] = None,
    override val columnNumber: Option[Int] = None
  ) extends BaseASTNode("InheritanceSpecifier")
      with LocInfo {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class BaseName(
    `type`: String,
    namePath: String,
    override val lineNumber: Option[Int] = None,
    override val columnNumber: Option[Int] = None
  ) extends BaseASTNode("BaseName")
      with LocInfo {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class UserDefinedTypeName(
    namePath: String,
    override val lineNumber: Option[Int] = None,
    override val columnNumber: Option[Int] = None
  ) extends BaseASTNode("UserDefinedTypeName")
      with LocInfo {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class ModifierDefinition(
    name: String,
    parameters: List[BaseASTNode],
    body: BaseASTNode,
    isVirtual: Boolean,
    `override`: BaseASTNode,
    override val lineNumber: Option[Int] = None,
    override val columnNumber: Option[Int] = None
  ) extends BaseASTNode("ModifierDefinition")
      with FunctionOrModifierDefinition
      with LocInfo {
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
    override val lineNumber: Option[Int] = None,
    override val columnNumber: Option[Int] = None
  ) extends BaseASTNode("VariableDeclaration")
      with LocInfo {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class ElementaryTypeName(
    name: String,
    stateMutability: String,
    override val lineNumber: Option[Int] = None,
    override val columnNumber: Option[Int] = None
  ) extends BaseASTNode("ElementaryTypeName")
      with LocInfo {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class Identifier(
    name: String,
    override val lineNumber: Option[Int] = None,
    override val columnNumber: Option[Int] = None
  ) extends BaseASTNode("Identifier")
      with LocInfo {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class Block(
    statements: List[BaseASTNode],
    override val lineNumber: Option[Int] = None,
    override val columnNumber: Option[Int] = None
  ) extends BaseASTNode("Block")
      with LocInfo {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class ExpressionStatement(
    expression: BaseASTNode,
    override val lineNumber: Option[Int] = None,
    override val columnNumber: Option[Int] = None
  ) extends BaseASTNode("ExpressionStatement")
      with LocInfo {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class FunctionCall(
    expression: BaseASTNode,
    arguments: List[BaseASTNode],
    names: List[String],
    identifiers: List[String],
    methodFullName: String,
    override val lineNumber: Option[Int] = None,
    override val columnNumber: Option[Int] = None
  ) extends BaseASTNode("FunctionCall")
      with LocInfo {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class MemberAccess(
    expression: BaseASTNode,
    memberName: String,
    override val lineNumber: Option[Int] = None,
    override val columnNumber: Option[Int] = None
  ) extends BaseASTNode("MemberAccess")
      with LocInfo {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class IndexAccess(
    base: BaseASTNode,
    index: BaseASTNode,
    override val lineNumber: Option[Int] = None,
    override val columnNumber: Option[Int] = None
  ) extends BaseASTNode("IndexAccess")
      with LocInfo {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class BinaryOperation(
    operator: String,
    left: BaseASTNode,
    right: BaseASTNode,
    override val lineNumber: Option[Int] = None,
    override val columnNumber: Option[Int] = None
  ) extends BaseASTNode("BinaryOperation")
      with LocInfo {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class StringLiteral(
    value: String,
    parts: List[String],
    isUnicode: List[JsBoolean],
    override val lineNumber: Option[Int] = None,
    override val columnNumber: Option[Int] = None
  ) extends BaseASTNode("StringLiteral")
      with LocInfo {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class EventDefinition(
    name: String,
    parameters: List[BaseASTNode],
    isAnonymous: Boolean,
    override val lineNumber: Option[Int] = None,
    override val columnNumber: Option[Int] = None
  ) extends BaseASTNode("EventDefinition")
      with LocInfo {
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
    stateMutability: String,
    override val lineNumber: Option[Int] = None,
    override val columnNumber: Option[Int] = None
  ) extends BaseASTNode("FunctionDefinition")
      with FunctionOrModifierDefinition
      with LocInfo {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class ModifierInvocation(
    name: String,
    arguments: List[BaseASTNode],
    override val lineNumber: Option[Int] = None,
    override val columnNumber: Option[Int] = None
  ) extends BaseASTNode("ModifierInvocation")
      with LocInfo {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class EmitStatement(
    eventCall: BaseASTNode,
    override val lineNumber: Option[Int] = None,
    override val columnNumber: Option[Int] = None
  ) extends BaseASTNode("EmitStatement")
      with LocInfo {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class ForStatement(
    initExpression: BaseASTNode,
    conditionExpression: BaseASTNode,
    loopExpression: BaseASTNode,
    body: BaseASTNode,
    override val lineNumber: Option[Int] = None,
    override val columnNumber: Option[Int] = None
  ) extends BaseASTNode("ForStatement")
      with LocInfo {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class VariableDeclarationStatement(
    variables: List[BaseASTNode],
    initialValue: BaseASTNode,
    override val lineNumber: Option[Int] = None,
    override val columnNumber: Option[Int] = None
  ) extends BaseASTNode("VariableDeclarationStatement")
      with LocInfo {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class UnaryOperation(
    operator: String,
    subExpression: BaseASTNode,
    isPrefix: Boolean,
    override val lineNumber: Option[Int] = None,
    override val columnNumber: Option[Int] = None
  ) extends BaseASTNode("UnaryOperation")
      with LocInfo {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class IfStatement(
    condition: BaseASTNode,
    trueBody: BaseASTNode,
    falseBody: BaseASTNode,
    override val lineNumber: Option[Int] = None,
    override val columnNumber: Option[Int] = None
  ) extends BaseASTNode("IfStatement")
      with LocInfo {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class BooleanLiteral(
    value: Boolean,
    override val lineNumber: Option[Int] = None,
    override val columnNumber: Option[Int] = None
  ) extends BaseASTNode("BooleanLiteral")
      with LocInfo {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class ArrayTypeName(
    baseTypeName: BaseASTNode,
    length: String,
    override val lineNumber: Option[Int] = None,
    override val columnNumber: Option[Int] = None
  ) extends BaseASTNode("ArrayTypeName")
      with LocInfo {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class NumberLiteral(
    number: String,
    subdenomination: String,
    override val lineNumber: Option[Int] = None,
    override val columnNumber: Option[Int] = None
  ) extends BaseASTNode("NumberLiteral")
      with LocInfo {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class StateVariableDeclaration(
    variables: List[BaseASTNode],
    initialValue: BaseASTNode,
    override val lineNumber: Option[Int] = None,
    override val columnNumber: Option[Int] = None
  ) extends BaseASTNode("StateVariableDeclaration")
      with LocInfo {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class Mapping(
    keyType: BaseASTNode,
    valueType: BaseASTNode,
    override val lineNumber: Option[Int] = None,
    override val columnNumber: Option[Int] = None
  ) extends BaseASTNode("Mapping")
      with LocInfo {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class StructDefinition(
    name: String,
    members: List[BaseASTNode],
    override val lineNumber: Option[Int] = None,
    override val columnNumber: Option[Int] = None
  ) extends BaseASTNode("StructDefinition")
      with LocInfo {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class UsingForDeclaration(
    typeName: BaseASTNode,
    libraryName: String,
    override val lineNumber: Option[Int] = None,
    override val columnNumber: Option[Int] = None
  ) extends BaseASTNode("UsingForDeclaration")
      with LocInfo {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class ReturnStatement(
    expression: BaseASTNode,
    override val lineNumber: Option[Int] = None,
    override val columnNumber: Option[Int] = None
  ) extends BaseASTNode("ReturnStatement")
      with LocInfo {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class TupleExpression(
    components: List[BaseASTNode],
    isArray: Boolean,
    override val lineNumber: Option[Int] = None,
    override val columnNumber: Option[Int] = None
  ) extends BaseASTNode("TupleExpression")
      with LocInfo {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class FunctionTypeName(
    parameterTypes: List[BaseASTNode],
    returnTypes: List[BaseASTNode],
    visibility: String,
    stateMutability: String,
    override val lineNumber: Option[Int] = None,
    override val columnNumber: Option[Int] = None
  ) extends BaseASTNode("  FunctionTypeName")
      with LocInfo {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class NewExpression(
    typeName: BaseASTNode,
    override val lineNumber: Option[Int] = None,
    override val columnNumber: Option[Int] = None
  ) extends BaseASTNode("  NewExpression")
      with LocInfo {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class TypeNameExpression(
    typeName: BaseASTNode,
    override val lineNumber: Option[Int] = None,
    override val columnNumber: Option[Int] = None
  ) extends BaseASTNode("  TypeNameExpression")
      with LocInfo {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class InlineAssemblyStatement(
    language: BaseASTNode,
    body: BaseASTNode,
    override val lineNumber: Option[Int] = None,
    override val columnNumber: Option[Int] = None
  ) extends BaseASTNode("  InlineAssemblyStatement")
      with LocInfo {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class AssemblyBlock(
    operations: List[BaseASTNode],
    override val lineNumber: Option[Int] = None,
    override val columnNumber: Option[Int] = None
  ) extends BaseASTNode("  AssemblyBlock")
      with LocInfo {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class AssemblyAssignment(
    names: List[BaseASTNode],
    expression: BaseASTNode,
    override val lineNumber: Option[Int] = None,
    override val columnNumber: Option[Int] = None
  ) extends BaseASTNode("  AssemblyAssignment")
      with LocInfo {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }
  case class AssemblyCall(
    functionName: String,
    arguments: List[BaseASTNode],
    override val lineNumber: Option[Int] = None,
    override val columnNumber: Option[Int] = None
  ) extends BaseASTNode("  AssemblyCall")
      with LocInfo {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }
  case class DecimalNumber(
    value: String,
    override val lineNumber: Option[Int] = None,
    override val columnNumber: Option[Int] = None
  ) extends BaseASTNode("  DecimalNumber")
      with LocInfo {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }
  case class ThrowStatement(override val lineNumber: Option[Int] = None, override val columnNumber: Option[Int] = None)
      extends BaseASTNode("  ThrowStatement")
      with LocInfo {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }
  case class Conditional(
    condition: BaseASTNode,
    override val lineNumber: Option[Int] = None,
    override val columnNumber: Option[Int] = None
  ) extends BaseASTNode("  Conditional")
      with LocInfo {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }
  case class WhileStatement(
    condition: BaseASTNode,
    body: BaseASTNode,
    override val lineNumber: Option[Int] = None,
    override val columnNumber: Option[Int] = None
  ) extends BaseASTNode("  WhileStatement")
      with LocInfo {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }
}
