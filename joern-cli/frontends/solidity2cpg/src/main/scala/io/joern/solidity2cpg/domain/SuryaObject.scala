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
  class BaseASTNode(private val `type`: String = "Unknown") extends SuryaObject {

    /** Overrides the default toString which will look something like the class name followed by a memory address. This
      * happens to override the rest of the case class toString methods which is why they have been overridden by
      * scala.runtime.ScalaRunTime._toString calls.
      * @return
      *   a "case class"-like toString representation that includes the type of the node.
      */
    override def toString: String = s"BaseASTNode(${`type`})"

    def getType: String = `type`
  }

  case class SourceUnit(children: List[BaseASTNode]) extends BaseASTNode() {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class PragmaDirective(name: String, value: String) extends BaseASTNode() {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class ImportDirective(
    path: String,
    unitAlias: String,
    unitAliasIdentifier: String,
    symbolAliases: String,
    symbolAliasesIdentifiers: String
  ) extends BaseASTNode() {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class ContractDefinition(
    name: String,
    baseContracts: List[BaseASTNode],
    subNodes: List[BaseASTNode],
    kind: String
  ) extends BaseASTNode() {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class InheritanceSpecifier(baseName: BaseName, arguments: List[BaseASTNode]) extends BaseASTNode() {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class BaseName(`type`: String, namePath: String) extends BaseASTNode() {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class UserDefinedTypeName(namePath: String) extends BaseASTNode() {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class ModifierDefinition(
    name: String,
    parameters: List[BaseASTNode],
    body: BaseASTNode,
    isVirtual: Boolean,
    `override`: BaseASTNode
  ) extends BaseASTNode() {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class VariableDeclaration(
    typeName: BaseASTNode,
    name: String,
    identifier: BaseASTNode,
    storageLocation: String,
    isStateVar: Boolean,
    isIndexed: Boolean,
    expression: BaseASTNode
  ) extends BaseASTNode() {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class ElementaryTypeName(name: String, stateMutability: String) extends BaseASTNode() {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class Identifier(name: String) extends BaseASTNode() {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class Block(statements: List[BaseASTNode]) extends BaseASTNode() {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class ExpressionStatement(expression: BaseASTNode) extends BaseASTNode() {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class FunctionCall(
    expression: BaseASTNode,
    arguments: List[BaseASTNode],
    names: List[String],
    identifiers: List[String]
  ) extends BaseASTNode() {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class MemberAccess(expression: BaseASTNode, memberName: String) extends BaseASTNode() {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class IndexAccess(base: BaseASTNode, index: BaseASTNode) extends BaseASTNode() {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class BinaryOperation(operator: String, left: BaseASTNode, right: BaseASTNode) extends BaseASTNode() {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class StringLiteral(value: String, parts: List[String], isUnicode: List[JsBoolean]) extends BaseASTNode() {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class EventDefinition(name: String, parameters: List[BaseASTNode], isAnonymous: Boolean) extends BaseASTNode() {
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
  ) extends BaseASTNode

  case class ModifierInvocation(name: String, arguments: List[BaseASTNode]) extends BaseASTNode() {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class EmitStatement(eventCall: BaseASTNode) extends BaseASTNode() {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class ForStatement(
    initExpression: BaseASTNode,
    conditionExpression: BaseASTNode,
    loopExpression: BaseASTNode,
    body: BaseASTNode
  ) extends BaseASTNode

  case class VariableDeclarationStatement(variables: List[BaseASTNode], initialValue: BaseASTNode)
      extends BaseASTNode() {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class UnaryOperation(operator: String, subExpression: BaseASTNode, isPrefix: Boolean) extends BaseASTNode() {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class IfStatement(condition: BaseASTNode, trueBody: BaseASTNode, falseBody: BaseASTNode) extends BaseASTNode() {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class BooleanLiteral(value: Boolean) extends BaseASTNode() {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class ArrayTypeName(baseTypeName: BaseASTNode, length: String) extends BaseASTNode() {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class NumberLiteral(number: String, subdenomination: String) extends BaseASTNode() {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class StateVariableDeclaration(variables: List[BaseASTNode]) extends BaseASTNode() {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class Mapping(keyType: BaseASTNode, valueType: BaseASTNode) extends BaseASTNode() {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class StructDefinition(name: String, members: List[BaseASTNode]) extends BaseASTNode() {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

  case class UsingForDeclaration(typeName: BaseASTNode, libraryName: String) extends BaseASTNode() {
    override def toString: String = scala.runtime.ScalaRunTime._toString(this)
  }

}
