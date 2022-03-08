package io.joern.solidity2cpg.domain

/** Represents objects in the Surya JSON. This allows them to be easily grouped together and used in match statements.
  */
sealed trait SuryaObject

object SuryaObject {
  case class SourceUnit(children: List[SuryaObject]) extends SuryaObject

  case class PragmaDirective(name: String, value: String) extends SuryaObject

  // TODO: Extend for unit and symbol aliases
  case class ImportDirective(path: String) extends SuryaObject

  case class ContractDefinition(name: String, baseContracts: List[SuryaObject], subNodes: List[SuryaObject], kind: String) extends SuryaObject 

  case class InheritanceSpecifier(baseName: List[SuryaObject]) extends SuryaObject

  case class UserDefinedTypeName(namePath: String) extends SuryaObject

  case class ModifierDefinition(name: String, paramaters: List[SuryaObject], body: SuryaObject, isVirtual: Boolean) extends SuryaObject

  case class VariableDeclaration(typeName: SuryaObject, name: String, identifier: SuryaObject, storageLocation: String,
                                 isStateVar: Boolean, isIndexed: Boolean, expression: SuryaObject) extends SuryaObject

  case class ElementaryTypeName(name: String) extends SuryaObject

  case class Identifier(name: String) extends SuryaObject

  case class Block(statements: [SuryaObject]) extends SuryaObject

  case class ExpressionStatement(expression: SuryaObject) extends SuryaObject

  case class FunctionCall(expression: SuryaObject, arguments: [SuryaObject]) extends SuryaObject

  case class MemberAccess(expression: [SuryaObject], memberName: String) extends SuryaObject

  case class IndexAccess(base: SuryaObject, index: SuryaObject) extends SuryaObject

  case class BinaryOperation(operator: String, left: SuryaObject, right: SuryaObject) extends SuryaObject

  case class StringLiteral(value: String, parts: [String], isUnicode: [Boolean]) extends SuryaObject

  case class EventDefinition(name: String, paramaters: [SuryaObject], isAnonymous: Boolean) extends SuryaObject

  case class FunctionDefinition(name: String, paramaters: [SuryaObject], returnParameters: SuryaObject,
                                body: SuryaObject, visibility: String, modifiers: [SuryaObject], 
                                override: Boolean, isConstructor: Boolean, isReceiveEther: Boolean,
                                isFallback: Boolean, isVirtual: Boolean, stateMutability: Boolean) extends SuryaObject

  case class ModifierInvocation(name: String, arguments: [SuryaObject]) extends SuryaObject

  case class EmitStatement(eventCall: SuryaObject) extends SuryaObject

  case class ForeStatement(initExpression: SuryaObject, conditionExpression: SuryaObject, loopExpression: SuryaObject,
                           body: SuryaObject) extends SuryaObject

  case class VariableDeclarationStatement(variables: [SuryaObject], initialValue: SuryaObject) extends SuryaObject

  case class UnaryOperation(operator: String, subExpression: SuryaObject, isPrefix: Boolean) extends SuryaObject

  case class IfStatement(condition: SuryaObject, trueBody: SuryaObject, falseBody: SuryaObject) extends SuryaObject

  case class BooleanLiteral(value: Boolean) extends SuryaObject

  case class ArrayTypeName(baseTypeName: SuryaObject, length: Int) extends SuryaObject 

  case class NumberLiteral(number: Int, subdenomination: Int) extends SuryaObject



  // TODO: Create the rest of these
}
