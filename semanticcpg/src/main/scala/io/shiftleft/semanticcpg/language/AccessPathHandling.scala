package io.shiftleft.semanticcpg.language

import io.shiftleft.codepropertygraph.generated.{Operators, Properties, PropertyNames}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.accesspath.*
import org.slf4j.LoggerFactory

import scala.jdk.CollectionConverters.IteratorHasAsScala

object AccessPathHandling {

  def leafToTrackedBaseAndAccessPathInternal(node: StoredNode): Option[(TrackedBase, List[AccessElement])] = {
    node match {
      case node: MethodParameterIn  => Some((TrackedNamedVariable(node.name), Nil))
      case node: MethodParameterOut => Some((TrackedNamedVariable(node.name), Nil))
      case node: Identifier         => Some((TrackedNamedVariable(node.name), Nil))
      case node: Literal            => Some((TrackedLiteral(node), Nil))
      case node: MethodRef          => Some((TrackedMethod(node), Nil))
      case node: TypeRef            => Some((TrackedTypeRef(node), Nil))
      case _: Return                => Some((TrackedFormalReturn, Nil))
      case _: MethodReturn          => Some((TrackedFormalReturn, Nil))
      case _: Unknown               => Some((TrackedUnknown, Nil))
      case _: ControlStructure      => Some((TrackedUnknown, Nil))
      // FieldIdentifiers are only fake arguments, hence should not be tracked
      case _: FieldIdentifier => Some((TrackedUnknown, Nil))
      case _                  => None
    }
  }

  private val logger                = LoggerFactory.getLogger(getClass)
  private var hasWarnedDeprecations = false

  def memberAccessToPath(memberAccess: Call, tail: List[AccessElement]): List[AccessElement] = {
    memberAccess.name match {
      case Operators.memberAccess | Operators.indirectMemberAccess =>
        if (!hasWarnedDeprecations) {
          logger.info(s"Deprecated Operator ${memberAccess.name} on $memberAccess")
          hasWarnedDeprecations = true
        }
        memberAccess
          .argumentOption(2)
          .collect {
            case node: Literal    => ConstantAccess(node.code)
            case node: Identifier => ConstantAccess(node.name)
            case other if other.propertyOption(PropertyNames.NAME).isPresent =>
              logger.warn(s"unexpected/deprecated node encountered: $other with properties: ${other.propertiesMap()}")
              ConstantAccess(other.property(Properties.Name))
          }
          .getOrElse(VariableAccess) :: tail

      case Operators.computedMemberAccess | Operators.indirectComputedMemberAccess =>
        if (!hasWarnedDeprecations) {
          logger.info(s"Deprecated Operator ${memberAccess.name} on $memberAccess")
          hasWarnedDeprecations = true
        }
        memberAccess
          .argumentOption(2)
          .collect { case lit: Literal =>
            ConstantAccess(lit.code)
          }
          .getOrElse(VariableAccess) :: tail
      case Operators.indirection =>
        IndirectionAccess :: tail
      case Operators.addressOf =>
        AddressOf :: tail
      case Operators.fieldAccess =>
        extractAccessStringTokenForFieldAccess(memberAccess) :: tail
      case Operators.indexAccess =>
        extractAccessStringToken(memberAccess) :: tail
      case Operators.indirectFieldAccess =>
        // we will reverse the list in the end
        extractAccessStringTokenForFieldAccess(memberAccess) :: IndirectionAccess :: tail
      case Operators.indirectIndexAccess =>
        // we will reverse the list in the end
        IndirectionAccess :: extractAccessIntToken(memberAccess) :: tail
      case Operators.pointerShift =>
        extractAccessIntToken(memberAccess) :: tail
      case Operators.getElementPtr =>
        // we will reverse the list in the end
        AddressOf :: extractAccessStringTokenForFieldAccess(memberAccess) :: IndirectionAccess :: tail
    }
  }

  private def extractAccessStringTokenForFieldAccess(memberAccess: Call): AccessElement = {
    memberAccess.argumentOption(2) match {
      case None =>
        logger.warn(
          s"Invalid AST: Found member access without second argument." +
            s" Member access CODE: ${memberAccess.code}" +
            s" In method ${memberAccess.method.fullName}"
        )
        VariableAccess
      case Some(literal: Literal) => ConstantAccess(literal.code)
      case Some(fieldIdentifier: FieldIdentifier) =>
        ConstantAccess(fieldIdentifier.canonicalName)
      case Some(identifier: Identifier) =>
        // TODO remove this case.
        // This is handling a very old CPG format version where IDENTIFIER was used instead of FIELD_IDENTIFIER.
        // Sadly we need this for now to support a GO cpg.
        ConstantAccess(identifier.name)
      case _ => VariableAccess
    }
  }

  private def extractAccessStringToken(memberAccess: Call): AccessElement = {
    memberAccess.argumentOption(2) match {
      case None =>
        logger.warn(
          s"Invalid AST: Found member access without second argument." +
            s" Member access CODE: ${memberAccess.code}" +
            s" In method ${memberAccess.method.fullName}"
        )
        VariableAccess
      case Some(literal: Literal) => ConstantAccess(literal.code)
      case Some(fieldIdentifier: FieldIdentifier) =>
        ConstantAccess(fieldIdentifier.canonicalName)
      case _ => VariableAccess
    }
  }

  private def extractAccessIntToken(memberAccess: Call): AccessElement = {
    memberAccess.argumentOption(2) match {
      case None =>
        logger.warn(
          s"Invalid AST: Found member access without second argument." +
            s" Member access CODE: ${memberAccess.code}" +
            s" In method ${memberAccess.method.fullName}"
        )
        VariablePointerShift
      case Some(literal: Literal) =>
        literal.code.toIntOption.map(PointerShift.apply).getOrElse(VariablePointerShift)
      case Some(fieldIdentifier: FieldIdentifier) =>
        fieldIdentifier.canonicalName.toIntOption
          .map(PointerShift.apply)
          .getOrElse(VariablePointerShift)
      case _ => VariablePointerShift
    }
  }

  def lastExpressionInBlock(block: Block): Option[Expression] =
    block._astOut
      .collect {
        case node: Expression if !node.isInstanceOf[Local] && !node.isInstanceOf[Method] => node
      }
      .toVector
      .sortBy(_.order)
      .lastOption

}
