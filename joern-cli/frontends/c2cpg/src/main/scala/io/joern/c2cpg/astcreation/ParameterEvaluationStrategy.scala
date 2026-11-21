package io.joern.c2cpg.astcreation

import io.shiftleft.codepropertygraph.generated.EvaluationStrategies
import org.eclipse.cdt.core.dom.ast.*
import org.eclipse.cdt.core.dom.ast.cpp.*
import org.eclipse.cdt.internal.core.dom.parser.c.CVisitor
import org.eclipse.cdt.internal.core.dom.parser.cpp.semantics.{CPPVisitor, TypeTraits}

import scala.annotation.tailrec
import scala.util.Try

/** The evaluation strategy of a C or C++ parameter, decided from the type CDT resolves for its declarator.
  *
  * A reference parameter is `BY_REFERENCE`. A parameter is `BY_VALUE` when the function receives a copy that shares
  * nothing with the argument: an arithmetic or enumeration type, or a struct, union or class whose members are all of
  * such types and whose copy constructor is trivial, so that making the copy runs no code that could write the
  * argument. Every other parameter is `BY_SHARING`: a pointer, an array or a function, which the function receives as a
  * pointer, a struct, union or class with a pointer or reference member or a user-provided copy constructor, and a type
  * CDT does not resolve or that depends on a template parameter, which may be any of these.
  */
private[astcreation] object ParameterEvaluationStrategy {

  /** Bounds the members followed into nested structs, unions and classes. */
  private val MaxDepth = 16

  def of(declarator: IASTDeclarator): String =
    Try {
      declarator.getTranslationUnit match {
        case _: ICPPASTTranslationUnit => CPPVisitor.createType(declarator)
        case _                         => CVisitor.createType(declarator)
      }
    }.toOption.flatMap(Option(_)).map(of).getOrElse(EvaluationStrategies.BY_SHARING)

  private def of(tpe: IType): String =
    strip(tpe) match {
      case _: ICPPReferenceType             => EvaluationStrategies.BY_REFERENCE
      case pack: ICPPParameterPackType      => of(pack.getType)
      case _: IArrayType | _: IFunctionType => EvaluationStrategies.BY_SHARING
      case other if sharesNothing(other, 0) => EvaluationStrategies.BY_VALUE
      case _                                => EvaluationStrategies.BY_SHARING
    }

  /** Whether a copy of a value of type `tpe` shares nothing with the value it is copied from. */
  private def sharesNothing(tpe: IType, depth: Int): Boolean =
    depth <= MaxDepth && (strip(tpe) match {
      case basic: IBasicType         => basic.getKind != IBasicType.Kind.eUnspecified
      case _: IEnumeration           => true
      case array: IArrayType         => sharesNothing(array.getType, depth + 1)
      case classType: ICPPClassType  => classSharesNothing(classType, depth)
      case composite: ICompositeType =>
        Try(composite.getFields.toSeq).toOption.exists(_.forall(fieldSharesNothing(_, depth)))
      case _ => false
    })

  private def classSharesNothing(classType: ICPPClassType, depth: Int): Boolean =
    Try {
      val bases = classType.getBases
      bases != ICPPBase.NO_BASES_BECAUSE_TYPE_IS_INCOMPLETE &&
      TypeTraits.hasTrivialCopyCtor(classType) &&
      bases.forall { base =>
        base.getBaseClass match {
          case baseClass: ICPPClassType => classSharesNothing(baseClass, depth + 1)
          case _                        => false
        }
      } &&
      classType.getDeclaredFields.forall {
        case field if field.isStatic => true
        case field                   => fieldSharesNothing(field, depth)
      }
    }.getOrElse(false)

  private def fieldSharesNothing(field: IField, depth: Int): Boolean =
    field match {
      case _: IProblemBinding => false
      case _                  => Try(field.getType).toOption.exists(sharesNothing(_, depth + 1))
    }

  /** `tpe` without its typedefs and `const` and `volatile` qualifiers. */
  @tailrec
  private def strip(tpe: IType, steps: Int = MaxDepth): IType =
    tpe match {
      case typedef: ITypedef if steps > 0         => strip(typedef.getType, steps - 1)
      case qualified: IQualifierType if steps > 0 => strip(qualified.getType, steps - 1)
      case other                                  => other
    }

}
