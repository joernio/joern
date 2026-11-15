package io.joern.c2cpg.astcreation

import org.eclipse.cdt.core.dom.ast.*
import org.eclipse.cdt.core.dom.ast.cpp.*

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Try

/** Finds the scalar variables of a translation unit that a call cannot write.
  *
  * A function receives a copy of an argument passed to a parameter of scalar type, or through `...`, so it cannot write
  * the variable passed. It can write a variable through the variable's address, or through a reference bound to it. A
  * scalar variable is passed by value when neither can happen: its address is never taken, and it is never bound to a
  * reference, by a call or otherwise. Parameter types are those of the declaration CDT resolves the call to in this
  * translation unit. Where CDT resolves no declaration, as for a call of an undeclared function or of a macro whose
  * definition was not parsed, the variable is not passed by value.
  */
private[astcreation] class ByValueVariables(translationUnit: IASTTranslationUnit) {

  /** Whether `name` declares a variable of scalar type that is passed by value. */
  def isPassedByValue(name: IASTName): Boolean =
    Try(name.resolveBinding()).toOption.exists {
      case variable: IVariable => isScalar(variable.getType) && !writableByCall.contains(variable)
      case _                   => false
    }

  /** The function definition or lambda in which each variable of the translation unit is declared. */
  private lazy val declaringFunction: Map[IBinding, IASTNode] = {
    val functions = mutable.Map.empty[IBinding, IASTNode]
    translationUnit.accept(new ASTVisitor() {
      shouldVisitDeclarators = true
      override def visit(declarator: IASTDeclarator): Int = {
        variableOf(declarator.getName).foreach { variable =>
          enclosingFunction(declarator).foreach(functions.put(variable, _))
        }
        ASTVisitor.PROCESS_CONTINUE
      }
    })
    functions.toMap
  }

  private lazy val writableByCall: Set[IBinding] = {
    val writable = mutable.Set.empty[IBinding]
    translationUnit.accept(new ASTVisitor() {
      shouldVisitExpressions = true
      override def visit(expression: IASTExpression): Int = {
        expression match {
          case idExpression: IASTIdExpression =>
            variableOf(idExpression.getName).filter(mayBeWritten(idExpression, _)).foreach(writable.add)
          case _ =>
        }
        ASTVisitor.PROCESS_CONTINUE
      }
    })
    writable.toSet
  }

  private def variableOf(name: IASTName): Option[IBinding] =
    Option(name).flatMap(name => Try(name.resolveBinding()).toOption).collect { case variable: IVariable => variable }

  private def mayBeWritten(use: IASTIdExpression, variable: IBinding): Boolean = {
    val operand = outermostOperand(use)
    usedFromNestedFunction(use, variable) || (operand.getParent match {
      case unary: IASTUnaryExpression if unary.getOperator == IASTUnaryExpression.op_amper => true
      case call: IASTFunctionCallExpression                                                =>
        val index = call.getArguments.indexWhere(_ eq operand)
        index >= 0 && !takesByValue(functionTypeOf(call), index)
      case binary: ICPPASTBinaryExpression if binary.getOverload != null =>
        val position = if (binary.getOperand1 eq operand) 0 else 1
        !overloadTakesByValue(binary.getOverload, position)
      case unary: ICPPASTUnaryExpression if unary.getOverload != null =>
        !overloadTakesByValue(unary.getOverload, 0)
      case cast: ICPPASTCastExpression => cast.getValueCategory != IASTExpression.ValueCategory.PRVALUE
      case _: IASTReturnStatement      => returnsReference(operand)
      case _: IASTInitializer | _: IASTInitializerList if isCpp => initializerMayBindReference(operand)
      case _                                                    => false
    })
  }

  private def isCpp: Boolean = translationUnit.isInstanceOf[ICPPASTTranslationUnit]

  /** The expression whose value `use` is: `use` itself, or a parenthesized expression, or in C++ a conditional or comma
    * expression, whose result may be `use` itself.
    */
  private def outermostOperand(use: IASTExpression): IASTExpression = {
    var operand: IASTExpression = use
    var climbing                = true
    while (climbing) {
      operand.getParent match {
        case parent: IASTUnaryExpression if parent.getOperator == IASTUnaryExpression.op_bracketedPrimary =>
          operand = parent
        case parent: IASTConditionalExpression if isCpp && !(parent.getLogicalConditionExpression eq operand) =>
          operand = parent
        case parent: IASTExpressionList if isCpp && parent.getExpressions.lastOption.exists(_ eq operand) =>
          operand = parent
        case _ => climbing = false
      }
    }
    operand
  }

  /** Whether `use` is inside a lambda that captures the variable by reference, or inside a function nested in the one
    * that declares the variable.
    */
  private def usedFromNestedFunction(use: IASTNode, variable: IBinding): Boolean =
    declaringFunction.get(variable) match {
      case None              => false
      case Some(declaringFn) =>
        Iterator
          .iterate(use.getParent)(_.getParent)
          .takeWhile(node => node != null && !(node eq declaringFn))
          .exists {
            case lambda: ICPPASTLambdaExpression =>
              val explicit =
                lambda.getCaptures.find(capture => Option(capture.getIdentifier).flatMap(variableOf).contains(variable))
              explicit.map(_.isByReference).getOrElse {
                lambda.getCaptureDefault == ICPPASTLambdaExpression.CaptureDefault.BY_REFERENCE
              }
            case _: IASTFunctionDefinition => true
            case _                         => false
          }
    }

  private def enclosingFunction(node: IASTNode): Option[IASTNode] =
    Iterator.iterate(node.getParent)(_.getParent).takeWhile(_ != null).collectFirst {
      case function: IASTFunctionDefinition => function
      case lambda: ICPPASTLambdaExpression  => lambda
    }

  /** The type of the function `call` calls: an overloaded `operator()`, the function or function pointer it names, or
    * in C++ the constructor of the class it names, as in `A(x)`.
    */
  private def functionTypeOf(call: IASTFunctionCallExpression): Option[IFunctionType] =
    call match {
      case cppCall: ICPPASTFunctionCallExpression if cppCall.getOverload != null => Option(cppCall.getOverload.getType)
      case _                                                                     =>
        Try(call.getFunctionNameExpression.getExpressionType).toOption.flatMap(functionTypeOf).orElse {
          constructorOf(call).map(_.getType)
        }
    }

  private def functionTypeOf(tpe: IType): Option[IFunctionType] =
    tpe match {
      case function: IFunctionType   => Some(function)
      case typedef: ITypedef         => functionTypeOf(typedef.getType)
      case qualified: IQualifierType => functionTypeOf(qualified.getType)
      case pointer: IPointerType     => functionTypeOf(pointer.getType)
      case _                         => None
    }

  private def takesByValue(functionType: Option[IFunctionType], index: Int): Boolean =
    functionType.exists { function =>
      val parameterTypes = function.getParameterTypes
      if (index < parameterTypes.length) isScalar(parameterTypes(index)) else function.takesVarArgs
    }

  /** A member operator receives its left operand as the object it is called on, so a scalar operand reaches it only as
    * its first parameter; a non-member operator receives each operand as a parameter.
    */
  private def overloadTakesByValue(overload: ICPPFunction, position: Int): Boolean =
    overload match {
      case method: ICPPMethod if !method.isStatic => position == 1 && takesByValue(Option(method.getType), 0)
      case function                               => takesByValue(Option(function.getType), position)
    }

  private def returnsReference(operand: IASTNode): Boolean =
    isCpp && enclosingFunction(operand).exists {
      case function: IASTFunctionDefinition =>
        Try(function.getDeclarator.getName.resolveBinding()).toOption.exists {
          case binding: IFunction => binding.getType.getReturnType.isInstanceOf[ICPPReferenceType]
          case _                  => true
        }
      case lambda: ICPPASTLambdaExpression =>
        Option(lambda.getDeclarator).flatMap(declarator => Option(declarator.getTrailingReturnType)).nonEmpty
      case _ => true
    }

  /** In C++, an initializer copies `operand` into a variable, member or object of scalar type, and may bind it to a
    * reference otherwise: the one being initialized, or a parameter of the constructor it calls.
    */
  private def initializerMayBindReference(operand: IASTNode): Boolean = {
    val (owner, argument, nested) = initializedBy(operand)
    owner match {
      case declarator: IASTDeclarator if isInitCaptureByReference(declarator) => true
      case declarator: IASTDeclarator                                         =>
        variableOf(declarator.getName) match {
          case Some(variable: IVariable) if isScalar(variable.getType) || isPointer(variable.getType) => false
          case _ => !constructorTakesByValue(declarator, argument, nested)
        }
      case chain: ICPPASTConstructorChainInitializer =>
        Try(chain.getMemberInitializerId.resolveBinding()).toOption match {
          case Some(member: IVariable) if isScalar(member.getType) || isPointer(member.getType) => false
          case _ => !constructorTakesByValue(chain, argument, nested)
        }
      case construction: ICPPASTSimpleTypeConstructorExpression =>
        !isScalar(construction.getExpressionType) && !constructorTakesByValue(construction, argument, nested)
      case newExpression: ICPPASTNewExpression =>
        !isScalar(newExpression.getExpressionType match {
          case pointer: IPointerType => pointer.getType
          case other                 => other
        }) && !constructorTakesByValue(newExpression, argument, nested)
      case _ => true
    }
  }

  /** Whether `declarator` declares an init-capture such as `[&r = x]`, which binds `r` to `x`. */
  private def isInitCaptureByReference(declarator: IASTDeclarator): Boolean =
    declarator.getParent match {
      case capture: ICPPASTInitCapture => capture.isByReference
      case _                           => false
    }

  /** The node that `operand` initializes, the index of the argument it is in, and whether it is nested more deeply than
    * an argument, as in `{{x}}`.
    */
  private def initializedBy(operand: IASTNode): (IASTNode, Int, Boolean) = {
    var child: IASTNode = operand
    var node: IASTNode  = operand.getParent
    var argument        = 0
    var depth           = 0
    while (isInitializer(node)) {
      node match {
        case list: IASTInitializerList =>
          argument = list.getClauses.indexWhere(_ eq child)
          depth += 1
        case constructor: ICPPASTConstructorInitializer =>
          argument = constructor.getArguments.indexWhere(_ eq child)
          depth += 1
        case _ =>
      }
      child = node
      node = node.getParent
    }
    (node, argument, depth > 1)
  }

  private def isInitializer(node: IASTNode): Boolean =
    node match {
      case _: ICPPASTConstructorChainInitializer                                         => false
      case _: IASTInitializer | _: IASTInitializerList | _: ICPPASTDesignatedInitializer => true
      case _                                                                             => false
    }

  private def constructorTakesByValue(owner: IASTNode, argument: Int, nested: Boolean): Boolean =
    !nested && constructorOf(owner).exists(constructor => takesByValue(Option(constructor.getType), argument))

  /** The constructor CDT resolves `owner` to call, such as the one a declaration `A a(x);` calls. */
  private def constructorOf(owner: IASTNode): Option[ICPPConstructor] =
    owner match {
      case implicitNameOwner: IASTImplicitNameOwner =>
        implicitNameOwner.getImplicitNames
          .flatMap(name => Try(name.resolveBinding()).toOption)
          .collectFirst { case constructor: ICPPConstructor => constructor }
      case _ => None
    }

  private def isPointer(tpe: IType): Boolean =
    strip(tpe).isInstanceOf[IPointerType]

  /** An arithmetic or enumeration type, or a typedef of one; not a reference. */
  private def isScalar(tpe: IType): Boolean =
    strip(tpe) match {
      case basic: IBasicType =>
        basic.getKind != IBasicType.Kind.eVoid && basic.getKind != IBasicType.Kind.eUnspecified
      case _: IEnumeration => true
      case _               => false
    }

  /** `tpe` without its typedefs and `const` and `volatile` qualifiers. */
  @tailrec
  private def strip(tpe: IType, steps: Int = 16): IType =
    tpe match {
      case typedef: ITypedef if steps > 0         => strip(typedef.getType, steps - 1)
      case qualified: IQualifierType if steps > 0 => strip(qualified.getType, steps - 1)
      case other                                  => other
    }

}
