package io.joern.dataflowengineoss.passes.reachingdef

import io.joern.x2cpg.Defines as X2CpgDefines
import io.shiftleft.codepropertygraph.generated.{Cpg, DispatchTypes, EvaluationStrategies, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

import scala.annotation.tailrec
import scala.collection.mutable

/** The arguments of the calls in `method` that a call cannot write, because the callee receives them by value.
  *
  * A call cannot write a variable it receives through a parameter whose evaluation strategy is `BY_VALUE`, as c2cpg
  * sets it when the system property `joern.byValueArguments` is `true`: the callee gets a copy that shares nothing with
  * the variable. So a call of a function does not define an identifier argument when every method the call may invoke
  * takes it through such a parameter, and initializing the parameter from the argument runs no code of the program. In
  * C++ a conversion function of the argument's class, or a constructor of the parameter's, may write the argument, so
  * outside a `.c` file, where the parameter's type is not the argument's, typedefs resolved, both must be of an
  * arithmetic type or of a declared type with no member functions, such as an enumeration or a C struct. The variable
  * must be one only this method can write: a local or a parameter of the method, not a variable it shares with other
  * code (a captured or global variable, or one with a storage class such as `static`), and not one whose address the
  * method takes. In C++ that excludes a variable a reference may outlive the call that bound it to: one passed to a
  * parameter that does not take it by value, bound to a reference variable, or captured by reference. A local the
  * frontend made for a name the method does not declare does not qualify: a static member of a class named without its
  * class, or a name the frontend could not resolve, which may be a global declared in a header that was not parsed.
  * Operators keep their own semantics, and a macro may assign its arguments, so only calls of functions qualify. A
  * callee the graph only has a stub for, an argument that a variadic parameter absorbs, and a call that invokes no
  * known method keep their definitions.
  */
private[reachingdef] class ByValueArguments(method: Method) {

  /** Bounds the typedefs followed to resolve a type. */
  private val MaxAliasSteps = 16

  /** The words that spell the arithmetic types of C and C++, as in `unsigned long long`. */
  private val ArithmeticTypeWords = Set(
    "_Bool",
    "_Complex",
    "__int128",
    "bool",
    "char",
    "char16_t",
    "char32_t",
    "char8_t",
    "double",
    "float",
    "int",
    "long",
    "short",
    "signed",
    "unsigned",
    "wchar_t"
  )

  /** The locals and parameters of `method` whose address the method takes, as in `&x` or `&x.field`. */
  private lazy val addressTaken: Set[Declaration] =
    method.call
      .nameExact(Operators.addressOf)
      .argument
      .ast
      .isIdentifier
      .flatMap(_.refsTo)
      .toSet

  /** In C++, the locals and parameters of `method` that a reference may outlive the call that bound it to: those passed
    * to a parameter that does not take them by value, bound to a reference variable, or captured by reference. The
    * callee or the holder of the reference may keep their address, and a later call may write them through it. An
    * argument a `...` absorbs is copied, so it binds no reference.
    */
  private lazy val referenceBound: Set[Declaration] =
    if (isCSource) Set.empty
    else {
      val passed = method.call.filter(isFunctionCall).flatMap { call =>
        call.argument.isIdentifier.filterNot(argument => copiesArgument(call, argument.argumentIndex)).flatMap(_.refsTo)
      }
      val bound = method.call
        .nameExact(Operators.assignment)
        .filter(_.argument(1) match {
          case target: Identifier => target.typeFullName.endsWith("&")
          case _                  => false
        })
        .argument(2)
        .isIdentifier
        .flatMap(_.refsTo)
      val capturedLocals     = method.local.filter(local => isByReference(local._closureBindingViaRefIn))
      val capturedParameters = method.parameter.filter(parameter => isByReference(parameter._closureBindingViaRefIn))
      (passed ++ bound ++ capturedLocals ++ capturedParameters).toSet
    }

  private def isByReference(bindings: Iterator[ClosureBinding]): Boolean =
    bindings.exists(_.evaluationStrategy != EvaluationStrategies.BY_VALUE)

  /** Whether `call` receives `argument` by value, so that the call does not define it. */
  def isPassedByValue(call: Call, argument: Expression): Boolean =
    isFunctionCall(call) && (argument match {
      case identifier: Identifier => isUnshared(identifier) && calleesTakeByValue(call, identifier)
      case _                      => false
    })

  private def isFunctionCall(call: Call): Boolean =
    !call.name.startsWith("<operator") && call.dispatchType != DispatchTypes.INLINED

  private def isUnshared(identifier: Identifier): Boolean =
    identifier.refsTo.l match {
      case List(local: Local) =>
        local.closureBindingId.isEmpty && !hasStorageClass(local._tagViaTaggedByOut) && !addressTaken.contains(local) &&
        !isUndeclaredInMethod(local) && !referenceBound.contains(local)
      case List(param: MethodParameterIn) =>
        !hasStorageClass(param._tagViaTaggedByOut) && !addressTaken.contains(param) && !referenceBound.contains(param)
      case _ => false
    }

  /** Whether `local` stands for a name `method` does not declare. The frontend's scope makes such a local, with order
    * 0, for each name it finds no declaration of in the method's scopes; a declared local has its place in its block.
    */
  private def isUndeclaredInMethod(local: Local): Boolean =
    local.order == 0

  private def hasStorageClass(tags: Iterator[Tag]): Boolean =
    tags.exists(_.name == X2CpgDefines.StorageClassTag)

  private def calleesTakeByValue(call: Call, argument: Identifier): Boolean = {
    lazy val argumentType = resolvedType(argument.typeFullName)
    takesByValue(call, argument.argumentIndex) && callees(call).forall { callee =>
      callee.parameter.index(argument.argumentIndex).l.exists { parameter =>
        isCSource || {
          val parameterType = resolvedType(parameter.typeFullName)
          parameterType == argumentType || (convertsWithoutCode(argumentType) && convertsWithoutCode(parameterType))
        }
      }
    }
  }

  /** Whether every method `call` may invoke is known and takes its argument at `index` through a `BY_VALUE`,
    * non-variadic parameter.
    */
  private def takesByValue(call: Call, index: Int): Boolean = {
    val methods = callees(call)
    methods.nonEmpty && methods.forall { callee =>
      !callee.isExternal && callee.parameter.index(index).l.exists { parameter =>
        !parameter.isVariadic && parameter.evaluationStrategy == EvaluationStrategies.BY_VALUE
      }
    }
  }

  /** Whether every method `call` may invoke is known and receives its argument at `index` as a copy: through a
    * `BY_VALUE` parameter, or through a `...`.
    */
  private def copiesArgument(call: Call, index: Int): Boolean = {
    val methods = callees(call)
    methods.nonEmpty && methods.forall { callee =>
      !callee.isExternal && {
        val parameters = callee.parameter.l
        parameters.exists(parameter =>
          parameter.index == index && parameter.evaluationStrategy == EvaluationStrategies.BY_VALUE
        ) ||
        parameters.exists(parameter => parameter.isVariadic && parameter.index <= index)
      }
    }
  }

  private val calleesByCall = mutable.Map.empty[Call, List[Method]]

  private def callees(call: Call): List[Method] =
    calleesByCall.getOrElseUpdate(call, NoResolve.getCalledMethods(call).l)

  private lazy val cpg = Cpg(method.graph)

  /** Whether `method` is in a C source file, where no conversion function exists. */
  private lazy val isCSource: Boolean = method.filename.endsWith(".c")

  /** Whether converting a value of type `typeFullName`, typedefs resolved, runs no code of the program: it is an
    * arithmetic type, or a type the graph declares with no member functions, so that no conversion function exists.
    */
  private def convertsWithoutCode(typeFullName: String): Boolean =
    typeFullName.split(' ').forall(ArithmeticTypeWords.contains) ||
      cpg.typeDecl.fullNameExact(typeFullName).isExternal(false).l.exists(_._bindingViaBindsOut.isEmpty)

  /** `typeFullName` with its typedefs resolved, through the type declarations' alias types. */
  private def resolvedType(typeFullName: String): String = {
    @tailrec
    def resolve(name: String, steps: Int): String =
      cpg.typeDecl.fullNameExact(name).aliasTypeFullName.headOption match {
        case Some(alias) if steps > 0 && alias != name => resolve(alias, steps - 1)
        case _                                         => name
      }
    resolve(typeFullName, MaxAliasSteps)
  }

}
