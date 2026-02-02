package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.Constants
import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.{Call, FieldIdentifier, Identifier, MethodRef}
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.codepropertygraph.generated.nodes.Binding

class CallableReferenceTests extends KotlinCode2CpgFixture(withOssDataflow = false) {

  "SAM implementation for bound instance method reference" should {
    val cpg = code("""
        |package com.test
        |
        |class Handler {
        |    fun process(x: Int, y: String): Boolean {
        |        return true
        |    }
        |}
        |
        |fun test() {
        |   val handler = Handler()
        |   val ref: (Int, String) -> Boolean = handler::process
        |}
        |""".stripMargin)

    val syntheticTypeDecl = cpg.typeDecl.fullName(".*Function2Impl.*").head
    val invokeMethod      = syntheticTypeDecl.method.name("invoke").head

    "inherit from Function interface and CallableReference" in {
      syntheticTypeDecl.inheritsFromTypeFullName.exists(_.contains("Function2")) shouldBe true
      syntheticTypeDecl.inheritsFromTypeFullName.exists(_.contains("CallableReference")) shouldBe true
    }

    "resolve generic types to concrete types in signature" in {
      invokeMethod.signature shouldBe "boolean(int,java.lang.String)"
    }

    "have correct parameters with resolved types" in {
      val params = invokeMethod.parameter.l.sortBy(_.index)
      params.size shouldBe 3
      params.head.name shouldBe "this"
      params(1).typeFullName shouldBe "int"
      params(2).typeFullName shouldBe "java.lang.String"
    }

    "forward all parameters to the referenced method" in {
      val processCall   = invokeMethod.ast.isCall.name("process").head
      val forwardedArgs = processCall.argument.isIdentifier.l
      forwardedArgs.size shouldBe 2
      forwardedArgs.map(_.typeFullName).toSet shouldBe Set("int", "java.lang.String")
    }

    "use DYNAMIC_DISPATCH with field access to receiver" in {
      val processCall = invokeMethod.ast.isCall.name("process").head
      processCall.dispatchType shouldBe "DYNAMIC_DISPATCH"

      val receiverAccess = processCall.receiver.isCall.name(Operators.fieldAccess).head
      receiverAccess.typeFullName shouldBe "com.test.Handler"
      receiverAccess.ast.isFieldIdentifier.canonicalName.head shouldBe "receiver"
      receiverAccess.ast.isIdentifier
        .name("this")
        .head
        .typeFullName shouldBe "com.test.Handler.process$kotlin.jvm.functions.Function2Impl"
    }

    "create a constructor call for the synthetic type with receiver as parameter" in {
      val ctorCalls = cpg.call.nameExact("<init>").methodFullName(".*Function2Impl.*<init>.*").l
      ctorCalls.size shouldBe 1

      val ctorCall = ctorCalls.head
      ctorCall.methodFullName shouldBe "com.test.Handler.process$kotlin.jvm.functions.Function2Impl.<init>:void(kotlin.jvm.functions.Function2)"
      ctorCall.typeFullName shouldBe "com.test.Handler.process$kotlin.jvm.functions.Function2Impl"

      val args = ctorCall.argument.l

      val receiverArg = args(1).asInstanceOf[Identifier]
      receiverArg.name shouldBe "handler"
      receiverArg.typeFullName shouldBe "com.test.Handler"
    }
  }

  "SAM implementation for static companion method reference" should {
    val cpg = code("""
        |package com.test
        |
        |class Utils {
        |    companion object {
        |        fun validate(x: Int): Boolean = x > 0
        |    }
        |}
        |
        |fun test() {
        |   val ref: (Int) -> Boolean = Utils::validate
        |}
        |""".stripMargin)

    val syntheticTypeDecl = cpg.typeDecl.fullName(".*Function1Impl.*").head
    val invokeMethod      = syntheticTypeDecl.method.name("invoke").head

    "use STATIC_DISPATCH without receiver access" in {
      val validateCall = invokeMethod.ast.isCall.name("validate").head
      validateCall.dispatchType shouldBe "STATIC_DISPATCH"
      validateCall.receiver.l shouldBe empty
    }

    "not generate field access for static methods" in {
      val fieldAccesses = invokeMethod.ast.isCall.name(Operators.fieldAccess).l
      fieldAccesses shouldBe empty
    }

    "create a constructor call for the synthetic type with companion object as parameter" in {
      val ctorCalls = cpg.call.nameExact("<init>").methodFullName(".*Function1Impl.*<init>.*").l
      ctorCalls.size shouldBe 1

      val ctorCall = ctorCalls.head
      ctorCall.methodFullName should include("Function1Impl")
      ctorCall.typeFullName should include("Function1Impl")

      val args = ctorCall.argument.l
      args.size shouldBe 2

      val receiverArg = args(1).asInstanceOf[Call]
      receiverArg.name shouldBe Operators.fieldAccess
      receiverArg.typeFullName shouldBe "com.test.Utils$Companion"

      val List(receiverBase: Identifier, receiverField: FieldIdentifier) = receiverArg.argument.l: @unchecked
      receiverBase.name shouldBe "Utils"
      receiverBase.typeFullName shouldBe "com.test.Utils$Companion"
      receiverField.canonicalName shouldBe Constants.CompanionObjectMemberName
    }
  }

  "SAM implementation for top-level function reference" should {
    val cpg = code("""
        |package com.test
        |
        |fun globalFunction(x: Int, y: Int): String {
        |    return "result"
        |}
        |
        |fun test() {
        |   val ref: (Int, Int) -> String = ::globalFunction
        |}
        |""".stripMargin)

    "create a MethodRef node to the referenced function" in {
      val methodRefs = cpg.methodRef.methodFullName(".*globalFunction.*").l
      methodRefs.size shouldBe 1

      val methodRef = methodRefs.head
      methodRef.methodFullName shouldBe "com.test.globalFunction:java.lang.String(int,int)"
    }

    "create a synthetic type that does NOT inherit from CallableReference" in {
      val syntheticTypes = cpg.typeDecl.fullName(".*globalFunction.*Function2.*").l
      syntheticTypes.size shouldBe 1

      val syntheticType = syntheticTypes.head
      syntheticType.inheritsFromTypeFullName.exists(_.contains("Function2")) shouldBe true
      syntheticType.inheritsFromTypeFullName.exists(_.contains("CallableReference")) shouldBe false
    }

    "create a binding from the synthetic type to the original function" in {
      val bindings = cpg.all
        .collectAll[Binding]
        .nameExact("invoke")
        .l
        .filter(_.bindingTypeDecl.fullName.contains("globalFunction"))
      bindings.size shouldBe 1

      val binding = bindings.head
      binding.methodFullName shouldBe "com.test.globalFunction:java.lang.String(int,int)"
      binding.signature shouldBe "java.lang.String(int,int)"
    }

    "not create an invoke method that calls the function, only the binding" in {
      val syntheticTypes = cpg.typeDecl.fullName(".*globalFunction.*Function2.*").l
      val syntheticType  = syntheticTypes.head
      val invokeMethods  = syntheticType.method.name("invoke").l
      // The invoke method should not be created in this case
      invokeMethods.isEmpty shouldBe true
    }
  }

  "SAM implementation with custom interfaces" should {
    val cpg = code("""
        |package com.test
        |
        |interface Transformer<T, R> {
        |    fun transform(input: T): R
        |}
        |
        |interface SimpleAction {
        |    fun execute(value: Int)
        |}
        |
        |class Processor {
        |    fun toInt(s: String): Int = s.length
        |    fun log(n: Int) { println(n) }
        |}
        |
        |fun test() {
        |    val proc = Processor()
        |    val t: Transformer<String, Int> = proc::toInt
        |    val a: SimpleAction = proc::log
        |}
        |""".stripMargin)

    "generate type implementing custom SAM interface with resolved generics" in {
      val transformerImpl = cpg.typeDecl.fullName(".*TransformerImpl.*").head
      transformerImpl.inheritsFromTypeFullName.exists(_.contains("Transformer")) shouldBe true

      val transformMethod = transformerImpl.method.name("transform").head
      transformMethod.signature shouldBe "int(java.lang.String)"

      val toIntCall = transformMethod.ast.isCall.name("toInt").head
      toIntCall.signature shouldBe "int(java.lang.String)"
    }

    "handle void return types in custom interfaces" in {
      val actionImpl    = cpg.typeDecl.fullName(".*SimpleActionImpl.*").head
      val executeMethod = actionImpl.method.name("execute").head
      executeMethod.signature shouldBe "void(int)"
    }
  }

  "SAM implementation edge cases" should {
    "handle methods with no parameters" in {
      val cpg = code("""
          |class Counter {
          |    fun increment(): Int = 1
          |}
          |
          |fun test() {
          |   val counter = Counter()
          |   val ref: () -> Int = counter::increment
          |}
          |""".stripMargin)

      val syntheticTypeDecl = cpg.typeDecl.fullName(".*Function0Impl.*").head
      val invokeMethod      = syntheticTypeDecl.method.name("invoke").head

      invokeMethod.signature shouldBe "int()"
      invokeMethod.parameter.filter(_.index > 0).size shouldBe 0

      val incrementCall = invokeMethod.ast.isCall.name("increment").head
      incrementCall.argument.isIdentifier.size shouldBe 0
    }

    "handle nullable parameter types" in {
      val cpg = code("""
          |class Handler {
          |    fun process(value: String?): Boolean {
          |        return value != null
          |    }
          |}
          |
          |fun test() {
          |   val handler = Handler()
          |   val ref: (String?) -> Boolean = handler::process
          |}
          |""".stripMargin)

      val syntheticTypeDecl = cpg.typeDecl.fullName(".*Function1Impl.*").head
      val invokeMethod      = syntheticTypeDecl.method.name("invoke").head

      val param = invokeMethod.parameter.index(1).head
      param.typeFullName should include("String")

      val processCall = invokeMethod.ast.isCall.name("process").head
      processCall.argument.isIdentifier.size shouldBe 1
    }

    "handle nullable return types" in {
      val cpg = code("""
          |class Finder {
          |    fun find(id: Int): String? {
          |        return null
          |    }
          |}
          |
          |fun test() {
          |   val finder = Finder()
          |   val ref: (Int) -> String? = finder::find
          |}
          |""".stripMargin)

      val syntheticTypeDecl = cpg.typeDecl.fullName(".*Function1Impl.*").head
      val invokeMethod      = syntheticTypeDecl.method.name("invoke").head

      invokeMethod.signature should include("String")

      val findCall = invokeMethod.ast.isCall.name("find").head
      findCall.signature should include("String")
    }

    "handle references with 'this' receiver" in {
      val cpg = code("""
          |package com.test
          |
          |class MyClass {
          |    fun method(x: Int) {}
          |    
          |    fun setup() {
          |        val ref: (Int) -> Unit = this::method
          |    }
          |}
          |""".stripMargin)

      val syntheticTypeDecl = cpg.typeDecl.fullName(".*Function1Impl.*").head
      val invokeMethod      = syntheticTypeDecl.method.name("invoke").head

      val methodCall = invokeMethod.ast.isCall.name("method").head
      methodCall.dispatchType shouldBe "DYNAMIC_DISPATCH"

      val receiverAccess = methodCall.receiver.isCall.name(Operators.fieldAccess).head
      receiverAccess.ast.isFieldIdentifier.canonicalName.head shouldBe "receiver"
    }

    "handle complex nested generic types" in {
      val cpg = code("""
          |package com.test
          |
          |interface Mapper<T, R> {
          |    fun map(items: List<T>): List<R>
          |}
          |
          |class Converter {
          |    fun convertStrings(items: List<String>): List<Int> {
          |        return items.map { it.length }
          |    }
          |}
          |
          |fun test() {
          |    val converter = Converter()
          |    val mapper: Mapper<String, Int> = converter::convertStrings
          |}
          |""".stripMargin)

      val syntheticTypeDecl = cpg.typeDecl.fullName(".*MapperImpl.*").head
      syntheticTypeDecl.inheritsFromTypeFullName.exists(_.contains("Mapper")) shouldBe true

      val mapMethod = syntheticTypeDecl.method.name("map").head
      mapMethod.parameter.index(1).head.typeFullName should include("List")
    }

    "handle methods with multiple return paths" in {
      val cpg = code("""
          |class Validator {
          |    fun check(x: Int): Boolean {
          |        if (x > 0) return true
          |        return false
          |    }
          |}
          |
          |fun test() {
          |   val validator = Validator()
          |   val ref: (Int) -> Boolean = validator::check
          |}
          |""".stripMargin)

      val syntheticTypeDecl = cpg.typeDecl.fullName(".*Function1Impl.*").head
      val invokeMethod      = syntheticTypeDecl.method.name("invoke").head

      invokeMethod.signature shouldBe "boolean(int)"
      val checkCall = invokeMethod.ast.isCall.name("check").head
      checkCall.methodFullName shouldBe "Validator.check"
    }
  }

  "enforce uniqueness for duplicate SAM implementations" should {
    val cpg = code("""
          |package com.test
          |
          |class Calculator {
          |    fun add(a: Int, b: Int): Int = a + b
          |}
          |
          |fun test() {
          |   val calc1 = Calculator()
          |   val calc2 = Calculator()
          |   val ref1: (Int, Int) -> Int = calc1::add
          |   val ref2: (Int, Int) -> Int = calc2::add
          |   val ref3: (Int, Int) -> Int = calc1::add
          |}
          |""".stripMargin)

    val syntheticTypeDecls = cpg.typeDecl.fullName(".*Calculator.*Function2Impl.*").l

    "only have ONE synthetic type declaration for Calculator::add with Function2<Int, Int, Int>" in {
      syntheticTypeDecls.size shouldBe 1
    }

    val syntheticTypeDecl = syntheticTypeDecls.head

    "should have the invoke method with the correct signature" in {
      val invokeMethod = syntheticTypeDecl.method.name("invoke").head

      invokeMethod.signature shouldBe "int(int,int)"
    }

    "the single synthetic type should be used by all three constructor calls" in {
      val ctorCalls = cpg.call
        .nameExact("<init>")
        .methodFullNameExact(
          "com.test.Calculator.add$kotlin.jvm.functions.Function2Impl.<init>:void(kotlin.jvm.functions.Function2)"
        )
        .l
      ctorCalls.size shouldBe 3
    }
  }
}
