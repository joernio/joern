package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.Constants
import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.{
  Binding,
  Call,
  FieldIdentifier,
  Identifier,
  MethodParameterIn,
  MethodRef
}
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language.*

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
      val processCall = invokeMethod.ast.isCall.name("process").head

      inside(processCall.argument.sortBy(_.argumentIndex).l) {
        // Call is checked below, focus on arguments only
        case List(_: Call, intArg: Identifier, stringArg: Identifier) =>
          intArg.name shouldBe "p1"
          intArg.typeFullName shouldBe "int"
          inside(intArg.refsTo.l) { case List(intArgParameter: MethodParameterIn) =>
            intArgParameter.name shouldBe "p1"
            intArgParameter.typeFullName shouldBe "int"
          }

          stringArg.name shouldBe "p2"
          stringArg.typeFullName shouldBe "java.lang.String"
          inside(stringArg.refsTo.l) { case List(stringArgParameter: MethodParameterIn) =>
            stringArgParameter.name shouldBe "p2"
            stringArgParameter.typeFullName shouldBe "java.lang.String"
          }
      }
    }

    "use DYNAMIC_DISPATCH with cast and field access to receiver" in {
      val processCall = invokeMethod.ast.isCall.name("process").head
      processCall.dispatchType shouldBe "DYNAMIC_DISPATCH"

      // Should have fieldAccess
      val receiverAccess = processCall.receiver.isCall.name(Operators.fieldAccess).head
      receiverAccess.typeFullName shouldBe "com.test.Handler"
      receiverAccess.ast.isFieldIdentifier.canonicalName.head shouldBe "receiver"
      receiverAccess.ast.isIdentifier
        .name("this")
        .head
        .typeFullName shouldBe "kotlin.jvm.internal.CallableReference"
    }

    "create a constructor call for the synthetic type with receiver as parameter" in {
      val ctorCalls = cpg.method("test").call.methodFullName(".*Function2Impl.*<init>.*").l
      ctorCalls.size shouldBe 1

      val ctorCall = ctorCalls.head
      ctorCall.methodFullName shouldBe "com.test.Handler.process$kotlin.jvm.functions.Function2Impl.<init>:void(com.test.Handler)"
      ctorCall.typeFullName shouldBe "void"
      ctorCall.signature shouldBe "void(com.test.Handler)"

      val args = ctorCall.argument.sortBy(_.argumentIndex).l

      val receiverArg = args(1).asInstanceOf[Identifier]
      receiverArg.name shouldBe "handler"
      receiverArg.typeFullName shouldBe "com.test.Handler"
    }

    "have PUBLIC and VIRTUAL modifiers on invoke method" in {
      val modifiers = invokeMethod.modifier.l
      modifiers.size shouldBe 2
      modifiers.map(_.modifierType).toSet shouldBe Set("PUBLIC", "VIRTUAL")
    }

    "have a constructor method with CONSTRUCTOR modifier" in {
      val constructors = syntheticTypeDecl.method.name("<init>").l
      constructors.size shouldBe 1

      val constructor = constructors.head
      constructor.fullName shouldBe "com.test.Handler.process$kotlin.jvm.functions.Function2Impl.<init>:void(com.test.Handler)"
      constructor.signature shouldBe "void(com.test.Handler)"

      val ctorParams = constructor.parameter.l.sortBy(_.index)
      ctorParams.size shouldBe 2
      ctorParams.head.name shouldBe "this"
      ctorParams.head.typeFullName shouldBe "com.test.Handler.process$kotlin.jvm.functions.Function2Impl"
      ctorParams(1).name shouldBe "handler"
      ctorParams(1).typeFullName shouldBe "com.test.Handler"

      val ctorModifiers = constructor.modifier.l
      ctorModifiers.size shouldBe 1
      ctorModifiers.head.modifierType shouldBe "CONSTRUCTOR"
    }

    "have constructor body that calls CallableReference constructor" in {
      val constructor         = syntheticTypeDecl.method.name("<init>").head
      val callableRefCtorCall = constructor.ast.isCall.name("<init>").methodFullName(".*CallableReference.*").head

      callableRefCtorCall.methodFullName shouldBe "kotlin.jvm.internal.CallableReference.<init>:void(com.test.Handler)"
      callableRefCtorCall.signature shouldBe "void(com.test.Handler)"
      callableRefCtorCall.dispatchType shouldBe "STATIC_DISPATCH"

      val args = callableRefCtorCall.argument.l
      args.size shouldBe 1

      val receiverArg = args.head.asInstanceOf[Identifier]
      receiverArg.name shouldBe "handler"
      receiverArg.typeFullName shouldBe "com.test.Handler"
      receiverArg.argumentIndex shouldBe 1

      inside(receiverArg.refsTo.l) { case List(param: MethodParameterIn) =>
        param.name shouldBe "handler"
        param.typeFullName shouldBe "com.test.Handler"
        param.index shouldBe 1
      }
    }

    "have dynamic type hints on this parameter" in {
      val thisParam = invokeMethod.parameter.index(0).head
      thisParam.dynamicTypeHintFullName shouldBe Seq("com.test.Handler.process$kotlin.jvm.functions.Function2Impl")
    }

    "have block code containing the actual method call" in {
      val block = invokeMethod.ast.isBlock.head
      block.code shouldBe "return receiver.process(p1, p2)"
    }

    "have a nested TypeDecl representing the function type" in {
      val nestedTypeDecls = cpg.typeDecl.fullName(".*Function2Impl.*invoke.*").l
      nestedTypeDecls.size shouldBe 1

      val nestedTypeDecl = nestedTypeDecls.head
      nestedTypeDecl.name shouldBe "invoke"
      nestedTypeDecl.fullName shouldBe "com.test.Handler.process$kotlin.jvm.functions.Function2Impl.invoke:boolean(int,java.lang.String)"
      nestedTypeDecl.astParentType shouldBe "TYPE_DECL"
      nestedTypeDecl.astParentFullName shouldBe "com.test.Handler.process$kotlin.jvm.functions.Function2Impl"
      nestedTypeDecl.inheritsFromTypeFullName should contain("kotlin.Function")
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
      ctorCall.methodFullName shouldBe "com.test.Utils$Companion.validate$kotlin.jvm.functions.Function1Impl.<init>:void(com.test.Utils$Companion)"
      ctorCall.typeFullName shouldBe "void"

      val args = ctorCall.argument.l
      args.size shouldBe 2

      val receiverArg = args(1).asInstanceOf[Call]
      receiverArg.name shouldBe Operators.fieldAccess
      receiverArg.typeFullName shouldBe "com.test.Utils$Companion"

      val List(receiverBase: Identifier, receiverField: FieldIdentifier) = receiverArg.argument.l: @unchecked
      receiverBase.name shouldBe "Utils"
      receiverBase.typeFullName shouldBe "com.test.Utils"
      receiverField.canonicalName shouldBe Constants.CompanionObjectMemberName
    }

    "have PUBLIC and VIRTUAL modifiers on invoke method" in {
      val modifiers = invokeMethod.modifier.l
      modifiers.size shouldBe 2
      modifiers.map(_.modifierType).toSet shouldBe Set("PUBLIC", "VIRTUAL")
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
      methodRef.typeFullName shouldBe ("com.test.globalFunction$kotlin.jvm.functions.Function2Impl")
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
        .sortBy(_.methodFullName)
      bindings.size shouldBe 2

      val List(erasedBinding, initialBinding) = bindings
      initialBinding.methodFullName shouldBe "com.test.globalFunction:java.lang.String(int,int)"
      initialBinding.signature shouldBe "java.lang.String(int,int)"

      erasedBinding.methodFullName shouldBe "com.test.globalFunction:java.lang.String(int,int)"
      erasedBinding.signature shouldBe "java.lang.Object(java.lang.Object,java.lang.Object)"
    }

    "not create an invoke method that calls the function, only the binding" in {
      val syntheticTypes = cpg.typeDecl.fullName(".*globalFunction.*Function2.*").l
      val syntheticType  = syntheticTypes.head
      val invokeMethods  = syntheticType.method.name("invoke").l
      // The invoke method should not be created in this case
      invokeMethods.isEmpty shouldBe true
    }

    "not create a constructor for unbound references" in {
      val syntheticTypes = cpg.typeDecl.fullName(".*globalFunction.*Function2.*").l
      val syntheticType  = syntheticTypes.head
      val constructors   = syntheticType.method.name("<init>").l
      constructors.isEmpty shouldBe true
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

    "have modifiers on methods implementing custom interfaces" in {
      val transformerImpl = cpg.typeDecl.fullName(".*TransformerImpl.*").head
      val transformMethod = transformerImpl.method.name("transform").head
      val modifiers       = transformMethod.modifier.l
      modifiers.map(_.modifierType).toSet shouldBe Set("PUBLIC", "VIRTUAL")
    }

    "have constructors for custom interface implementations" in {
      val transformerImpl = cpg.typeDecl.fullName(".*TransformerImpl.*").head
      val constructors    = transformerImpl.method.name("<init>").l
      constructors.size shouldBe 1

      val constructor = constructors.head
      constructor.modifier.map(_.modifierType).head shouldBe "CONSTRUCTOR"
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

      // Check invoke method
      invokeMethod.fullName shouldBe "Counter.increment$kotlin.jvm.functions.Function0Impl.invoke:int()"
      invokeMethod.signature shouldBe "int()"
      val invokeParams = invokeMethod.parameter.l.sortBy(_.index)
      invokeParams.size shouldBe 1
      invokeParams.head.name shouldBe "this"
      invokeParams.head.typeFullName shouldBe "Counter.increment$kotlin.jvm.functions.Function0Impl"

      // Check constructor
      val constructor = syntheticTypeDecl.method.name("<init>").head
      constructor.fullName shouldBe "Counter.increment$kotlin.jvm.functions.Function0Impl.<init>:void(Counter)"
      constructor.signature shouldBe "void(Counter)"
      val ctorParams = constructor.parameter.l.sortBy(_.index)
      ctorParams.size shouldBe 2
      ctorParams.head.name shouldBe "this"
      ctorParams.head.typeFullName shouldBe "Counter.increment$kotlin.jvm.functions.Function0Impl"
      ctorParams(1).name shouldBe "counter"
      ctorParams(1).typeFullName shouldBe "Counter"

      // Check constructor call
      val ctorCall = cpg.method("test").call.methodFullName(".*Function0Impl.*<init>.*").head
      ctorCall.signature shouldBe "void(Counter)"
      ctorCall.argument.size shouldBe 2
      val receiverArg = ctorCall.argument.argumentIndex(1).head.asInstanceOf[Identifier]
      receiverArg.name shouldBe "counter"
      receiverArg.typeFullName shouldBe "Counter"

      // Check increment call
      val incrementCall = invokeMethod.ast.isCall.name("increment").head
      incrementCall.signature shouldBe "int()"
      incrementCall.methodFullName shouldBe "Counter.increment:int()"
      incrementCall.argument.isIdentifier.size shouldBe 0

      // Check dynamic type hints
      val invokeThisParam = invokeMethod.parameter.index(0).head
      invokeThisParam.dynamicTypeHintFullName should not be empty
      invokeThisParam.dynamicTypeHintFullName.head should include("Function0Impl")

      val ctorThisParam = constructor.parameter.index(0).head
      ctorThisParam.dynamicTypeHintFullName should not be empty
      ctorThisParam.dynamicTypeHintFullName.head should include("Function0Impl")
    }

    "handle nullable parameter types" in {
      val cpg = code("""
          |class Handler {
          |    fun process(value: String?): String? {
          |        return value
          |    }
          |}
          |
          |fun test() {
          |   val handler = Handler()
          |   val ref: (String?) -> String? = handler::process
          |}
          |""".stripMargin)

      val syntheticTypeDecl = cpg.typeDecl.fullName(".*Function1Impl.*").head
      val invokeMethod      = syntheticTypeDecl.method.name("invoke").head

      // Check invoke method
      invokeMethod.fullName shouldBe "Handler.process$kotlin.jvm.functions.Function1Impl.invoke:java.lang.String(java.lang.String)"
      invokeMethod.signature shouldBe "java.lang.String(java.lang.String)"
      val invokeParams = invokeMethod.parameter.l.sortBy(_.index)
      invokeParams.size shouldBe 2
      invokeParams.head.name shouldBe "this"
      invokeParams(1).name shouldBe "p1"
      invokeParams(1).typeFullName should include("String")

      // Check constructor
      val constructor = syntheticTypeDecl.method.name("<init>").head
      constructor.fullName shouldBe "Handler.process$kotlin.jvm.functions.Function1Impl.<init>:void(Handler)"
      constructor.signature shouldBe "void(Handler)"
      val ctorParams = constructor.parameter.l.sortBy(_.index)
      ctorParams.size shouldBe 2
      ctorParams(1).name shouldBe "handler"
      ctorParams(1).typeFullName shouldBe "Handler"

      // Check constructor call
      val ctorCall = cpg.method("test").call.methodFullName(".*Function1Impl.*<init>.*").head
      ctorCall.signature shouldBe "void(Handler)"
      val receiverArg = ctorCall.argument.argumentIndex(1).head.asInstanceOf[Identifier]
      receiverArg.name shouldBe "handler"
      receiverArg.typeFullName shouldBe "Handler"

      // Check process call
      val processCall = invokeMethod.ast.isCall.name("process").head
      processCall.signature should include("String")
      processCall.methodFullName shouldBe "Handler.process:java.lang.String(java.lang.String)"
      processCall.argument.isIdentifier.size shouldBe 1

      // Check dynamic type hints
      val invokeThisParam = invokeMethod.parameter.index(0).head
      invokeThisParam.dynamicTypeHintFullName should not be empty
      invokeThisParam.dynamicTypeHintFullName.head should include("Function1Impl")

      val ctorThisParam = constructor.parameter.index(0).head
      ctorThisParam.dynamicTypeHintFullName should not be empty
      ctorThisParam.dynamicTypeHintFullName.head should include("Function1Impl")
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

      // Check invoke method
      invokeMethod.fullName shouldBe "com.test.MyClass.method$kotlin.jvm.functions.Function1Impl.invoke:void(int)"
      invokeMethod.signature shouldBe "void(int)"
      val invokeParams = invokeMethod.parameter.l.sortBy(_.index)
      invokeParams.size shouldBe 2
      invokeParams.head.name shouldBe "this"
      invokeParams(1).name shouldBe "p1"
      invokeParams(1).typeFullName shouldBe "int"

      // Check constructor
      val constructor = syntheticTypeDecl.method.name("<init>").head
      constructor.fullName shouldBe "com.test.MyClass.method$kotlin.jvm.functions.Function1Impl.<init>:void(com.test.MyClass)"
      constructor.signature shouldBe "void(com.test.MyClass)"
      val ctorParams = constructor.parameter.l.sortBy(_.index)
      ctorParams.size shouldBe 2
      ctorParams(1).name shouldBe "this"
      ctorParams(1).typeFullName shouldBe "com.test.MyClass"

      // Check constructor call
      val ctorCall = cpg.method("setup").call.methodFullName(".*Function1Impl.*<init>.*").head
      ctorCall.signature shouldBe "void(com.test.MyClass)"
      val receiverArg = ctorCall.argument.argumentIndex(1).head.asInstanceOf[Identifier]
      receiverArg.name shouldBe "this"
      receiverArg.typeFullName shouldBe "com.test.MyClass"

      // Check method call
      val methodCall = invokeMethod.ast.isCall.name("method").head
      methodCall.dispatchType shouldBe "DYNAMIC_DISPATCH"
      methodCall.signature shouldBe "void(int)"
      methodCall.methodFullName shouldBe "com.test.MyClass.method:void(int)"
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

      // Check map method (SAM interface method)
      val mapMethod = syntheticTypeDecl.method.name("map").head
      mapMethod.fullName shouldBe "com.test.Converter.convertStrings$com.test.MapperImpl.map:java.util.List(java.util.List)"
      mapMethod.signature shouldBe "java.util.List(java.util.List)"
      val mapParams = mapMethod.parameter.l.sortBy(_.index)
      mapParams.size shouldBe 2
      mapParams.head.name shouldBe "this"
      mapParams(1).name shouldBe "items"
      mapParams(1).typeFullName should include("List")

      // Check constructor
      val constructor = syntheticTypeDecl.method.name("<init>").head
      constructor.fullName shouldBe "com.test.Converter.convertStrings$com.test.MapperImpl.<init>:void(com.test.Converter)"
      constructor.signature shouldBe "void(com.test.Converter)"
      val ctorParams = constructor.parameter.l.sortBy(_.index)
      ctorParams.size shouldBe 2
      ctorParams(1).name shouldBe "converter"
      ctorParams(1).typeFullName shouldBe "com.test.Converter"

      // Check constructor call
      val ctorCall = cpg.method("test").call.methodFullName(".*MapperImpl.*<init>.*").head
      ctorCall.signature shouldBe "void(com.test.Converter)"
      val receiverArg = ctorCall.argument.argumentIndex(1).head.asInstanceOf[Identifier]
      receiverArg.name shouldBe "converter"
      receiverArg.typeFullName shouldBe "com.test.Converter"

      // Check convertStrings call
      val convertCall = mapMethod.ast.isCall.name("convertStrings").head
      convertCall.signature shouldBe "java.util.List(java.util.List)"
      convertCall.methodFullName shouldBe "com.test.Converter.convertStrings:java.util.List(java.util.List)"
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

    val syntheticTypeDecls = cpg.typeDecl.fullName(".*Calculator.*Function2Impl.*").filter(_.astParentType == "").l

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
          "com.test.Calculator.add$kotlin.jvm.functions.Function2Impl.<init>:void(com.test.Calculator)"
        )
        .l
      ctorCalls.size shouldBe 3
    }
  }

  "enforce uniqueness across multiple files" should {
    val cpg = code(
      """
        |package com.test
        |
        |class Calculator {
        |    fun add(a: Int, b: Int): Int = a + b
        |}
        |""".stripMargin,
      "Calculator.kt"
    ).moreCode(
      """
        |package com.test
        |
        |fun firstFunction() {
        |   val calc = Calculator()
        |   val ref: (Int, Int) -> Int = calc::add
        |}
        |""".stripMargin,
      "FirstFile.kt"
    ).moreCode(
      """
        |package com.test
        |
        |fun secondFunction() {
        |   val calc = Calculator()
        |   val ref: (Int, Int) -> Int = calc::add
        |}
        |""".stripMargin,
      "SecondFile.kt"
    ).moreCode(
      """
        |package com.test
        |
        |class ThirdFile {
        |    fun thirdFunction() {
        |       val calc = Calculator()
        |       val ref: (Int, Int) -> Int = calc::add
        |    }
        |}
        |""".stripMargin,
      "ThirdFile.kt"
    )

    val syntheticTypeDecls = cpg.typeDecl.fullName(".*Calculator.*Function2Impl.*").filter(_.astParentType == "").l

    "only have ONE synthetic type declaration across all files" in {
      syntheticTypeDecls.size shouldBe 1
    }

    val syntheticTypeDecl = syntheticTypeDecls.head

    "should have the correct invoke method" in {
      val invokeMethod = syntheticTypeDecl.method.name("invoke").head
      invokeMethod.fullName shouldBe "com.test.Calculator.add$kotlin.jvm.functions.Function2Impl.invoke:int(int,int)"
      invokeMethod.signature shouldBe "int(int,int)"
    }

    "should have the correct constructor" in {
      val constructor = syntheticTypeDecl.method.name("<init>").head
      constructor.fullName shouldBe "com.test.Calculator.add$kotlin.jvm.functions.Function2Impl.<init>:void(com.test.Calculator)"
      constructor.signature shouldBe "void(com.test.Calculator)"
    }

    "the single synthetic type should be used by constructor calls in all three files" in {
      val ctorCalls = cpg.call
        .nameExact("<init>")
        .methodFullNameExact(
          "com.test.Calculator.add$kotlin.jvm.functions.Function2Impl.<init>:void(com.test.Calculator)"
        )
        .l
      ctorCalls.size shouldBe 3

      // Verify they're in different methods/files
      val callingMethods = ctorCalls.method.name.toSet
      callingMethods shouldBe Set("firstFunction", "secondFunction", "thirdFunction")
    }
  }
}
