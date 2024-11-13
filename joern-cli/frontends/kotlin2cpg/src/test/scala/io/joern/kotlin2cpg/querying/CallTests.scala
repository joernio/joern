package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.Constants
import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Identifier, Literal}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.semanticcpg.language.*

class CallTests extends KotlinCode2CpgFixture(withOssDataflow = false) {

  implicit val resolver: ICallResolver = NoResolve

  "CPG for code with two functions with the same name, but different params" should {
    val cpg = code("""
        |package mypkg
        |
        |fun foo(x: Int, y: Int): Int {
        |  return x + y
        |}
        |
        |fun main(args : Array<String>) {
        |  val argc: Int = args.size
        |  println(foo(argc, 1))
        |}
        |
        |fun foo(x: Int): Int {
        |  return x * y
        |}
        |""".stripMargin)

    "should contain a call node for `argc`'s declaration with correct props set" in {
      cpg.call(Operators.assignment).size shouldBe 1

      val List(c) = cpg.call(Operators.assignment).l
      c.argument.size shouldBe 2
      c.code shouldBe "val argc: Int = args.size"
      c.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      c.lineNumber shouldBe Some(9)
      c.columnNumber shouldBe Some(6)
    }

    "should contain a call node for `x + y` with correct props set" in {
      cpg.call(Operators.addition).size shouldBe 1

      val List(c) = cpg.call(Operators.addition).l
      c.argument.size shouldBe 2
      c.code shouldBe "x + y"
      c.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      c.lineNumber shouldBe Some(5)
      c.columnNumber shouldBe Some(9)
    }

    "should contain a call node for `println` with correct props set" in {
      cpg.call("println").size shouldBe 1

      val List(p) = cpg.call("println").l
      p.argument.size shouldBe 1
      p.lineNumber shouldBe Some(10)
      p.code shouldBe "println(foo(argc, 1))"
      p.methodFullName shouldBe "kotlin.io.println:void(int)"
      p.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      p.columnNumber shouldBe Some(2)
    }

    "should contain a call node for `foo` with correct props set" in {
      cpg.call("foo").size shouldBe 1

      val List(p) = cpg.call("foo").l
      p.argument.size shouldBe 2
      p.lineNumber shouldBe Some(10)
      p.code shouldBe "foo(argc, 1)"
      p.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      p.columnNumber shouldBe Some(10)
    }

    "should contain the correct number of CALL nodes" in {
      cpg.call.size shouldBe 6
    }

    "should allow traversing from call to surrounding method" in {
      val List(x) = cpg.call("foo").method.l
      x.name shouldBe "main"
    }

    "should allow traversing from call to callee method" in {
      val List(x) = cpg.call.code("foo.*").callee.l
      x.name shouldBe "foo"
    }

    "should allow traversing from argument to parameter" in {
      val List(x) = cpg.call.code("foo.*").argument(1).parameter.l
      x.name shouldBe "x"
    }
  }

  "CPG for code with a class declaration " should {
    val cpg = code("""
        |package mypkg
        |
        |class Foo {
        |    fun add1(x: Int, toPrint: java.lang.String): Int {
        |        println(toPrint)
        |        return x + 1
        |    }
        |}
        |
        |fun main(argc: Int): Int {
        |  val x = Foo()
        |  val y = x.add1(argc, "AMESSAGE")
        |  return y
        |}
        |""".stripMargin)

    "should contain a CALL node for `Foo()` with the correct properties set" in {
      val List(c) = cpg.call.methodFullName(".*init.*").l
      c.methodFullName shouldBe "mypkg.Foo.<init>:void()"
      c.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      c.signature shouldBe "void()"
      c.code shouldBe "Foo()"
      c.columnNumber shouldBe Some(10)
      c.lineNumber shouldBe Some(12)
    }

    "should contain a CALL node for `add1` with the correct props set" in {
      val List(c) = cpg.call("add1").l
      c.argument.size shouldBe 3
      c.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
      c.code shouldBe "x.add1(argc, \"AMESSAGE\")"
      c.columnNumber shouldBe Some(10)
      c.lineNumber shouldBe Some(13)
      c.methodFullName shouldBe "mypkg.Foo.add1:int(int,java.lang.String)"
      c.signature shouldBe "int(int,java.lang.String)"
      c.typeFullName shouldBe "int"

      val List(firstArg, secondArg, thirdArg) = cpg.call("add1").argument.l
      firstArg.code shouldBe "x"
      firstArg.argumentIndex shouldBe 0

      secondArg.code shouldBe "argc"
      secondArg.argumentIndex shouldBe 1

      thirdArg.argumentIndex shouldBe 2
    }

    "should contain a call node for `add1` with a receiver set" in {
      cpg.call("add1").receiver.size shouldBe 1

      val List(r) = cpg.call("add1").receiver.l
      r.code shouldBe "x"
    }
  }

  "CPG for code with a call to an implicitly imported stdlib fn " should {
    val cpg = code("""
        |package mypkg
        |
        |fun doSome(x: String) {
        |  println("PLACEHOLDER")
        |}
        |
        |fun main(args : Array<String>) {
        |  doSome("SOME")
        |}
        |""".stripMargin)

    "should contain a call node for `println` with a fully-qualified stdlib METHOD_FULL_NAME" in {
      val List(c) = cpg.call(".*println.*").l
      c.methodFullName shouldBe "kotlin.io.println:void(java.lang.Object)"
      c.signature shouldBe "void(java.lang.Object)"
    }

    "should contain a call node for `doSome` with the correct METHOD_FULL_NAME set" in {
      val List(c) = cpg.call(".*doSome.*").l
      c.methodFullName shouldBe "mypkg.doSome:void(java.lang.String)"
      c.signature shouldBe "void(java.lang.String)"
    }
  }

  "CPG for code with invocation of extension function from stdlib" should {
    val cpg = code("""
        |package mypkg
        |
        |fun main() {
        |  println(1.toString())
        |}
        |""".stripMargin)

    "should contain a CALL node for the `toString` invocation with the correct props set" in {
      val List(c) = cpg.call.code("1.*toString.*").l
      c.methodFullName shouldBe "kotlin.Int.toString:java.lang.String()"
      c.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
      c.signature shouldBe "java.lang.String()"
      c.typeFullName shouldBe "java.lang.String"
    }
  }

  "CPG for code with a simple method call to decl of Java's stdlib" should {
    val cpg = code("""
      |package mypkg
      |
      |fun foo(x: String): Int {
      |   val r = Runtime.getRuntime()
      |   r.exec(x)
      |   return 0
      |}
      |
      |""".stripMargin)

    "should contain a CALL node with arguments with the correct props set" in {
      val List(firstArg: Identifier, secondArg: Identifier) = cpg.call.code("r.exec.*").argument.l: @unchecked
      firstArg.typeFullName shouldBe "java.lang.Runtime"
      secondArg.typeFullName shouldBe "java.lang.String"
    }
  }

  "CPG for code with a simple call to class from Java's stdlib imported with _as_" should {
    val cpg = code("""
        |package mypkg
        |
        |import java.io.File as MyFile
        |
        |fun main() {
        |   val fullPath = "/tmp/kotlin2cpg.example.txt"
        |   val f = MyFile(fullPath)
        |   val msg = "AMESSAGE"
        |   f.writeText(msg)
        |}
        |""".stripMargin)

    "should contain a CALL node `writeText` with the correct props set" in {
      val List(c) = cpg.call.code("f.writeText.*").l
      c.methodFullName shouldBe "kotlin.io.writeText:void(java.io.File,java.lang.String,java.nio.charset.Charset)"
    }
  }

  "CPG for code with a simple call with unknown identifier imported via _as_" should {
    val cpg = code("""
        |package mypkg
        |
        |import no.such.CaseClass as MyCaseClass
        |
        |fun main() {
        |  val res = MyCaseClass.PROP
        |  println(res)
        |
        |  val otherRes = MyCaseClass("AN_ARGUMENT")
        |  println(otherRes.aFn())
        |}
        |""".stripMargin)

    "should contain a CALL node for `MyCaseClass.PROP` with the correct props set" in {
      val List(c) = cpg.call.code("MyCaseClass.PROP").l
      c.methodFullName shouldBe Operators.fieldAccess
      c.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      c.lineNumber shouldBe Some(7)
      c.columnNumber shouldBe Some(12)
      c.signature shouldBe ""
    }

    "should contain a CALL node for `MyCaseClass(\\\"AN_ARGUMENT\\\")` with the correct props set" in {
      val List(c) = cpg.call.code("MyCaseClass.*AN_ARGUMENT.*").l
      c.methodFullName shouldBe s"no.such.CaseClass:${Defines.UnresolvedSignature}(1)"
      c.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      c.lineNumber shouldBe Some(10)
      c.columnNumber shouldBe Some(17)
      c.signature shouldBe s"${Defines.UnresolvedSignature}(1)"
    }
  }

  "CPG for code with call which has parenthesized expression as receiver" should {
    val cpg = code("""
      |package mypkg
      |
      |fun main() {
      |  val x = 100
      |  val y = 200
      |  val z = (if (x / 20 < 3) 3 else y / 20).toFloat()
      |  println(z)
      |}
      |""".stripMargin)

    "should contain a CALL node for `.*toFloat()` with the correct props set" in {
      val List(c) = cpg.call.code("\\(.*toFloat.*").l
      c.methodFullName shouldBe "kotlin.Int.toFloat:float()"
    }
  }

  "CPG for code with call to ctor using named arguments" should {
    val cpg = code("""
      |package no.such.pkg
      |class Person(val firstName: String, val lastName: String)
      |fun doSomething(x: String, y: String) {
      |    val p = Person(lastName = y, firstName = x)
      |    println(p.firstName)
      |    println(p.lastName)
      |}
      |""".stripMargin)

    "should contain a CALL node with arguments that have the argument name set" in {
      val List(c) = cpg.call.code("Person.*").l
      c.argument(1).argumentName shouldBe Some("lastName")
      c.argument(2).argumentName shouldBe Some("firstName")
    }
  }

  "CPG for code with call with named arguments of user-defined fn" should {
    val cpg = code("""
       |package no.such.pkg
       |fun printNextLineRm(file: String, line: String) {
       |    println("file: $file")
       |    println("line: $line")
       |}
       |fun doSomething() = printNextLineRm(line = "MD_Update(&m,buf,j);", file = "rand_lcl.h")
       |""".stripMargin)

    "should contain a CALL node with arguments that have the argument name set" in {
      val List(c) = cpg.call.code("printNextLineRm.*").l
      c.argument(1).argumentName shouldBe Some("line")
      c.argument(2).argumentName shouldBe Some("file")
    }
  }

  "CPG for code with named arguments in call on object" should {
    val cpg = code("""
       |package no.such.pkg
       |fun outer() {
       |    Pair(1,2).copy(second = 3)
       |}
       |""".stripMargin)

    "contain a CALL node with arguments that have the argument name set" in {
      val List(c) = cpg.call.name("copy").l
      c.argument(1).argumentName shouldBe Some("second")
    }
  }

  "CPG for code with implicit this access on apply and run call" should {
    val cpg = code("""
        |package no.such.pkg
        |
        |fun outer() {
        |    Pair(1,2).apply { println(second) }
        |}
        |""".stripMargin)

    "contain a CALL node with argument that is a this access" in {
      val List(printCall) = cpg.call.name("println").l
      val secondCall      = printCall.argument(1).asInstanceOf[Call]
      secondCall.methodFullName shouldBe Operators.fieldAccess
      secondCall.code shouldBe "this.second"
      secondCall.argument(1).asInstanceOf[Identifier].typeFullName shouldBe "kotlin.Pair"
    }
  }

  "CPG for code with call with argument with type with upper bound" should {
    val cpg = code("""
      |package mypkg
      |open class Base
      |class Second : Base()
      |class Third : Base()
      |
      |fun <S:Base>doSomething(one: S) {
      |    println(one)
      |}
      |fun f1() {
      |    val s = Second()
      |    val t = Third()
      |    doSomething(s)
      |    doSomething(t)
      |}
      |""".stripMargin)

    "should contain a CALL node with arguments that have the argument name set" in {
      val List(c1, c2) = cpg.call.code("doSomething.*").l
      c1.methodFullName shouldBe "mypkg.doSomething:void(mypkg.Base)"
      c2.methodFullName shouldBe "mypkg.doSomething:void(mypkg.Base)"
    }
  }

  "CPG for code with qualified-expression call with argument with type with upper bound" should {
    val cpg = code("""
        |package mypkg
        |fun f1() {
        |    val ns = sequenceOf("four", "three", "two", "one")
        |    val ml = mutableListOf<String>()
        |    ns.mapIndexedNotNullTo(ml, { i, s -> s })
        |}
        |""".stripMargin)

    "should contain a METHOD node with correct METHOD_FULL_NAME set" in {
      val List(c) = cpg.method.nameExact("mapIndexedNotNullTo").callIn.l
      c.methodFullName shouldBe "kotlin.sequences.mapIndexedNotNullTo:java.util.Collection(kotlin.sequences.Sequence,java.util.Collection,kotlin.jvm.functions.Function2)"
    }
  }

  "CPG for code with simple call having literals passed in with argument names" should {
    val cpg = code("""
      |package mypkg
      |fun f1(one: String, two: String)  = println(one + " " + two)
      |fun f2() = f1(two = "this",  one = "that")
      |""".stripMargin)

    "should contain a CALL node with arguments with their ARGUMENT_NAME property set" in {
      val List(c: Call) = cpg.method.nameExact("f1").callIn.l
      c.argument.map(_.argumentName).flatten.l shouldBe List("two", "one")
    }
  }

  "CPG for code with simple call having another call passed in with an argument name" should {
    val cpg = code("""
      |package mypkg
      |fun f1(one: String, two: String)  = println(one + " " + two)
      |fun f2() = f1(two = "this",  one = f3())
      |fun f3() = "that"
      |""".stripMargin)

    "should contain a CALL node with arguments with their ARGUMENT_NAME property set" in {
      val List(c: Call) = cpg.method.nameExact("f1").callIn.l
      c.argument.map(_.argumentName).flatten.l shouldBe List("two", "one")
    }
  }

  "CPG for code with simple call having a ctor call passed in with an argument name" should {
    val cpg = code("""
      |package mypkg
      |data class X(val p: String)
      |fun f1(one: X, two: String)  = println(one.p + " " + two)
      |fun f2() = f1(two = "this",  one = X("that"))
      |""".stripMargin)

    "should contain a CALL node with arguments with their ARGUMENT_NAME property set" in {
      val List(c: Call) = cpg.method.nameExact("f1").callIn.l
      c.argument.map(_.argumentName).flatten.l shouldBe List("two", "one")
    }
  }

  "CPG for code with simple call having an if-expression passed in with an argument name" should {
    val cpg = code("""
      |package mypkg
      |fun f1(one: String, two: String)  = println(one + " " + two)
      |fun f2() = f1(two = "this",  one = if(Random(1).nextBoolean()) "that" else "thatother")
      |""".stripMargin)

    "should contain a CALL node with arguments with their ARGUMENT_NAME property set" in {
      val List(c: Call) = cpg.method.nameExact("f1").callIn.l
      c.argument.map(_.argumentName).flatten.l shouldBe List("two", "one")
    }
  }

  "CPG for code with simple call having a try-expression passed in with an argument name" should {
    val cpg = code("""
      |package mypkg
      |fun f1(one: String, two: String)  = println(one + " " + two)
      |fun f2() = f1(two = "this",  one = try { "that" } catch (e: Exception) { "thatother" })
      |""".stripMargin)

    "should contain a CALL node with arguments with their ARGUMENT_NAME property set" in {
      val List(c: Call) = cpg.method.nameExact("f1").callIn.l
      c.argument.map(_.argumentName).flatten.l shouldBe List("two", "one")
    }
  }

  "CPG for code with simple call having an when-expression passed in with an argument name" should {
    val cpg = code("""
      |package mypkg
      |fun f1(one: String, two: String)  = println(one + " " + two)
      |fun f2() = f1(two = "this",  one = when(Random(1).nextBoolean()) { true -> "that" else -> "thatother" })
      |""".stripMargin)

    "should contain a CALL node with arguments with their ARGUMENT_NAME property set" in {
      val List(c: Call) = cpg.method.nameExact("f1").callIn.l
      c.argument.map(_.argumentName).flatten.l shouldBe List("two", "one")
    }
  }

  "CPG for code with simple call having a simple qualified-expression passed in with an argument name" should {
    val cpg = code("""
      |package mypkg
      |data class X(val p: String)
      |fun f1(one: String, two: String)  = println(one + " " + two)
      |fun f2() = {
      |  val x = X("that")
      |  f1(two = "this",  one = x.p)
      |}
      |""".stripMargin)

    "should contain a CALL node with arguments with their ARGUMENT_NAME property set" in {
      val List(c: Call) = cpg.method.nameExact("f1").callIn.l
      c.argument.map(_.argumentName).flatten.l shouldBe List("two", "one")
    }
  }

  "CPG for code with simple call having a qualified-expression with ctor passed in with an argument name" should {
    val cpg = code("""
      |package mypkg
      |data class X(val p: String)
      |fun f1(one: String, two: String)  = println(one + " " + two)
      |fun f2() = f1(two = "this",  one = X("that").p)
      |""".stripMargin)

    "should contain a CALL node with arguments with their ARGUMENT_NAME property set" in {
      val List(c: Call) = cpg.method.nameExact("f1").callIn.l
      c.argument.map(_.argumentName).flatten.l shouldBe List("two", "one")
    }
  }

  "CPG for code with simple call having a labeled expression passed in with an argument name" should {
    val cpg = code("""
      |package mypkg
      |class X(val p: String) {
      |  fun f2() = f1(two = "this",  one = alabel@ "that")
      |}
      |fun f1(one: String, two: String)  = println(one + " " + two)
      |""".stripMargin)

    "should contain a CALL node with arguments with their ARGUMENT_NAME property set" in {
      val List(c: Call) = cpg.method.nameExact("f1").callIn.l
      c.argument.map(_.argumentName).flatten.l shouldBe List("two", "one")
    }
  }

  "CPG for code with simple call having a qualified-expression for `super` passed in with an argument name" should {
    val cpg = code("""
      |package mypkg
      |class X(val p: String) {
      |  fun f2() = f1(two = "this",  one = super.toString())
      |}
      |fun f1(one: String, two: String)  = println(one + " " + two)
      |""".stripMargin)

    "should contain a CALL node with arguments with their ARGUMENT_NAME property set" in {
      val List(c: Call) = cpg.method.nameExact("f1").callIn.l
      c.argument.map(_.argumentName).flatten.l shouldBe List("two", "one")
    }
  }

  "CPG for code with simple call having a parenthesized expression passed in with an argument name" should {
    val cpg = code("""
      |package mypkg
      |fun f1(one: String, two: String)  = println(one + " " + two)
      |fun f2() = f1(two = "this",  one = ("that"))
      |""".stripMargin)

    "should contain a CALL node with arguments with their ARGUMENT_NAME property set" in {
      val List(c: Call) = cpg.method.nameExact("f1").callIn.l
      c.argument.map(_.argumentName).flatten.l shouldBe List("two", "one")
    }
  }

  "CPG for code with simple call having an annotated expression passed in with an argument name" should {
    val cpg = code("""
      |package mypkg
      |@Target(AnnotationTarget.EXPRESSION)
      |@Retention(AnnotationRetention.SOURCE)
      |annotation class Annotation
      |fun f1(one: String, two: String)  = println(one + " " + two)
      |fun f2() = f1(two = "this",  one = @Annotation "that")
      |""".stripMargin)

    "should contain a CALL node with arguments with their ARGUMENT_NAME property set" in {
      val List(c: Call) = cpg.method.nameExact("f1").callIn.l
      c.argument.map(_.argumentName).flatten.l shouldBe List("two", "one")
    }
  }

  "CPG for code with simple call having a binary expression passed in with an argument name" should {
    val cpg = code("""
      |package mypkg
      |fun f1(one: Int, two: String)  = println(one.toString() + " " + two)
      |fun f2() = f1(two = "this",  one = 1 * 2)
      |""".stripMargin)

    "should contain a CALL node with arguments with their ARGUMENT_NAME property set" in {
      val List(c: Call) = cpg.method.nameExact("f1").callIn.l
      c.argument.map(_.argumentName).flatten.l shouldBe List("two", "one")
    }
  }

  "CPG for code with simple call having a binary expression with type RHS passed in with an argument name" should {
    val cpg = code("""
      |package mypkg
      |fun f1(one: String, two: String)  = println(one + " " + two)
      |fun f2() = f1(two = "this",  one = "that" as String)
      |""".stripMargin)

    "should contain a CALL node with arguments with their ARGUMENT_NAME property set" in {
      val List(c: Call) = cpg.method.nameExact("f1").callIn.l
      c.argument.map(_.argumentName).flatten.l shouldBe List("two", "one")
    }
  }

  "CPG for code with simple call having an object expression passed in with an argument name" should {
    val cpg = code("""
      |package mypkg
      |interface I { fun that(): String }
      |fun f1(one: I, two: String)  = println(one.that() + " " + two)
      |fun f2() = f1(two = "this",  one = object : I { override fun that() = "that" })
      |""".stripMargin)

    "should contain a CALL node with arguments with their ARGUMENT_NAME property set" in {
      val List(c: Call) = cpg.method.nameExact("f1").callIn.l
      c.argument.map(_.argumentName).flatten.l shouldBe List("two", "one")
    }
  }

  "CPG for code with simple call having a lambda passed in with an argument name" should {
    val cpg = code("""
      |package mypkg
      |fun f1(one: () -> String, two: String)  = println(one() + " " + two)
      |fun f2() = f1(two = "this",  one = { "that" })
      |""".stripMargin)

    "should contain a CALL node with arguments with their ARGUMENT_NAME property set" in {
      val List(c: Call) = cpg.method.nameExact("f1").callIn.l
      c.argument.map(_.argumentName).flatten.l shouldBe List("two", "one")
    }
  }

  "CPG for code with simple call having an anonymous function passed in with an argument name" should {
    val cpg = code("""
      |package mypkg
      |fun f1(one: () -> String, two: String)  = println(one() + " " + two)
      |fun f2() = f1(two = "this",  one = fun(): String { return "that" })
      |""".stripMargin)

    "should contain a CALL node with arguments with their ARGUMENT_NAME property set" in {
      val List(c: Call) = cpg.method.nameExact("f1").callIn.l
      c.argument.map(_.argumentName).flatten.l shouldBe List("two", "one")
    }
  }

  "have correct call to overriden base class" in {
    val cpg = code("""
        |package somePackage
        |class A: java.io.Closeable {
        |    fun foo() {
        |        close()
        |    }
        |    override fun close() {
        |    }
        |}
        |""".stripMargin)

    inside(cpg.call.nameExact("close").l) { case List(call) =>
      call.methodFullName shouldBe "somePackage.A.close:void()"
      call.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
      inside(call.receiver.l) { case List(receiver: Identifier) =>
        receiver.name shouldBe Constants.ThisName
        receiver.typeFullName shouldBe "somePackage.A"
      }
      inside(call.argument.l) { case List(argument: Identifier) =>
        argument.name shouldBe Constants.ThisName
        argument.argumentIndex shouldBe 0
      }
    }
  }

  "have correct call to kotlin standard library function" in {
    val cpg = code("""
        |fun method() {
        |  println("test")
        |}
        |""".stripMargin)

    inside(cpg.call.nameExact("println").l) { case List(call) =>
      call.methodFullName shouldBe "kotlin.io.println:void(java.lang.Object)"
      call.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      call.receiver.isEmpty shouldBe true
      inside(call.argument.l) { case List(argument: Literal) =>
        argument.code shouldBe "\"test\""
        argument.argumentIndex shouldBe 1
      }
    }
  }

  "have correct call to custom top level function" in {
    val cpg = code("""
        |package somePackage
        |fun topLevelFunc() {
        |}
        |fun method() {
        |  topLevelFunc()
        |}
        |""".stripMargin)

    inside(cpg.call.nameExact("topLevelFunc").l) { case List(call) =>
      call.methodFullName shouldBe "somePackage.topLevelFunc:void()"
      call.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

      call.receiver.isEmpty shouldBe true
      call.argument.isEmpty shouldBe true
    }
  }

  "have correct call to private class method" in {
    val cpg = code("""
                     |package somePackage
                     |class A {
                     |  private fun func1() {
                     |  }
                     |  fun func2() {
                     |    func1()
                     |  }
                     |}
                     |""".stripMargin)

    inside(cpg.call.nameExact("func1").l) { case List(call) =>
      call.methodFullName shouldBe "somePackage.A.func1:void()"
      call.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

      call.receiver.isEmpty shouldBe true
      inside(call.argument.l) { case List(argument: Identifier) =>
        argument.name shouldBe "this"
        argument.argumentIndex shouldBe 0
      }
    }
  }

  "have correct call for nested qualified expressions" in {
    val cpg = code("""
                     |package somePackage
                     |class A {
                     |  private val sub: A?;
                     |  fun func() {
                     |    sub?.sub?.func();
                     |  }
                     |}
                     |""".stripMargin)

    inside(cpg.call.nameExact("func").l) { case List(call) =>
      call.methodFullName shouldBe "somePackage.A.func:void()"
      call.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
    }
  }
}
