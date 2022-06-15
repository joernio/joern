package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.Identifier
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.semanticcpg.language._

class CallTests extends KotlinCode2CpgFixture(withOssDataflow = false) {

  implicit val resolver = NoResolve

  "CPG for code with two functions with the same name, but different params" should {
    lazy val cpg = code("""
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
    lazy val cpg = code("""
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
      val List(p) = cpg.call.methodFullName(".*init.*").l
      p.methodFullName shouldBe "mypkg.Foo.<init>:void()"
      p.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      p.signature shouldBe "void()"
      p.code shouldBe "Foo()"
      p.columnNumber shouldBe Some(10)
      p.lineNumber shouldBe Some(12)
    }

    "should contain a CALL node for `add1` with the correct props set" in {
      val List(p) = cpg.call("add1").l
      p.argument.size shouldBe 3
      p.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
      p.code shouldBe "x.add1(argc, \"AMESSAGE\")"
      p.columnNumber shouldBe Some(10)
      p.lineNumber shouldBe Some(13)
      p.methodFullName shouldBe "mypkg.Foo.add1:int(int,java.lang.String)"
      p.signature shouldBe "int(int,java.lang.String)"
      p.typeFullName shouldBe "int"

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
    lazy val cpg = code("""
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
    lazy val cpg = code("""
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
    lazy val cpg = code("""
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
      val List(firstArg: Identifier, secondArg: Identifier) = cpg.call.code("r.exec.*").argument.l
      firstArg.typeFullName shouldBe "java.lang.Runtime"
      secondArg.typeFullName shouldBe "java.lang.String"
    }
  }

  "CPG for code with a simple call to class from Java's stdlib imported with _as_" should {
    lazy val cpg = code("""
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
      c.methodFullName shouldBe "java.io.File.writeText:void(java.lang.String,java.nio.charset.Charset)"
    }
  }

  "CPG for code with a simple call with unknown identifier imported via _as_" should {
    lazy val cpg = code("""
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
      c.methodFullName shouldBe "no.such.CaseClass:ANY(ANY)"
      c.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      c.lineNumber shouldBe Some(10)
      c.columnNumber shouldBe Some(17)
      c.signature shouldBe "ANY(ANY)"
    }
  }

  "CPG for code with call which has parenthesized expression as receiver" should {
    lazy val cpg = code("""
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
}
