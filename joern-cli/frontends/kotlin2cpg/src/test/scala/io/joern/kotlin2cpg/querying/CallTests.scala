package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.TestContext
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.Identifier
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.semanticcpg.language._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class CallTests extends AnyFreeSpec with Matchers {

  implicit val resolver = NoResolve

  "CPG for code with two functions with the same name, but different params" - {
    lazy val cpg = TestContext.buildCpg("""
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

    "should contain a call node for `argc`'s declaration with correct fields" in {
      cpg.call(Operators.assignment).size shouldBe 1

      val List(c) = cpg.call(Operators.assignment).l
      c.argument.size shouldBe 2
      c.code shouldBe "val argc: Int = args.size"
      c.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      c.lineNumber shouldBe Some(8)
      c.columnNumber shouldBe Some(6)
    }

    "should contain a call node for `x + y` with correct fields" in {
      cpg.call(Operators.addition).size shouldBe 1

      val List(c) = cpg.call(Operators.addition).l
      c.argument.size shouldBe 2
      c.code shouldBe "x + y"
      c.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      c.lineNumber shouldBe Some(4)
      c.columnNumber shouldBe Some(9)
    }

    // TODO: check for the dispatch types as well
    "should contain a call node for `println` with correct fields" in {
      cpg.call("println").size shouldBe 1

      val List(p) = cpg.call("println").l
      p.argument.size shouldBe 1
      p.lineNumber shouldBe Some(9)
      p.code shouldBe "println(foo(argc, 1))"
      p.methodFullName shouldBe "kotlin.io.println:void(java.lang.Integer)"
      p.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      p.columnNumber shouldBe Some(2)
    }

    "should contain a call node for `foo` with correct fields" in {
      cpg.call("foo").size shouldBe 1

      val List(p) = cpg.call("foo").l
      p.argument.size shouldBe 2
      p.lineNumber shouldBe Some(9)
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
      // TODO: check why the dedupBy is needed
      val List(x) = cpg.call.code("foo.*").callee.dedupBy(_.id).l
      x.name shouldBe "foo"
    }

    "should allow traversing from argument to parameter" in {
      // TODO: check why the dedupBy is needed
      val List(x) = cpg.call.code("foo.*").argument(1).parameter.dedupBy(_.id).l
      x.name shouldBe "x"
    }
  }

  "CPG for code with a class declaration " - {
    lazy val cpg = TestContext.buildCpg("""
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
      p.lineNumber shouldBe Some(11)
    }

    "should contain a CALL node for `add1` with the correct props set" in {
      val List(p) = cpg.call("add1").l
      p.argument.size shouldBe 3
      p.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
      p.code shouldBe "x.add1(argc, \"AMESSAGE\")"
      p.columnNumber shouldBe Some(10)
      p.lineNumber shouldBe Some(12)
      p.methodFullName shouldBe "mypkg.Foo.add1:java.lang.Integer(java.lang.Integer,java.lang.String)"
      p.signature shouldBe "java.lang.Integer(java.lang.Integer,java.lang.String)"
      p.typeFullName shouldBe "java.lang.Integer"

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

  "CPG for code with a call to an implicitly imported stdlib fn " - {
    lazy val cpg = TestContext.buildCpg("""
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

  "CPG for code with a call to a constructor from library with default content root jar" - {
    lazy val cpg = TestContext.buildCpg(
      """
        |package mypkg
        |
        |import com.google.gson.Gson
        |
        |fun main() {
        |  val serialized = Gson().toJson(productList)
        |  println(serialized)
        |}
        |""".stripMargin,
      includeAllJars = true
    )

    "should contain a call node for `Gson()`" in {
      val List(c) = cpg.call.methodFullName(".*Gson.*init.*").l
      c.methodFullName shouldBe "com.google.gson.Gson.<init>:void()"
      c.signature shouldBe "void()"
    }
  }

  "CPG for code with invocation of extension function from stdlib" - {
    lazy val cpg = TestContext.buildCpg("""
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

  "CPG for code with QE inside QE" - {
    lazy val cpg = TestContext.buildCpg("""
      |package mypkg
      |
      |fun main() {
      |    Runtime.getRuntime().exec("ls -al")
      |    println("DONE")
      |}
      |""".stripMargin)

    "should contain a CALL node " in {
      val List(c) = cpg.call.code("Runtime.*").codeNot(".*exec.*").l
      c.methodFullName shouldBe "java.lang.Runtime.getRuntime:java.lang.Runtime()"
      c.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      c.signature shouldBe "java.lang.Runtime()"
      c.name shouldBe "getRuntime"
      c.typeFullName shouldBe "java.lang.Runtime"
    }
  }

  "CPG for code with call simple stdlib fn for map creation" - {
    lazy val cpg = TestContext.buildCpg("""
      |import kotlin.collections.mutableMapOf
      |
      |fun main(args : Array<String>) {
      |  val numbersMap = mutableMapOf("one" to 1, "two" to 2)
      |  numbersMap["one"] = 11
      |  println(numbersMap)
      |}
      |""".stripMargin)

    "should contain a CALL node with the correct props set" in {
      val List(c) = cpg.call.code("mutableMapOf.*").l
      c.methodFullName shouldBe "kotlin.collections.mutableMapOf:java.util.Map(kotlin.Array)"
      c.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      c.typeFullName shouldBe "java.util.Map"
      c.lineNumber shouldBe Some(4)
      c.columnNumber shouldBe Some(19)
    }
  }

  "CPG for code with a simple method call to decl of Java's stdlib" - {
    lazy val cpg = TestContext.buildCpg("""
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

  "CPG for code with " - {
    lazy val cpg = TestContext.buildCpg("""
    |package mypkg
    |
    |fun main() {
    |    val str = "ASTRING"
    |    val res = str.length.toString()
    |    println(res)
    |}
    |
    |""".stripMargin)

    "should contain a CALL node for `p.length` with the correct props set" in {
      val List(c) = cpg.call.codeExact("str.length").l
      c.methodFullName shouldBe Operators.fieldAccess
      c.signature shouldBe ""
      c.typeFullName shouldBe "java.lang.Integer"
      c.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
    }

    "should contain a CALL node for `p.length.toString` with the correct props set" in {
      val List(c) = cpg.call.code("str.length.toString.*").l
      c.methodFullName shouldBe "kotlin.Int.toString:java.lang.String()"
      c.signature shouldBe "java.lang.String()"
      c.typeFullName shouldBe "java.lang.String"
      c.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
    }
  }

  "CPG for code with a simple call to class from Java's stdlib imported with _as_" - {
    lazy val cpg = TestContext.buildCpg("""
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

  "CPG for code with a simple call with unknown identifier imported via _as_" - {
    lazy val cpg = TestContext.buildCpg("""
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
      c.lineNumber shouldBe Some(6)
      c.columnNumber shouldBe Some(12)
      c.signature shouldBe ""
    }

    "should contain a CALL node for `MyCaseClass(\\\"AN_ARGUMENT\\\")` with the correct props set" in {
      val List(c) = cpg.call.code("MyCaseClass.*AN_ARGUMENT.*").l
      c.methodFullName shouldBe "MyCaseClass:ANY(ANY)"
      c.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      c.lineNumber shouldBe Some(9)
      c.columnNumber shouldBe Some(17)
      c.signature shouldBe "ANY(ANY)"
    }
  }

  "CPG for code with call to `100.toFloat`" - {
    lazy val cpg = TestContext.buildCpg("""
     |package mypkg
     |
     |fun main() {
     |  100.toFloat()
     |}
     |""".stripMargin)

    "should contain a CALL node for `.*toFloat()` with the correct props set" in {
      val List(c) = cpg.call.code(".*toFloat.*").l
      c.methodFullName shouldBe "kotlin.Int.toFloat:java.lang.Float()"
    }
  }

  "CPG for code with call which has parenthesized expression as receiver" - {
    lazy val cpg = TestContext.buildCpg("""
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
      c.methodFullName shouldBe "kotlin.Int.toFloat:java.lang.Float()"
    }
  }

  "CPG for code with usage of stdlib's `to` on stdlib objects" - {
    lazy val cpg = TestContext.buildCpg("""
        |package mypkg
        |
        |fun main() {
        |  val map = mapOf(1 to "x", 2 to "y", -1 to "zz")
        |  println(map) // {1=x, 2=y, -1=zz}
        |}
        |
        |""".stripMargin)

    "should contain a CALL node with METHOD_FULLNAME with `java.lang.Object` in its parameters" in {
      val List(c) = cpg.call.code("1 to.*").l
      c.methodFullName shouldBe "kotlin.to:kotlin.Pair(java.lang.Object)"
    }
  }

  "CPG for code with usage of stdlib's `to` on user-defined objects" - {
    lazy val cpg = TestContext.buildCpg("""
        |package mypkg
        |
        |class AClass(val msg: String)
        |
        |fun main() {
        |    val map = mapOf(1 to AClass("one"), 2 to AClass("two"), -1 to AClass("three"))
        |    println(map) // {1=mypkg.AClass@5ebec15, 2=mypkg.AClass@21bcffb5, -1=mypkg.AClass@380fb434}
        |}
        |
        |""".stripMargin)

    "should contain a CALL node with METHOD_FULLNAME with `java.lang.Object` in its parameters" in {
      val List(c) = cpg.call.code("1 to.*").l
      c.methodFullName shouldBe "kotlin.to:kotlin.Pair(java.lang.Object)"
    }
  }
}
