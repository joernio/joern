package io.joern.kotlin2cpg.validation

import io.joern.kotlin2cpg.Kt2CpgTestContext
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.edges.Ast
import io.shiftleft.codepropertygraph.generated.nodes.{ClosureBinding, FieldIdentifier, Identifier}
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.jIteratortoTraversal

class ValidationTests extends AnyFreeSpec with Matchers {
  "CPG for code with simple method containing if-expression" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |fun main(argc: Int): Int {
        |   val z: Int = if(argc > 0) argc else 0
        |   return z
        |}
        |""".stripMargin)

    "should not contain IDENTIFIER nodes with more than one incoming AST edge" in {
      cpg.identifier
        .filter(_.inE.filter { e => e.isInstanceOf[Ast] }.size > 1)
        .code
        .l shouldBe List()
    }

    "should not contain any IDENTIFIER nodes without inbound AST edges" in {
      cpg.identifier
        .filter(_._astIn.size == 0)
        .code
        .l shouldBe Seq()
    }
  }

  "CPG for code with simple method containing simple class declaration" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
      |package mypkg
      |
      |class AClass {
      |    fun main(argc: Int): Int {
      |       val z: Int = if(argc > 0) argc else 0
      |       return z
      |    }
      |}
      |""".stripMargin)

    "should not contain IDENTIFIER nodes with more than one incoming AST edge" in {
      cpg.identifier
        .filter(_.inE.filter { e => e.isInstanceOf[Ast] }.size > 1)
        .code
        .l shouldBe List()
    }

    "should not contain any IDENTIFIER nodes without inbound AST edges" in {
      cpg.identifier
        .filter(_._astIn.size == 0)
        .code
        .l shouldBe Seq()
    }
  }
  "CPG for code with simple lazy blocks" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
      |package mypkg
      |
      |import java.nio.file.Files
      |
      |fun main() {
      |    val customDir = Files.createTempDirectory("custom").toFile()
      |    val foo = lazy { customDir }
      |    println(foo)
      |}
      |""".stripMargin)

    "should not contain any identifiers without an ast parent" in {
      cpg.identifier
        .filter(_._astIn.size == 0)
        .code
        .l shouldBe Seq()
    }
  }

  "CPG for code call to DSL-like fn" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
      |package mypkg
      |
      |import org.http4k.core.HttpHandler
      |import org.http4k.core.Method.GET
      |import org.http4k.core.Response
      |import org.http4k.core.Request
      |import org.http4k.core.Status.Companion.OK
      |import org.http4k.routing.bind
      |import org.http4k.routing.to
      |import org.http4k.routing.routes
      |import org.http4k.server.SunHttp
      |import org.http4k.server.asServer
      |
      |import java.io.BufferedReader
      |import java.io.InputStreamReader
      |import java.lang.StringBuilder
      |
      |fun HelloWorld(): HttpHandler {
      |   val x = Response(OK).body("ok)
      |   return routes(
      |        "/health" bind GET to { Response(OK).body("ok") },
      |   )
      |}
      |""".stripMargin)

    "should not contain any CALL nodes with `null` in their NAME prop" in {
      cpg.call.filter { c => c.name == null }.code.l shouldBe List()
    }
  }

  "CPG for code with qualified expression inside qualified expression" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |fun main() {
        |    Runtime.getRuntime().exec("ls -al")
        |}
        |""".stripMargin)

    "should not contain any CALL nodes with `null` in their NAME prop" in {
      cpg.call.filter { c => c.name == null }.code.l shouldBe List()
    }
  }

  "CPG for code with a simple lambda which captures a method parameter" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
      |package mypkg
      |
      |fun foo(x: String): Int {
      |    1.let {
      |       println(x)
      |    }
      |   return 0
      |}
      |""".stripMargin)

    "should not contain any IDENTIFIER nodes without inbound AST edges" in {
      cpg.identifier
        .filter(_._astIn.size == 0)
        .code
        .l shouldBe Seq()
    }
  }

  "CPG for code with a simple lambda which captures a method parameter inside method" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |class AClass {
        |    fun doSomething(x: String) {
        |        1.let {
        |            println(x)
        |        }
        |    }
        |}
        |
        |""".stripMargin)

    "should not contain any IDENTIFIER nodes without inbound AST edges" in {
      cpg.identifier
        .filter(_._astIn.size == 0)
        .code
        .l shouldBe Seq()
    }
  }

  "CPG for code with a simple lambda which captures a method parameter, nested twice" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |fun foo(x: String): Int {
        |    1.let {
        |      2.let {
        |        println(x)
        |      }
        |    }
        |   return 0
        |}
        |""".stripMargin)

    "should not contain any IDENTIFIER nodes without inbound AST edges" in {
      cpg.identifier
        .filter(_._astIn.size == 0)
        .code
        .l shouldBe Seq()
    }
  }

  "CPG for code with a simple if-statement" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |fun main() {
        |    val msg = "y"
        |    if(msg == "y") {
        |        println("HELLO")
        |    }
        |}
        |""".stripMargin)

    "should not contain any IDENTIFIER nodes without inbound AST edges" in {
      cpg.identifier
        .filter(_._astIn.size == 0)
        .code
        .l shouldBe Seq()
    }
  }

  "CPG for code with simple `if`-statement" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |fun main() {
        |  val aList = listOf("a", "b", "c")
        |  val msg = "b"
        |  if(aList.contains(msg)) {
        |    println("HELLO")
        |  }
        |}
        |""".stripMargin)

    "should not contain any IDENTIFIER nodes without inbound AST edges" in {
      cpg.identifier
        .filter(_._astIn.size == 0)
        .code
        .l shouldBe Seq()
    }
  }

  "CPG for code with _safe call_ operator" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
      |package mypkg
      |
      |class AClass {
      |    fun printX(x: String?) {
      |        val msg = x
      |        msg?.let {
      |            println(it)
      |        }
      |    }
      |}
      |
      |fun main() {
      |    val a = AClass()
      |    a.printX("MSG")
      |}
      |""".stripMargin)

    "should not contain any IDENTIFIER nodes without inbound AST edges" in {
      cpg.identifier
        .filter(_._astIn.size == 0)
        .code
        .l shouldBe Seq()
    }
  }

  "CPG for code with call with lambda param inside try-statement" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
      |package mypkg
      |
      |fun main() {
      |    try {
      |        1.let{
      |          println("INSIDE_TRY")
      |        }
      |    } catch (e: Exception) {
      |        print("Exception caught.")
      |    }
      |}
      |""".stripMargin)

    "should contain a METHOD node for the lambda" in {
      cpg.method.fullName(".*lambda.*").size shouldBe 1
    }

    "should not contain any IDENTIFIER nodes without inbound AST edges" in {
      cpg.identifier
        .filter(_._astIn.size == 0)
        .code
        .l shouldBe Seq()
    }
  }

  "CPG for code with call with lambda param inside if-else-statement" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |import kotlin.random.Random
        |
        |fun main() {
        |    val rand = Random.nextInt(0,  100)
        |    if (rand < 50) {
        |        1.let {
        |            println("`rand` is smaller than 50: " + rand)
        |        }
        |    } else {
        |        2.let {
        |            println("`rand` is greater than or eq to 50: " + rand)
        |        }
        |    }
        |}
        |""".stripMargin)

    "should METHOD nodes for the lambdas" in {
      cpg.method.fullName(".*lambda.*").size shouldBe 2
    }

    "should not contain any IDENTIFIER nodes without inbound AST edges" in {
      cpg.identifier
        .filter(_._astIn.size == 0)
        .code
        .l shouldBe Seq()
    }
  }

  "CPG for code with call with lambda inside method definition" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |import kotlin.random.Random
        |
        |class AClass() {
        |    fun check(col: List<Int?>) {
        |        val rand = Random.nextInt(0, 100)
        |        when (rand) {
        |            1 -> println("1!")
        |            2 -> println("2!")
        |            else -> {
        |                val filtered = col.all { entry -> entry != null }
        |                println(filtered)
        |            }
        |        }
        |    }
        |}
        |
        |fun main() {
        |    val list = listOf(1, 2, 3)
        |    val a = AClass()
        |    a.check(list)
        |    println("SCOPE")
        |}
        |""".stripMargin)

    "should METHOD nodes for the lambdas" in {
      cpg.method.fullName(".*lambda.*").size shouldBe 1
    }

    "should not contain any IDENTIFIER nodes without inbound AST edges" in {
      cpg.identifier
        .filter(_._astIn.size == 0)
        .code
        .l shouldBe Seq()
    }
  }

  "CPG for code with anonymous function as argument" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |import kotlin.collections.List
        |import kotlin.collections.listOf
        |
        |fun foo(x: String): Int {
        |    val l: kotlin.collections.List = listOf(1, x)
        |    l.filter(fun(item) = { println(item); item > 0 })
        |    return 0
        |}
        |""".stripMargin)

    "should not contain any IDENTIFIER nodes without inbound AST edges" in {
      cpg.identifier
        .filter(_._astIn.size == 0)
        .code
        .l shouldBe Seq()
    }
  }

  "CPG for code with function defined inside another function" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |fun withInline(): Int {
        |    fun add1(x: Int): Int {
        |        return 1
        |    }
        |    return add1(1)
        |}
        |
        |fun main() {
        |    val x = withInline()
        |    println(x)
        |}
        |""".stripMargin)

    "should not contain any IDENTIFIER nodes without inbound AST edges" in {
      cpg.identifier
        .filter(_._astIn.size == 0)
        .code
        .l shouldBe Seq()
    }
  }

  "CPG for code with lambda inside while-statement" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |
        |package main
        |fun main() {
        |    val str = "ASTRING"
        |    while(true) {
        |        1.let {
        |            println(str)
        |        }
        |    }
        |}
        |""".stripMargin)

    "should not contain any IDENTIFIER nodes without inbound AST edges" in {
      cpg.identifier
        .filter(_._astIn.size == 0)
        .code
        .l shouldBe Seq()
    }
  }

  "CPG for code with with function which takes a lambda as an argument" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package main
        |
        |fun withCallback(callback: (String) -> Unit) {
        |    callback("AMESSAGE")
        |}
        |
        |fun main() {
        |    withCallback { println(it) }
        |}
        |""".stripMargin)

    "should not contain any METHOD nodes with FNs with a the `>` character in them" in {
      cpg.method
        .fullNameNot(".*<lambda>.*")
        .fullNameNot(".*<init>.*")
        .fullNameNot("<operator>.*")
        .fullName(".*>.*")
        .fullName
        .l shouldBe List()
    }

    "should not contain any CALL nodes with MFNs with a the `>` character in them" in {
      cpg.call
        .methodFullNameNot(".*<lambda>.*")
        .methodFullNameNot(".*<init>.*")
        .methodFullNameNot("<operator>.*")
        .methodFullName(".*>.*")
        .methodFullName
        .take(1)
        .l shouldBe List()
    }
  }

  "CPG for code with class with method which takes a lambda as an argument" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package main
        |
        |class AClass {
        |    fun withCallback(callback: (String) -> Unit) {
        |        callback("AMESSAGE")
        |    }
        |}
        |
        |fun main() {
        |    val a = AClass()
        |    a.withCallback { println(it) }
        |}
        |""".stripMargin)

    "should not contain any METHOD nodes with FNs with a the `>` character in them" in {
      cpg.method
        .fullNameNot(".*<lambda>.*")
        .fullNameNot(".*<init>.*")
        .fullNameNot("<operator>.*")
        .fullName(".*>.*")
        .fullName
        .l shouldBe List()
    }

    "should not contain any CALL nodes with MFNs with a the `>` character in them" in {
      cpg.call
        .methodFullNameNot(".*<lambda>.*")
        .methodFullNameNot(".*<init>.*")
        .methodFullNameNot("<operator>.*")
        .methodFullName(".*>.*")
        .methodFullName
        .l shouldBe List()
    }
  }

  "CPG for code with simple object expression with apply called after it" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package main
        |
        |fun main() {
        |    val o = object { var prop = 1 }.apply { prop = 2 }
        |    println(o.prop) // prints `2`
        |}
        |
        |""".stripMargin)

    "should not contain any CALL nodes with MFNs starting with `.`" in {
      cpg.call
        .methodFullName("\\..*")
        .methodFullName
        .l shouldBe List()
    }
  }

  "CPG for code with method with two callbacks with two generic types" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package main
        |
        |class AClass<T>(private val x: T) {
        |    fun <R> doWithTwoTs(cbOne: () -> R, cbTwo: (T) -> Unit) {
        |        println(cbOne())
        |        cbTwo(x)
        |    }
        |}
        |
        |fun main() {
        |    val x = 1
        |    val a = AClass(x)
        |    // prints
        |    //```
        |    //FIRST
        |    //SECOND: 3
        |    //```
        |    a.doWithTwoTs({ "FIRST" }, { val res = it + 2; println("SECOND: $res"); res })
        |}
        |
        |
        |""".stripMargin)

    "should not contain any CALL nodes with MFNs starting with `.`" in {
      cpg.call
        .methodFullName("\\..*")
        .methodFullName
        .l shouldBe List()
    }

    "should not contain any METHOD nodes with FNs with a the `>` character in them" in {
      cpg.method
        .fullNameNot(".*<lambda>.*")
        .fullNameNot(".*<init>.*")
        .fullNameNot("<operator>.*")
        .fullName(".*>.*")
        .fullName
        .l shouldBe List()
    }

    "should not contain any CALL nodes with MFNs with a the `>` character in them" in {
      cpg.call
        .methodFullNameNot(".*<lambda>.*")
        .methodFullNameNot(".*<init>.*")
        .methodFullNameNot("<operator>.*")
        .methodFullName(".*>.*")
        .map { c =>
          (c.methodFullName, c.code)
        }
        .l shouldBe List()
    }
  }

  "CPG for code with dynamic dispatch call inside lambda with class name in the receiver" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package main
        |
        |class BClass {
        |    val msg = "Hello from B!"
        |}
        |
        |internal object AnObject {
        |    private var m: BClass? = null
        |
        |    fun bClass(): BClass {
        |        return checkNotNull(m) {
        |            "You can't access `m` if you don't initialize it!"
        |        }
        |    }
        |
        |    fun initialize() {
        |        m = BClass()
        |    }
        |}
        |
        |fun main() {
        |    AnObject.initialize()
        |    1.let {
        |        val b = AnObject.bClass()
        |        print(b.msg)
        |    }
        |}
        |
        |
        |""".stripMargin)

    "should not contain any LOCAL nodes with the CLOSURE_BINDING_ID prop set but without corresponding CLOSURE_BINDING node" in {
      val allClosureBindingIds =
        cpg.all
          .collect { case c: ClosureBinding => c }
          .closureBindingId
          .l

      cpg.local
        .where(_.closureBindingId)
        .filterNot { l => allClosureBindingIds.contains(l.closureBindingId) }
        .map { cb => (cb.code, cb.closureBindingId) }
        .l shouldBe List()
    }
  }

  "CPG for code with local declaration with RHS a call with lambda argument capturing the parameter of its containing method" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package main
        |
        |fun getValidPredefs(startingWith: String): List<String> {
        |    val validPrefixes = listOf("one_predef", "two_predef", "three_predef")
        |    val afterFilter = validPrefixes.filter { it ->
        |        it.startsWith(startingWith)
        |    }
        |    return afterFilter
        |}
        |
        |fun main() {
        |    val toPrint = getValidPredefs("o")
        |    println(toPrint)
        |}
        |""".stripMargin)

    "should not contain any LOCAL nodes with the CLOSURE_BINDING_ID prop set but without corresponding CLOSURE_BINDING node" in {
      val allClosureBindingIds =
        cpg.all
          .collect { case c: ClosureBinding => c }
          .closureBindingId
          .l

      cpg.local
        .where(_.closureBindingId)
        .filterNot { l => allClosureBindingIds.contains(l.closureBindingId.get) }
        .map { cb => (cb.code, cb.closureBindingId) }
        .l shouldBe List()
    }
  }

  "CPG for code with lambda inside method with captured constructor parameter and method parameter" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package main
        |
        |class AClass constructor(val prefix: String = "default_prefix") {
        |    fun printX(x: String) {
        |       1.let {
        |            println(prefix + ": " + x)
        |       }
        |    }
        |}
        |
        |fun main() {
        |    val a = AClass("my_prefix")
        |    a.printX("a_message")
        |}
        |""".stripMargin)

    "should not contain any LOCAL nodes with the CLOSURE_BINDING_ID prop set but without corresponding CLOSURE_BINDING node" in {
      val allClosureBindingIds =
        cpg.all
          .collect { case c: ClosureBinding => c }
          .closureBindingId
          .l

      cpg.local
        .where(_.closureBindingId)
        .filterNot { l => allClosureBindingIds.contains(l.closureBindingId.get) }
        .map { cb => (cb.code, cb.closureBindingId) }
        .l shouldBe List()
    }
  }

}
