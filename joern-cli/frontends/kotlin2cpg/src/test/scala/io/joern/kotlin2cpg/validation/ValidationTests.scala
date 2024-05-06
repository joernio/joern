package io.joern.kotlin2cpg.validation

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.ClosureBinding
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.semanticcpg.language.*

class ValidationTests extends KotlinCode2CpgFixture(withOssDataflow = false) {
  "CPG for code with lambdas with no params and one param" should {
    lazy val cpg = code("""
        |package com.example
        |
        |fun parseParams(name: String, msg: String): Map<String, String?> {
        |    val checkedName = name.takeUnless { it -> it.contains('\\') }?.ifBlank { "default_name" }
        |    val checkedMsg = msg.ifBlank { "default_msg" }
        |    return mapOf("parsed_name" to checkedName, "parsed_msg" to checkedMsg)
        |}
        |
        |fun main() {
        |   val p1 = "PARAM_1"
        |   val p2 = "PARAM_2"
        |   val parsed = parseParams(p1, p2)
        |   println("parsed: " + parsed)
        |}
        |""".stripMargin)

    "should contain TYPE nodes for `kotlin.Function0`, `kotlin.Function1` and `kotlin.Pair`" in {
      cpg.typ.fullNameExact("kotlin.Function0").size should not be 0
      cpg.typ.fullNameExact("kotlin.Function1").size should not be 0
      cpg.typ.fullNameExact("kotlin.Pair").size should not be 0
    }

    "should contain CLOSURE_BINDING nodes for the lambdas" in {
      cpg.all.collectAll[ClosureBinding].size should not be 0
    }
  }

  "CPG for code with usage of `Gson` external library" should {
    lazy val cpg = code("""
        |package mypkg
        |
        |import com.google.gson.Gson
        |
        |fun main() {
        |   val l = ArrayList<String>()
        |   l.add("ONE")
        |   l.add("TWO")
        |   val j = Gson().toJson(l)
        |   println("json: " + j)
        |}
        |""".stripMargin)

    "should contain CALL node for the ctor-call with a METHOD_FULL_NAME starting with the package name" in {
      val List(c) = cpg.call.codeExact("Gson()").l
      c.methodFullName.startsWith("com.google.gson.Gson") shouldBe true
    }
  }

  "CPG for code with simple method containing if-expression" should {
    lazy val cpg = code("""
        |package mypkg
        |
        |fun main(argc: Int): Int {
        |   val z: Int = if(argc > 0) argc else 0
        |   return z
        |}
        |""".stripMargin)

    "should not contain IDENTIFIER nodes with more than one incoming AST edge" in {
      cpg.identifier
        .filter(_._astIn.size > 1)
        .code
        .l shouldBe List()
    }

    "should not contain any IDENTIFIER nodes without inbound AST edges" in {
      cpg.identifier
        .filter(_._astIn.isEmpty)
        .code
        .l shouldBe Seq()
    }
  }

  "CPG for code with simple method containing simple class declaration" should {
    lazy val cpg = code("""
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
        .filter(_._astIn.size > 1)
        .code
        .l shouldBe List()
    }

    "should not contain any IDENTIFIER nodes without inbound AST edges" in {
      cpg.identifier
        .filter(_._astIn.isEmpty)
        .code
        .l shouldBe Seq()
    }
  }
  "CPG for code with simple lazy blocks" should {
    lazy val cpg = code("""
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
        .filter(_._astIn.isEmpty)
        .code
        .l shouldBe Seq()
    }
  }

  "CPG for code call to DSL-like fn" should {
    lazy val cpg = code("""
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

  "CPG for code with qualified expression inside qualified expression" should {
    lazy val cpg = code("""
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

  "CPG for code with a simple lambda which captures a method parameter" should {
    lazy val cpg = code("""
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
        .filter(_._astIn.isEmpty)
        .code
        .l shouldBe Seq()
    }
  }

  "CPG for code with a simple lambda which captures a method parameter inside method" should {
    lazy val cpg = code("""
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
        .filter(_._astIn.isEmpty)
        .code
        .l shouldBe Seq()
    }
  }

  "CPG for code with a simple lambda which captures a method parameter, nested twice" should {
    lazy val cpg = code("""
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
        .filter(_._astIn.isEmpty)
        .code
        .l shouldBe Seq()
    }
  }

  "CPG for code with a simple if-statement" should {
    lazy val cpg = code("""
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
        .filter(_._astIn.isEmpty)
        .code
        .l shouldBe Seq()
    }
  }

  "CPG for code with simple `if`-statement" should {
    lazy val cpg = code("""
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
        .filter(_._astIn.isEmpty)
        .code
        .l shouldBe Seq()
    }
  }

  "CPG for code with _safe call_ operator" should {
    lazy val cpg = code("""
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
        .filter(_._astIn.isEmpty)
        .code
        .l shouldBe Seq()
    }
  }

  "CPG for code with call with lambda param inside try-statement" should {
    lazy val cpg = code("""
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
        .filter(_._astIn.isEmpty)
        .code
        .l shouldBe Seq()
    }
  }

  "CPG for code with call with lambda param inside if-else-statement" should {
    lazy val cpg = code("""
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
        .filter(_._astIn.isEmpty)
        .code
        .l shouldBe Seq()
    }
  }

  "CPG for code with call with lambda inside method declaration" should {
    lazy val cpg = code("""
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
        .filter(_._astIn.isEmpty)
        .code
        .l shouldBe Seq()
    }
  }

  "CPG for code with anonymous function as argument" should {
    lazy val cpg = code("""
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
        .filter(_._astIn.isEmpty)
        .code
        .l shouldBe Seq()
    }
  }

  "CPG for code with function defined inside another function" should {
    lazy val cpg = code("""
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
        .filter(_._astIn.isEmpty)
        .code
        .l shouldBe Seq()
    }
  }

  "CPG for code with lambda inside while-statement" should {
    lazy val cpg = code("""
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
        .filter(_._astIn.isEmpty)
        .code
        .l shouldBe Seq()
    }
  }

  "CPG for code with with function which takes a lambda as an argument" should {
    lazy val cpg = code("""
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

  "CPG for code with class with method which takes a lambda as an argument" should {
    lazy val cpg = code("""
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

  "CPG for code with method with two callbacks with two generic types" should {
    lazy val cpg = code("""
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

  "CPG for code with call to `checkNotNull`" should {
    lazy val cpg = code("""
        |fun f1(x: String?) {
        |    val notNullX = checkNotNull(x) { println("SOMETHING") }
        |    println(notNullX)
        |}
        |
        |fun main() {
        |    f1("SOMETHING")
        |}
        |""".stripMargin)

    "should not contain any LOCAL nodes with the CLOSURE_BINDING_ID prop set but without corresponding CLOSURE_BINDING node" in {
      val allClosureBindingIds = cpg.all.collect { case c: ClosureBinding => c }.closureBindingId.l

      cpg.local
        .where(_.closureBindingId)
        .filterNot { l => allClosureBindingIds.contains(l.closureBindingId.get) }
        .map { cb => (cb.code, cb.closureBindingId, cb.getClass.toString) }
        .l shouldBe List()
    }
  }

  "CPG for code with local declaration with RHS a call with lambda argument capturing the parameter of its containing method" should {
    lazy val cpg = code("""
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
      val allClosureBindingIds = cpg.all.collectAll[ClosureBinding].closureBindingId.l
      cpg.local
        .where(_.closureBindingId)
        .filterNot { l => allClosureBindingIds.contains(l.closureBindingId.get) }
        .map { cb => (cb.code, cb.closureBindingId) }
        .l shouldBe List()
    }
  }

  "CPG for code with lambda inside method with captured constructor parameter and method parameter" should {
    lazy val cpg = code("""
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
      val allClosureBindingIds = cpg.all.collectAll[ClosureBinding].closureBindingId.l
      cpg.local
        .where(_.closureBindingId)
        .filterNot { l => allClosureBindingIds.contains(l.closureBindingId.get) }
        .map { cb => (cb.code, cb.closureBindingId) }
        .l shouldBe List()
    }
  }

  "CPG for code with method that has a suspend lambda parameter" should {
    lazy val cpg = code("""
        |
        |package main
        |
        |import kotlinx.coroutines.*
        |
        |fun doSomething(block: suspend (String) -> Unit) {
        |     runBlocking {
        |        val waitAndExec = async {
        |            println("BEFORE")
        |            delay(1000)
        |            block("ARG_FROM_INSIDE")
        |            println("AFTER")
        |        }
        |        waitAndExec.await()
        |    }
        |}
        |
        |fun main() {
        |    val toSuspend = {x: String ->
        |        println("VALUE_IN_BLOCK: " + x)
        |    }
        |    doSomething(toSuspend)
        |}
        |""".stripMargin)

    "should not contain any LOCAL nodes with the CLOSURE_BINDING_ID prop set but without corresponding CLOSURE_BINDING node" in {
      val allClosureBindingIds = cpg.all.collectAll[ClosureBinding].closureBindingId.l
      cpg.local
        .where(_.closureBindingId)
        .filterNot { l => allClosureBindingIds.contains(l.closureBindingId.get) }
        .map { cb => (cb.code, cb.closureBindingId) }
        .l shouldBe List()
    }

    "should not contain any METHOD nodes with FNs with a the `>` character in them" in {
      cpg.method
        .fullNameNot(".*<lambda>.*")
        .fullNameNot(".*<init>.*")
        .fullNameNot("<operator>.*")
        .fullNameNot("<unresolvedNamespace>.*")
        .fullName(".*>.*")
        .fullName
        .l shouldBe List()
    }

    "should not contain any CALL nodes with MFNs with a the `>` character in them" in {
      cpg.call
        .methodFullNameNot(".*<lambda>.*")
        .methodFullNameNot(".*<init>.*")
        .methodFullNameNot("<operator>.*")
        .methodFullNameNot("<unresolvedNamespace>.*")
        .methodFullName(".*>.*")
        .methodFullName
        .l shouldBe List()
    }
  }

  "CPG for code with call to qualified expression with parenthesized elvis expression as receiver" should {
    lazy val cpg = code("""
      |package mypkg
      |
      |import java.time.Duration
      |
      |fun main() {
      |  val timeout = Duration.ofSeconds(1)
      |  val sleep = Duration.ofSeconds(2)
      |  val millis = (timeout ?: sleep).toMillis()
      |  println("milliseconds: " + millis)
      |}
      |""".stripMargin)

    "should not contain CALL nodes with DYNAMIC_DISPATCH prop set, but without an argument with ARGUMENT_INDEX 0" in {
      cpg.call
        .dispatchTypeExact(DispatchTypes.DYNAMIC_DISPATCH)
        .filter(_.argument.argumentIndex(0).isEmpty)
        .code
        .l shouldBe List()
    }
  }

  "CPG for code with call to qualified expression with parenthesized if-else-expression as receiver" should {
    lazy val cpg = code("""
       |package mypkg
       |
       |import kotlin.random.Random
       |
       |fun main() {
       |    val x = Random.nextBoolean()
       |    val y = 41414141
       |    val z = 42424242
       |    val p =
       |        (if (x) {
       |            y
       |        } else {
       |            z
       |        }).toString()
       |    println("p: " + p)
       |}
       |""".stripMargin)

    "should not contain CALL nodes with DYNAMIC_DISPATCH prop set, but without an argument with ARGUMENT_INDEX 0" in {
      cpg.call
        .dispatchTypeExact(DispatchTypes.DYNAMIC_DISPATCH)
        .filter(_.argument.argumentIndex(0).isEmpty)
        .code
        .l shouldBe List()
    }
  }

  "CPG for code with variants of user-defined constructors" should {
    lazy val cpg = code("""
       |package mypkg
       |
       |class AClass
       |class BClass(val x: String)
       |class CClass(var y: String) {
       |    constructor(p: String, q:String) : this(p) {
       |        this.y = p
       |        println("q: " + q)
       |    }
       |}
       |
       |fun main() {
       |    val a = AClass()
       |    val b = BClass("A_MESSAGE")
       |    val c1 = CClass("A_MESSAGE")
       |    val c2 = CClass("A_MESSAGE", "ANOTHER_MESSAGE")
       |    println(a)
       |    println(b.x)
       |    println(c1.y)
       |    println(c2.y)
       |}
       |""".stripMargin)

    "should contain matching ctor-call/ctor-def pairs on their respective FULL_NAMEs" in {
      val List(aCtor)     = cpg.method.fullName(".*AClass.*init.*").l
      val List(aCtorCall) = cpg.call.methodFullName(".*AClass.*init.*").l
      aCtorCall.methodFullName shouldBe aCtor.fullName

      val List(bCtor)     = cpg.method.fullName(".*BClass.*init.*").l
      val List(bCtorCall) = cpg.call.methodFullName(".*BClass.*init.*").l
      bCtorCall.methodFullName shouldBe bCtor.fullName

      val ctorMethods = cpg.method.fullName(".*CClass.*init.*").toSet
      val ctorCalls   = cpg.call.methodFullName(".*CClass.*init.*").toSet
      ctorCalls.map(_.methodFullName).subsetOf(ctorMethods.map(_.fullName)) shouldBe true
    }
  }

  "CPG for code with `fieldAccess` call on Java stdlib type" should {
    lazy val cpg = code("""
        |package main
        |
        |import java.io.File
        |
        |fun main() {
        |    val sep = File.separator
        |    println("sep: " + sep)
        |}
        |""".stripMargin)

    "should not contain `fieldAccess` CALL nodes with less than to ARGUMENT edges" in {
      cpg.call
        .methodFullName(Operators.fieldAccess)
        .filter(_.argument.size < 2)
        .code
        .l shouldBe List()
    }
  }

  "CPG for code with `fieldAccess` call" should {
    lazy val cpg = code("""
        |package main
        |
        |class AClass {
        |    var x: String = "INITIAL"
        |}
        |
        |fun doSomething(p1: String): String {
        |    val aClass = AClass()
        |    aClass.x = p1
        |    return aClass.x
        |}
        |
        |fun main() {
        |    val out = doSomething("AMESSAGE")
        |    println(out)
        |}
        |""".stripMargin)

    "should not contain `fieldAccess` CALLs with an operator in the METHOD_FULL_NAME but no operator in the NAME" in {
      cpg.call
        .methodFullName(Operators.fieldAccess)
        .name
        .dedup
        .l shouldBe List(Operators.fieldAccess)
    }
  }

  "CPG for code with `fieldAccess` call on implicit _this_" should {
    lazy val cpg = code("""
        |package main
        |
        |class AClass {
        |    val AMESSAGE = "AMESSAGE"
        |    fun printMsg() {
        |        println(AMESSAGE)
        |    }
        |}
        |
        |fun main() {
        |    val aClass = AClass()
        |    aClass.printMsg()
        |}
        |""".stripMargin)

    "should not contain `fieldAccess` CALLs with DYNAMIC_DISPATCH" in {
      cpg.call
        .methodFullName(Operators.fieldAccess)
        .dispatchTypeExact(DispatchTypes.DYNAMIC_DISPATCH)
        .code
        .l shouldBe List()
    }
  }
}
