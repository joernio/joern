package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.TestContext
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.semanticcpg.language._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class DefaultContentRootsTests extends AnyFreeSpec with Matchers {

  "CPG for code with a simple function definition with parameters of stdlib types, but not fully specified" - {
    lazy val cpg = TestContext.buildCpg("""
        |package mypkg
        |
        |fun add1mul(x: Int, y: Int): Int {
        |  return (x + 1) * y
        |}
        |""".stripMargin)

    "should contain two METHOD_PARAMETER_IN nodes with the correct type fullnames" in {
      def params = cpg.parameter.filter(_.method.name == "add1mul")

      val List(x) = params.name("x").l
      x.code shouldBe "x"
      x.typeFullName shouldBe "java.lang.Integer"

      val List(y) = params.name("y").l
      y.code shouldBe "y"
      y.typeFullName shouldBe "java.lang.Integer"
    }
  }

  "CPG for code with a simple class definition" - {
    lazy val cpg = TestContext.buildCpg("""
        |package mypkg
        |
        |class Foo {
        |  fun bar(x: Int, y: Int): Int {
        |    return x * 2
        |  }
        |}
        |""".stripMargin)

    "should contain two METHOD_PARAMETER nodes with the correct type fullnames" in {
      def params = cpg.parameter.filter(_.method.name == "bar")
      params.name.toSet shouldBe Set("x", "y")

      val List(x) = params.name("x").l
      x.code shouldBe "x"
      x.typeFullName shouldBe "java.lang.Integer"

      val List(y) = params.name("y").l
      y.code shouldBe "y"
      y.typeFullName shouldBe "java.lang.Integer"
    }
  }

  "CPG for code with type alias of a stdlib type" - {
    lazy val cpg = TestContext.buildCpg("""
        |typealias FooList = List<Int>
        |
        |fun foo() {
        |  val myList: FooList = listOf(1, 2, 3)
        |  println(myList)
        |}
        |""".stripMargin)

    "should contain type decl for alias `FooList` of `List<Int>` with the correct aliasTypeFullName set" in {
      val List(x) = cpg.typeDecl("FooList").l
      x.aliasTypeFullName shouldBe Some("java.util.List")
    }
  }

  "CPG for code with array access" - {
    lazy val cpg = TestContext.buildCpg("""
        |fun foo(): Int {
        |  val x = listOf(1, 2, 3)
        |  return x[0]
        |}
        |""".stripMargin)

    "should contain IDENTIFIER nodes for `x` with the correct typeFullNames set" in {
      val List(i) = cpg.identifier.codeExact("x").where(_.inCall.methodFullNameExact(Operators.assignment)).l
      i.typeFullName shouldBe "java.util.List"
    }
  }

  "CPG for code with `this` expression" - {
    lazy val cpg = TestContext.buildCpg("""
        |package mypkg
        |
        |class Foo {
        |  fun bar()  {
        |    val x = this
        |    println(x)
        |  }
        |}
        | """.stripMargin)

    "should contain IDENTIFIER nodes for `this` with the correct typeFullNames set" in {
      val typeFullNames = cpg.identifier.codeExact("this").typeFullName.toSet
      typeFullNames shouldBe Set("mypkg.Foo")
    }
  }

  "CPG for code with a class definition" - {
    lazy val cpg = TestContext.buildCpg("""
        |package mypkg
        |
        |class Foo: Object {
        |  val z: Int = 1
        |
        |  fun add1(x: Int): Int {
        |    return x + 1
        |  }
        |}
        | """.stripMargin)

    "should contain a MEMBER node for `z` with the correct typeFullName set" in {
      val List(m) = cpg.typeDecl.fullName(".*Foo.*").member.name("z").l
      m.typeFullName shouldBe "java.lang.Integer"
    }
  }

  "CPG for code with Java stdlib" - {
    lazy val cpg = TestContext.buildCpg("""
        |package mypkg
        |
        |fun foo(cmd: String) {
        |    val r = Runtime.getRuntime()
        |    r.exec(cmd)
        |}
        | """.stripMargin)

    "should contain a CALL node for `Runtime.getRuntime` with the correct methodFullName set" in {
      val List(c) = cpg.call.code("Runtime.*").l
      c.methodFullName shouldBe "java.lang.Runtime.getRuntime:java.lang.Runtime()"
    }

    "should contain a IDENTIFIER nodes for `r` with the correct typeFullNames set" in {
      val typeFullNames = cpg.identifier.codeExact("r").typeFullName.toSet
      typeFullNames shouldBe Set("java.lang.Runtime")
    }

    "should contain a CALL node for `r.execute` with the correct methodFullName set" in {
      val List(c) = cpg.call.code("r.exec.*").l
      c.methodFullName shouldBe "java.lang.Runtime.exec:java.lang.Process(java.lang.String)"
    }
  }

  "CPG for code using the Javalin web framework" - {
    lazy val cpg = TestContext.buildCpg(
      """
        |package mypkg
        |
        |import io.javalin.Javalin
        |
        |fun main() {
        |  val app = Javalin.create().start(7000)
        |
        |  app.get("/status") { ctx -> ctx.result("ok") }
        |
        |  app.get("/exec/:program") { ctx ->
        |    val prog = ctx.pathParam("program")
        |    val invocation = prog
        |    val proc = Runtime.getRuntime().exec(invocation)
        |    val out = proc.getInputStream().bufferedReader().use { it.readText() }
        |    ctx.result("----Executed `" + invocation + "` with result: `" + out  + "`.\n-----\n")
        |  }
        |
        |  app.get("/exec/*/*") { ctx ->
        |    val invocation = ctx.header("x-program")
        |    val proc = Runtime.getRuntime().exec(invocation)
        |    val out = proc.getInputStream().bufferedReader().use { it.readText() }
        |    ctx.result("----Executed `" + invocation + "` with result: `" + out  + "`.\n-----\n")
        |  }
        |}
        | """.stripMargin,
      includeAllJars = true
    )

    "should contain a CALL node for `Runtime.getRuntime.*exec` with the correct methodFullNames set" in {
      val List(c) = cpg.call.code("Runtime.*exec.*").take(1).l
      c.methodFullName shouldBe "java.lang.Runtime.exec:java.lang.Process(java.lang.String)"
      c.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
    }

    "should contain IDENTIFIER nodes for `prog` with the correct typeFullNames set" in {
      val typeFullNames = cpg.identifier.codeExact("prog").typeFullName.toSet
      typeFullNames shouldBe Set("java.lang.String")
    }

    "should contain IDENTIFIER nodes for `proc` with the correct typeFullNames set" in {
      val typeFullNames =
        cpg.identifier
          .codeExact("proc")
          .typeFullName
          .toSet
      typeFullNames shouldBe Set("java.lang.Process")
    }

    "should contain a CALL node for `cpx.pathParam` with the correct methodFullName set" in {
      val List(c) = cpg.call.code("ctx.pathParam.*").l
      c.methodFullName shouldBe "io.javalin.http.Context.pathParam:java.lang.String(java.lang.String)"
      c.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
    }

    "should contain a CALL node for `ctx.body` with the correct methodFullName set" in {
      val List(c) = cpg.call.code("ctx.header.*").l
      c.methodFullName shouldBe "io.javalin.http.Context.header:java.lang.String(java.lang.String)"
      c.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
    }

    "should contain IDENTIFIER nodes for `app` with the correct typeFullNames set" in {
      val typeFullNames = cpg.identifier.codeExact("app").typeFullName.toSet
      typeFullNames shouldBe Set("io.javalin.Javalin")
    }

    "should contain IDENTIFIER nodes for `invocation` with the correct typeFullNames set" in {
      val typeFullNames = cpg.identifier.codeExact("invocation").typeFullName.toSet
      typeFullNames shouldBe Set("java.lang.String")
    }
  }

  "CPG for code with CALL to `super`" - {
    lazy val cpg = TestContext.buildCpg(
      """
        |package mypkg
        |
        |import android.content.Intent
        |import android.content.IntentFilter
        |import android.os.Bundle
        |import android.view.View
        |import android.app.Activity
        |
        |class AboutUsActivity : Activity() {
        |    override fun onCreate(savedInstanceState: Bundle?) {
        |        super.onCreate(savedInstanceState)
        |    }
        |}
        | """.stripMargin,
      includeAllJars = true
    )

    "should contain a CALL node for `onCreate` with the correct props set" in {
      def createCall = cpg.call.code(".*onCreate.*")

      val List(c) = createCall.l
      c.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      c.methodFullName shouldBe "android.app.Activity.onCreate:void(android.os.Bundle)"
      c.argument.size shouldBe 2

      val List(firstArg, secondArg) = createCall.argument.l
      firstArg.code shouldBe "super"
      firstArg.argumentIndex shouldBe 0

      secondArg.code shouldBe "savedInstanceState"
      secondArg.argumentIndex shouldBe 1
    }
  }

  "CPG for code using the http4k framework" - {
    lazy val cpg = TestContext.buildCpg(
      """
        |package com.example
        |
        |import org.http4k.core.HttpHandler
        |import org.http4k.core.Method.GET
        |import org.http4k.core.Response
        |import org.http4k.core.Request
        |import org.http4k.core.Status.Companion.OK
        |import org.http4k.routing.bind
        |import org.http4k.routing.routes
        |import org.http4k.server.SunHttp
        |import org.http4k.server.asServer
        |
        |import java.io.BufferedReader
        |import java.io.InputStreamReader
        |import java.lang.StringBuilder
        |
        |fun HelloWorld(): HttpHandler {
        |    return routes(
        |        "/health" bind GET to { Response(OK).body("ok") },
        |        "/exec" bind GET to { req ->
        |            val cmd = req.query("cmd")
        |            val proc = Runtime.getRuntime().exec(cmd)
        |            val lineReader = BufferedReader(InputStreamReader(proc.getInputStream()));
        |            val output = StringBuilder()
        |            lineReader.lines().forEach { line ->
        |                output.append(line + "\n")
        |            }
        |            Response(OK).body("Did execute command `" + cmd + "`, got stdout:" + output)
        |        },
        |    )
        |}
        |
        |fun main() {
        |    val port = 8080
        |    println("Serving content on port " + port.toString() + ".")
        |    HelloWorld().asServer(SunHttp(port)).start()
        |}
        | """.stripMargin,
      includeAllJars = true
    )

    "should contain a CALL node for `port.toString()` with the correct methodFullName set" in {
      val List(c) = cpg.call.codeExact("port.toString()").l
      c.methodFullName shouldBe "kotlin.Int.toString:java.lang.String()"
    }

    "should contain a CALL node for `routes` with the correct methodFullName set" in {
      val List(c) = cpg.call.methodFullName(".*routes.*").l
      // TODO: remove the _out_ from the methodFullName
      c.methodFullName shouldBe "org.http4k.routing.routes:org.http4k.routing.RoutingHttpHandler(kotlin.Array)"
    }

    "should contain a CALL node for `req.query` with the correct methodFullName set" in {
      val List(c) = cpg.call.code("req.query.*").l
      c.methodFullName shouldBe "org.http4k.core.Request.query:java.lang.String(java.lang.String)"
    }

    "should contain a CALL node for `Runtime.getRuntime.*exec` with the correct methodFullName set" in {
      val List(c) = cpg.call.code("Runtime.*exec.*").take(1).l
      c.methodFullName shouldBe "java.lang.Runtime.exec:java.lang.Process(java.lang.String)"
    }
  }

  "CPG for code with addition of aliased type and its original" - {
    lazy val cpg = TestContext.buildCpg("""
        |package mypkg
        |
        |typealias MyInt = Int
        |
        |fun foo() {
        |  val x: MyInt = 1
        |  val y: Int = 2
        |  val bar = x + y
        |  println(bar)
        |}
        |""".stripMargin)

    "should have the correct types inferred" in {
      val List(identifierForAliasedType) = cpg.identifier.codeExact("x").take(1).l
      identifierForAliasedType.typeFullName shouldBe "java.lang.Integer"

      val List(identifierForOriginalType) = cpg.identifier.codeExact("y").take(1).l
      identifierForOriginalType.typeFullName shouldBe "java.lang.Integer"

      val List(identifierForOpResult) = cpg.identifier.codeExact("bar").take(1).l
      identifierForOpResult.typeFullName shouldBe "java.lang.Integer"
    }
  }
}
