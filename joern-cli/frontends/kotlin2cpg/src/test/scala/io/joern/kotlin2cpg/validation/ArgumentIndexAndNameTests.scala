package io.joern.kotlin2cpg.validation

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.ControlStructureTypes
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class ArgumentIndexAndNameTests extends KotlinCode2CpgFixture(withOssDataflow = false) {

  private case class Arg(code: String, index: Int, name: Option[String] = None)

  private def argsOf(call: Call): List[Arg] =
    call.argument.l.sortBy(_.argumentIndex).map(a => Arg(a.code, a.argumentIndex, a.argumentName))

  private def callIn(cpg: Cpg, method: String, callName: String): Call = {
    val List(call) = cpg.method.nameExact(method).call.nameExact(callName).l
    call
  }

  /** The node type an expression lowers to, e.g. `CALL <operator>.fieldAccess` or `METHOD_REF`. */
  private def loweredTo(e: Expression): String = e match {
    case c: Call              => s"CALL ${c.name}"
    case cs: ControlStructure => s"CONTROL_STRUCTURE ${cs.controlStructureType}"
    case other                => other.label
  }

  "CPG for a named argument of each expression kind" should {
    lazy val cpg = code("""
        |package mypkg
        |
        |object O
        |fun sink(p: Any?) {}
        |fun bar() {}
        |fun Int.ext(): Int = this + 1
        |
        |class A(val m: Int) {
        |    fun memberReference() { sink(p = m) }
        |    fun thisReference() { sink(p = this) }
        |}
        |fun throwExpression() { sink(p = throw IllegalStateException()) }
        |fun stringTemplate(x: Any) { sink(p = "a$x") }
        |fun classLiteral() { sink(p = String::class) }
        |fun isCheck(x: Any) { sink(p = x is String) }
        |fun prefixOperator(b: Boolean) { sink(p = !b) }
        |fun postfixOperator() { var i = 0; sink(p = i++) }
        |fun arrayAccess(arr: IntArray) { sink(p = arr[0]) }
        |fun callableReference() { sink(p = ::bar) }
        |fun whenWithoutSubject(b: Boolean) { sink(p = when { b -> 1; else -> 2 }) }
        |fun objectReference() { sink(p = O) }
        |fun extensionCall() { sink(p = 1.ext()) }
        |""".stripMargin)

    val loweringOfEachKind = List(
      "memberReference"    -> s"CALL ${Operators.fieldAccess}",
      "thisReference"      -> "IDENTIFIER",
      "throwExpression"    -> s"CONTROL_STRUCTURE ${ControlStructureTypes.THROW}",
      "stringTemplate"     -> s"CALL ${Operators.formatString}",
      "classLiteral"       -> "CALL <operator>.class",
      "isCheck"            -> s"CALL ${Operators.is}",
      "prefixOperator"     -> s"CALL ${Operators.logicalNot}",
      "postfixOperator"    -> s"CALL ${Operators.postIncrement}",
      "arrayAccess"        -> s"CALL ${Operators.indexAccess}",
      "callableReference"  -> "METHOD_REF",
      "whenWithoutSubject" -> s"CALL ${Operators.conditional}",
      "objectReference"    -> "TYPE_REF",
      "extensionCall"      -> "CALL ext"
    )

    "put the name and index on the node the argument lowers to" in {
      loweringOfEachKind.foreach { case (kind, expectedRoot) =>
        withClue(s"$kind: ") {
          val List(arg) = callIn(cpg, kind, "sink").argument.l
          loweredTo(arg) shouldBe expectedRoot
          arg.argumentName shouldBe Some("p")
          arg.argumentIndex shouldBe 1
        }
      }
    }

    "not leak the name onto nodes inside the argument" in {
      val nodesNamedP = cpg.all.collect { case e: Expression if e.argumentName.contains("p") => e }.l
      nodesNamedP.map(_.id()).toSet shouldBe cpg.call.nameExact("sink").argument.id.toSet
    }
  }

  "CPG for a call that skips default parameters" should {
    lazy val cpg = code("""
        |package mypkg
        |
        |fun createButton(text: String, color: String = "Blue", width: Int = 100, isEnabled: Boolean = true) {}
        |
        |fun main() {
        |    createButton("Submit", isEnabled = false)
        |}
        |""".stripMargin)

    "index arguments by their position in the call, not in the declaration" in {
      // isEnabled is the 4th parameter but the 2nd argument
      argsOf(callIn(cpg, "main", "createButton")) shouldBe List(
        Arg("\"Submit\"", 1),
        Arg("false", 2, Some("isEnabled"))
      )
    }
  }

  "CPG for a named argument in each kind of call" should {
    lazy val cpg = code("""
        |package mypkg
        |
        |class Foo(a: Int, b: Int)
        |data class D(val a: Int, val b: Int)
        |class C { fun m(a: Int, b: Int) {} }
        |open class P { open fun m(a: Int, b: Int) {} }
        |class Q : P() { fun superCall() { super.m(1, b = 2) } }
        |fun Int.ext(a: Int, b: Int) {}
        |
        |fun ctorCall() { Foo(1, b = 2) }
        |fun ctorInProperty() { val x = Foo(1, b = 2) }
        |fun ctorInDestructuring() { val (p, q) = D(1, b = 2) }
        |fun qualifiedCtorCall() { mypkg.Foo(1, b = 2) }
        |fun memberCall(c: C) { c.m(1, b = 2) }
        |fun extensionCall() { 3.ext(1, b = 2) }
        |""".stripMargin)

    "give the constructed object index 0 and the constructor arguments 1 and 2" in {
      List("ctorCall", "ctorInProperty", "ctorInDestructuring", "qualifiedCtorCall").foreach { method =>
        withClue(s"$method: ") {
          val receiver :: arguments = argsOf(callIn(cpg, method, "<init>")): @unchecked
          receiver.index shouldBe 0 // a temporary or the declared variable, depending on the path
          arguments shouldBe List(Arg("1", 1), Arg("2", 2, Some("b")))
        }
      }
    }

    "give super and a member call's receiver index 0" in {
      argsOf(callIn(cpg, "superCall", "m")) shouldBe List(Arg("super", 0), Arg("1", 1), Arg("2", 2, Some("b")))
      argsOf(callIn(cpg, "memberCall", "m")) shouldBe List(Arg("c", 0), Arg("1", 1), Arg("2", 2, Some("b")))
    }

    "pass an extension call's receiver as the first argument" in {
      argsOf(callIn(cpg, "extensionCall", "ext")) shouldBe List(Arg("3", 1), Arg("1", 2), Arg("2", 3, Some("b")))
    }
  }

  "CPG for a member call with each kind of positional argument" should {
    lazy val cpg = code("""
        |package mypkg
        |
        |object O
        |class C { fun m(a: Any?) {} }
        |
        |fun main(x: Any, b: Boolean, arr: IntArray, c: C) {
        |    var i = 0
        |    c.m("a$x")
        |    c.m(String::class)
        |    c.m(x is String)
        |    c.m(x as String)
        |    c.m(!b)
        |    c.m(i++)
        |    c.m(arr[0])
        |    c.m(when { b -> 1; else -> 2 })
        |    c.m(when (i) { 1 -> 1; else -> 2 })
        |    c.m(if (b) 1 else 2)
        |    c.m(try { 1 } catch (e: Exception) { 2 })
        |    c.m(object {})
        |    c.m(1 + 2)
        |    c.m(O)
        |    c.m(C())
        |    c.m(this)
        |}
        |""".stripMargin)

    "give the receiver index 0 and the unnamed argument index 1" in {
      val calls = cpg.call.nameExact("m").l
      calls.size shouldBe 16
      calls.foreach { call =>
        withClue(s"${call.code}: ") {
          val List(receiver, argument) = argsOf(call)
          receiver shouldBe Arg("c", 0)
          argument.index shouldBe 1
          argument.name shouldBe None
        }
      }
    }
  }

  "CPG for expressions that are not arguments" should {
    lazy val cpg = code("""
        |package mypkg
        |
        |fun f(b: Boolean) {
        |    println(1)
        |    when { b -> println(2); else -> println(3) }
        |    if (b) println(4) else println(5)
        |    val y = listOf(1)
        |}
        |""".stripMargin)

    "leave statements at the default index -1" in {
      cpg.call.codeExact("println(1)").argumentIndex.l shouldBe List(-1)
      callIn(cpg, "f", Operators.assignment).argumentIndex shouldBe -1
      cpg.controlStructure.controlStructureTypeExact(ControlStructureTypes.IF).argumentIndex.l shouldBe List(-1)
    }

    "not name any node" in {
      cpg.all.collect { case e: Expression if e.argumentName.isDefined => e.code }.l shouldBe empty
    }
  }

  "CPG for a when statement whose subject declares a variable" should {
    lazy val cpg = code("""
        |package mypkg
        |
        |fun source(): Int = 42
        |fun sink(x: Int) {}
        |
        |fun g() {
        |    when (val y = source()) {
        |        1 -> sink(y)
        |        else -> sink(0)
        |    }
        |}
        |""".stripMargin)

    "keep the initializer of the subject variable" in {
      pendingUntilFixed {
        cpg.call.nameExact("source").method.name.l shouldBe List("g")
        cpg.assignment.code.l shouldBe List("val y = source()")
      }
    }
  }

  "CPG for a member call whose only argument is a METHOD_REF" should {
    lazy val cpg = code("""
        |package mypkg
        |
        |class C { fun m(a: Any?) {} }
        |fun bar() {}
        |
        |fun main(c: C) {
        |    c.m(::bar)
        |    c.m({ 1 })
        |    c.m(fun() = 1)
        |}
        |""".stripMargin)

    "keep the receiver as argument 0 and the METHOD_REF as argument 1" in {
      pendingUntilFixed {
        val calls = cpg.call.nameExact("m").l
        calls.size shouldBe 3
        calls.foreach { call =>
          withClue(s"${call.code}: ") {
            call.argument.argumentIndex.sorted.l shouldBe List(0, 1)
            call.argument(0).code shouldBe "c"
            call.argument(1) shouldBe a[MethodRef]
            call.receiver.code.l shouldBe List("c")
          }
        }
      }
    }
  }
}
