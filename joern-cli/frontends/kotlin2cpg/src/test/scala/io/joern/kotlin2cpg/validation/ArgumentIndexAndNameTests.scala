package io.joern.kotlin2cpg.validation

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.ControlStructureTypes
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class ArgumentIndexAndNameTests extends KotlinCode2CpgFixture(withOssDataflow = false) {

  "CPG for named arguments of various expression kinds" should {
    lazy val cpg = code("""
        |package mypkg
        |
        |object O
        |fun sink(p: Any?) {}
        |fun bar() {}
        |fun Int.ext(): Int = this + 1
        |
        |class A(val m: Int) {
        |    fun member() { sink(p = m) }
        |    fun self() { sink(p = this) }
        |}
        |
        |fun thrower() { sink(p = throw IllegalStateException()) }
        |
        |fun main(x: Any, b: Boolean, arr: IntArray) {
        |    var i = 0
        |    sink(p = "a$x")
        |    sink(p = String::class)
        |    sink(p = x is String)
        |    sink(p = !b)
        |    sink(p = i++)
        |    sink(p = arr[0])
        |    sink(p = ::bar)
        |    sink(p = when { b -> 1; else -> 2 })
        |    sink(p = O)
        |    sink(p = 1.ext())
        |}
        |""".stripMargin)

    def argOfSinkAt(line: Int): Expression = {
      val List(arg) = cpg.call.nameExact("sink").lineNumber(line).argument.l
      arg
    }

    "set the name on the argument root for every kind" in {
      val expectedByLine = Map(
        10 -> classOf[Call],             // member reference -> this.m field access
        11 -> classOf[Identifier],       // this
        14 -> classOf[ControlStructure], // throw
        18 -> classOf[Call],             // interpolated string -> formatString
        19 -> classOf[Call],             // class literal
        20 -> classOf[Call],             // is
        21 -> classOf[Call],             // prefix
        22 -> classOf[Call],             // postfix
        23 -> classOf[Call],             // array access
        24 -> classOf[MethodRef],        // callable reference
        25 -> classOf[Call],             // when without subject -> conditional
        26 -> classOf[TypeRef],          // object reference
        27 -> classOf[Call]              // extension call
      )
      expectedByLine.foreach { case (line, cls) =>
        val arg = argOfSinkAt(line)
        withClue(s"line $line (${arg.code}): ") {
          cls.isInstance(arg) shouldBe true
          arg.argumentName shouldBe Some("p")
          arg.argumentIndex shouldBe 1
        }
      }
    }

    "use the expected root for operator arguments" in {
      argOfSinkAt(10).asInstanceOf[Call].name shouldBe Operators.fieldAccess
      argOfSinkAt(14).asInstanceOf[ControlStructure].controlStructureType shouldBe ControlStructureTypes.THROW
      argOfSinkAt(18).asInstanceOf[Call].name shouldBe Operators.formatString
      argOfSinkAt(20).asInstanceOf[Call].name shouldBe Operators.is
      argOfSinkAt(23).asInstanceOf[Call].name shouldBe Operators.indexAccess
      argOfSinkAt(25).asInstanceOf[Call].name shouldBe Operators.conditional
    }

    "not put the argument name on any node other than the argument itself" in {
      val named = cpg.all.collect { case e: Expression if e.argumentName.contains("p") => e.id() }.toSet
      named shouldBe cpg.call.nameExact("sink").argument.id.toSet
    }
  }

  "CPG for a call mixing positional and named arguments with skipped defaults" should {
    lazy val cpg = code("""
        |package mypkg
        |
        |fun createButton(
        |    text: String,
        |    color: String = "Blue",
        |    width: Int = 100
        |    isEnabled: Boolean = true
        |) {}
        |
        |fun main() {
        |    createButton("Submit", isEnabled = false)
        |}
        |""".stripMargin)

    "index arguments by their position in the call and name only the named one" in {
      val List(call) = cpg.call.nameExact("createButton").l
      call.argument.map(a => (a.code, a.argumentIndex, a.argumentName)).l.sortBy(_._2) shouldBe List(
        ("\"Submit\"", 1, None),
        ("false", 2, Some("isEnabled"))
      )
    }
  }

  "CPG for arguments of a call with a receiver" should {
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

    "give the receiver index 0 and the argument index 1" in {
      val calls = cpg.call.nameExact("m").l
      calls.size shouldBe 16
      calls.foreach { call =>
        withClue(s"line ${call.lineNumber.get} (${call.code}): ") {
          call.argument.argumentIndex.sorted.l shouldBe List(0, 1)
          call.argument(0).code shouldBe "c"
          call.argument(1).argumentName shouldBe None
        }
      }
    }
  }

  "CPG for statements in fixed child slots" should {
    lazy val cpg = code("""
        |package mypkg
        |
        |fun f(x: Int, l: List<Int>) {
        |    for (a in l) {
        |        when (x) {
        |            1 -> return
        |            else -> throw IllegalStateException()
        |        }
        |    }
        |    for (a in l) try { println(a) } catch (e: Exception) {}
        |}
        |""".stripMargin)

    "give when-entry bodies the same index scheme regardless of their kind" in {
      val List(ret) = cpg.ret.lineNumber(7).l
      ret.argumentIndex shouldBe 2
      val List(thr) = cpg.controlStructure.controlStructureTypeExact(ControlStructureTypes.THROW).l
      thr.argumentIndex shouldBe 3
    }

    "give a statement for-body its body slot index" in {
      val List(tryNode) = cpg.controlStructure.controlStructureTypeExact(ControlStructureTypes.TRY).l
      tryNode.argumentIndex shouldBe 3
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

    "keep the default index and no name" in {
      val List(println1) = cpg.call.nameExact("println").lineNumber(5).l
      println1.argumentIndex shouldBe -1
      val List(assignment) = cpg.call.nameExact(Operators.assignment).l
      assignment.argumentIndex shouldBe -1
      cpg.controlStructure.controlStructureTypeExact(ControlStructureTypes.IF).argumentIndex.l shouldBe List(-1)
      cpg.all.collect { case e: Expression if e.argumentName.isDefined => e.code }.l shouldBe List()
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

  "CPG for calls with a receiver and a sole METHOD_REF argument" should {
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
