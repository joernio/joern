package io.joern.kotlin2cpg.dataflow

import io.joern.dataflowengineoss.language.toExtendedCfgNode
import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.semanticcpg.language._

class SimpleDataFlowTests extends KotlinCode2CpgFixture(withOssDataflow = true) {
  "CPG for code with simple function" should {
    val cpg = code("""
        |package mypkg
        |fun doSomething(x: Int): Int { return x + 1 }
        |""".stripMargin)

    "should find a flow from method parameter to method return" in {
      val source = cpg.method.name("doSomething").parameter
      val sink   = cpg.method.name("doSomething").block.expressionDown.isReturn
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(List(("doSomething(x)", Some(3)), ("x + 1", Some(3)), ("return x + 1", Some(3))))
    }
  }

  "CPG for code with simple arithmetic operators" should {
    val cpg = code("""
        |package mypkg
        |
        |fun doSomething(x: Int): Int {
        |  val add41 = x + 41
        |  val subtract41 = x - 41
        |  val multiplyBy41 = x * 41
        |  val divideBy41 = x / 41
        |  return 41
        |}
        |""".stripMargin)

    def source = cpg.method.name("doSomething").parameter

    "should find a flow through addition" in {
      val sink  = cpg.identifier.name("add41")
      val flows = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(List(("doSomething(x)", Some(4)), ("x + 41", Some(5)), ("val add41 = x + 41", Some(5))))
    }

    "should find a flow through subtraction" in {
      val sink  = cpg.identifier.name("subtract41")
      val flows = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(List(("doSomething(x)", Some(4)), ("x - 41", Some(6)), ("val subtract41 = x - 41", Some(6))))
    }

    "should find a flow through multiplication" in {
      val sink  = cpg.identifier.name("multiplyBy41")
      val flows = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(
          List(
            ("doSomething(x)", Some(4)),
            ("x - 41", Some(6)),
            ("x * 41", Some(7)),
            ("val multiplyBy41 = x * 41", Some(7))
          )
        )
    }

    "should find a flow through division" in {
      val sink  = cpg.identifier.name("divideBy41")
      val flows = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(
          List(
            ("doSomething(x)", Some(4)),
            ("x - 41", Some(6)),
            ("x * 41", Some(7)),
            ("x / 41", Some(8)),
            ("val divideBy41 = x / 41", Some(8))
          )
        )
    }
  }

  "CPG for code with simple assignment operators" should {
    val cpg = code("""
        |package mypkg
        |
        |fun doSomething(x: Int): Int {
        |  val modifyBy = 41
        |
        |  val x = 1
        |  x += modifyBy
        |  val addEq = x
        |
        |  val y = 1
        |  y -= modifyBy
        |  val subtractEq = y
        |
        |  val z = 1
        |  z *= modifyBy
        |  val multiplyEq = z
        |
        |  val p = 1
        |  p /= modifyBy
        |  val divideEq = p
        |
        |  return 41
        |}
        |""".stripMargin)

    def source = cpg.literal.code("41")

    "should find a flow through assign-addition" in {
      val sink  = cpg.identifier.name("addEq")
      val flows = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(List(("val modifyBy = 41", Some(5)), ("x += modifyBy", Some(8)), ("val addEq = x", Some(9))))
    }

    "should find a flow through assign-subtraction" in {
      val sink  = cpg.identifier.name("subtractEq")
      val flows = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(List(("val modifyBy = 41", Some(5)), ("y -= modifyBy", Some(12)), ("val subtractEq = y", Some(13))))
    }

    "should find a flow through assign-multiplication" in {
      val sink  = cpg.identifier.name("multiplyEq")
      val flows = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(List(("val modifyBy = 41", Some(5)), ("z *= modifyBy", Some(16)), ("val multiplyEq = z", Some(17))))
    }

    "should find a flow through assign-division" in {
      val sink  = cpg.identifier.name("divideEq")
      val flows = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(List(("val modifyBy = 41", Some(5)), ("p /= modifyBy", Some(20)), ("val divideEq = p", Some(21))))
    }
  }

  "CPG for code using null safety operators" should {
    val cpg = code("""
        |package mypkg
        |
        |fun doSomething(p: Int?): Int {
        |    val afterElvis = p ?: 42424242
        |    val maybeCast = afterElvis as? Int
        |    val forcedNonOptional = maybeCast!!
        |    return forcedNonOptional
        |}
        |""".stripMargin)

    "should find a flow through the `elvis` operator" in {
      val source = cpg.method.name("doSomething").parameter
      val sink   = cpg.identifier.name("afterElvis")
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(
          List(
            ("doSomething(p)", Some(4)),
            ("p ?: 42424242", Some(5)),
            ("val afterElvis = p ?: 42424242", Some(5)),
            ("afterElvis as? Int", Some(6))
          ),
          List(("doSomething(p)", Some(4)), ("p ?: 42424242", Some(5)), ("val afterElvis = p ?: 42424242", Some(5)))
        )
    }

    "should find a flow through the safe cast" in {
      val source = cpg.method.name("doSomething").parameter
      val sink   = cpg.identifier.name("maybeCast")
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(
          List(
            ("doSomething(p)", Some(4)),
            ("p ?: 42424242", Some(5)),
            ("val afterElvis = p ?: 42424242", Some(5)),
            ("afterElvis as? Int", Some(6)),
            ("val maybeCast = afterElvis as? Int", Some(6))
          ),
          List(
            ("doSomething(p)", Some(4)),
            ("p ?: 42424242", Some(5)),
            ("val afterElvis = p ?: 42424242", Some(5)),
            ("afterElvis as? Int", Some(6)),
            ("val maybeCast = afterElvis as? Int", Some(6)),
            ("maybeCast!!", Some(7))
          )
        )
    }

    "should find a flow through the not-null assert operator" in {
      val source = cpg.method.name("doSomething").parameter
      val sink   = cpg.identifier.name("forcedNonOptional")
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(
          List(
            ("doSomething(p)", Some(4)),
            ("p ?: 42424242", Some(5)),
            ("val afterElvis = p ?: 42424242", Some(5)),
            ("afterElvis as? Int", Some(6)),
            ("val maybeCast = afterElvis as? Int", Some(6)),
            ("maybeCast!!", Some(7)),
            ("val forcedNonOptional = maybeCast!!", Some(7))
          )
        )
    }
  }

  "CPG for code with `if` control expressions without blocks" should {
    val cpg = code("""
        |package mypkg
        |
        |import kotlin.random.Random
        |
        |fun doSomething(p: Int): Int {
        |  val someVal = p
        |
        |  val afterThen = if (p % 2 == 0) someVal else 42
        |  println(afterThen)
        |
        |  val afterElse = if (p % 2 == 0) 42 else someVal
        |  println(afterElse)
        |
        |  return 41
        |}
        |""".stripMargin)

    "should find a flow through the `then` branch" in {
      val source = cpg.method.name("doSomething").parameter
      val sink   = cpg.identifier.name("afterThen")
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(
          List(
            ("doSomething(p)", Some(6)),
            ("val someVal = p", Some(7)),
            ("if (p % 2 == 0) someVal else 42", Some(9)),
            ("val afterThen = if (p % 2 == 0) someVal else 42", Some(9)),
            ("println(afterThen)", Some(10))
          ),
          List(
            ("doSomething(p)", Some(6)),
            ("val someVal = p", Some(7)),
            ("if (p % 2 == 0) someVal else 42", Some(9)),
            ("val afterThen = if (p % 2 == 0) someVal else 42", Some(9))
          )
        )
    }

    "should find a flow through the `else` branch" in {
      val source = cpg.method.name("doSomething").parameter
      val sink   = cpg.identifier.name("afterElse")
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(
          List(
            ("doSomething(p)", Some(6)),
            ("val someVal = p", Some(7)),
            ("if (p % 2 == 0) 42 else someVal", Some(12)),
            ("val afterElse = if (p % 2 == 0) 42 else someVal", Some(12)),
            ("println(afterElse)", Some(13))
          ),
          List(
            ("doSomething(p)", Some(6)),
            ("val someVal = p", Some(7)),
            ("if (p % 2 == 0) 42 else someVal", Some(12)),
            ("val afterElse = if (p % 2 == 0) 42 else someVal", Some(12))
          )
        )
    }
  }

  "CPG for code with `if` control expressions with blocks" should {
    val cpg = code("""
        |package mypkg
        |
        |import kotlin.random.Random
        |
        |fun doSomething(p: Int): Int {
        |  val someVal = p
        |
        |  val afterThen = if (p % 2 == 0) { someVal } else { 42 }
        |  println(afterThen)
        |
        |  val afterElse = if (p % 2 == 0) { 42 } else { someVal }
        |  println(afterElse)
        |
        |  return 41
        |}
        |
        |fun main() {
        |  val dicey = Random.nextInt()
        |  doSomething(dicey)
        |}
        |""".stripMargin)

    "should find a flow through the `then` branch" in {
      val source = cpg.method.name("doSomething").parameter
      val sink   = cpg.identifier.name("afterThen")
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(
          List(
            ("doSomething(p)", Some(6)),
            ("p % 2", Some(9)),
            ("if (p % 2 == 0) { someVal } else { 42 }", Some(9)),
            ("val afterThen = if (p % 2 == 0) { someVal } else { 42 }", Some(9)),
            ("println(afterThen)", Some(10))
          ),
          List(
            ("doSomething(p)", Some(6)),
            ("p % 2", Some(9)),
            ("if (p % 2 == 0) { someVal } else { 42 }", Some(9)),
            ("val afterThen = if (p % 2 == 0) { someVal } else { 42 }", Some(9))
          )
        )
    }

    "should find a flow through the `else` branch" in {
      val source = cpg.method.name("doSomething").parameter
      val sink   = cpg.identifier.name("afterElse")
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(
          List(
            ("doSomething(p)", Some(6)),
            ("p % 2", Some(9)),
            ("if (p % 2 == 0) { someVal } else { 42 }", Some(9)),
            ("val afterThen = if (p % 2 == 0) { someVal } else { 42 }", Some(9)),
            ("println(afterThen)", Some(10)),
            ("if (p % 2 == 0) { 42 } else { someVal }", Some(12)),
            ("val afterElse = if (p % 2 == 0) { 42 } else { someVal }", Some(12)),
            ("println(afterElse)", Some(13))
          ),
          List(
            ("doSomething(p)", Some(6)),
            ("p % 2", Some(9)),
            ("if (p % 2 == 0) { someVal } else { 42 }", Some(9)),
            ("val afterThen = if (p % 2 == 0) { someVal } else { 42 }", Some(9)),
            ("println(afterThen)", Some(10)),
            ("if (p % 2 == 0) { 42 } else { someVal }", Some(12)),
            ("val afterElse = if (p % 2 == 0) { 42 } else { someVal }", Some(12))
          )
        )
    }
  }

  "CPG for code with `try` control expressions" should {
    val cpg = code("""
        |import kotlin.random.Random
        |
        |fun doSomething(p: Int): Int {
        |  val someVal = p
        |
        |  val afterBody = try { someVal } catch(e: Exception) { 0 }
        |  println(afterBody)
        |
        |  val afterCatch = try { 41 / 0 } catch(e: Exception) { someVal }
        |  println(afterCatch)
        |
        |  return 41
        |}
        |""".stripMargin)

    "should find a flow through the `try` branch" in {
      val source = cpg.method.name("doSomething").parameter
      val sink   = cpg.identifier.name("afterBody")
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(
          List(
            ("doSomething(p)", Some(6)),
            ("val someVal = p", Some(7)),
            ("someVal", Some(9)),
            ("try { someVal } catch(e: Exception) { 0 }", Some(9)),
            ("val afterBody = try { someVal } catch(e: Exception) { 0 }", Some(9)),
            ("println(afterBody)", Some(10))
          ),
          List(
            ("doSomething(p)", Some(6)),
            ("val someVal = p", Some(7)),
            ("someVal", Some(9)),
            ("try { someVal } catch(e: Exception) { 0 }", Some(9)),
            ("val afterBody = try { someVal } catch(e: Exception) { 0 }", Some(9))
          )
        )
    }

    "should find a flow through the `catch` branch" in {
      val source = cpg.method.name("doSomething").parameter
      val sink   = cpg.identifier.name("afterCatch")
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(
          List(
            ("doSomething(p)", Some(6)),
            ("val someVal = p", Some(7)),
            ("someVal", Some(9)),
            ("try { someVal } catch(e: Exception) { 0 }", Some(9)),
            ("val afterBody = try { someVal } catch(e: Exception) { 0 }", Some(9)),
            ("println(afterBody)", Some(10)),
            ("someVal", Some(12)),
            ("try { 41 / 0 } catch(e: Exception) { someVal }", Some(12)),
            ("val afterCatch = try { 41 / 0 } catch(e: Exception) { someVal }", Some(12)),
            ("println(afterCatch)", Some(13))
          ),
          List(
            ("doSomething(p)", Some(6)),
            ("val someVal = p", Some(7)),
            ("someVal", Some(9)),
            ("try { someVal } catch(e: Exception) { 0 }", Some(9)),
            ("val afterBody = try { someVal } catch(e: Exception) { 0 }", Some(9)),
            ("println(afterBody)", Some(10)),
            ("someVal", Some(12)),
            ("try { 41 / 0 } catch(e: Exception) { someVal }", Some(12)),
            ("val afterCatch = try { 41 / 0 } catch(e: Exception) { someVal }", Some(12))
          )
        )
    }
  }

  "CPG for code with `if` control structures" should {
    val cpg = code("""
        |package mypkg
        |
        |import kotlin.random.Random
        |
        |fun doSomething(p: Int): Int {
        |  var tickOne = 0
        |  if (p % 2 == 0) {
        |    tickOne = p
        |  } else {
        |    println("NOPNOPNOPNOPNOPNOPNOPNOPNOPNOP")
        |  }
        |  val afterIfFromThen = tickOne
        |  println(afterIfFromThen)
        |
        |  var tickTwo = 0
        |  if (p % 2 == 0) {
        |    println("NOPNOPNOPNOPNOPNOPNOPNOPNOPNOP")
        |  } else {
        |    tickTwo = p
        |  }
        |  val afterIfFromElse = tickTwo
        |  println(afterIfFromElse)
        |
        |  return 41
        |}
        |""".stripMargin)

    "should find a flow through `then`-block of `if` control structure" in {
      val source = cpg.method.name("doSomething").parameter
      val sink   = cpg.identifier.name("afterIfFromThen")
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(
          List(
            ("doSomething(p)", Some(6)),
            ("p % 2", Some(8)),
            ("tickOne = p", Some(9)),
            ("val afterIfFromThen = tickOne", Some(13)),
            ("println(afterIfFromThen)", Some(14))
          ),
          List(
            ("doSomething(p)", Some(6)),
            ("p % 2", Some(8)),
            ("tickOne = p", Some(9)),
            ("val afterIfFromThen = tickOne", Some(13))
          )
        )
    }

    "should find a flow through `else`-block of `if` control structure" in {
      val source = cpg.method.name("doSomething").parameter
      val sink   = cpg.identifier.name("afterIfFromElse")
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(
          List(
            ("doSomething(p)", Some(6)),
            ("p % 2", Some(8)),
            ("p % 2", Some(17)),
            ("tickTwo = p", Some(20)),
            ("val afterIfFromElse = tickTwo", Some(22)),
            ("println(afterIfFromElse)", Some(23))
          ),
          List(
            ("doSomething(p)", Some(6)),
            ("p % 2", Some(8)),
            ("p % 2", Some(17)),
            ("tickTwo = p", Some(20)),
            ("val afterIfFromElse = tickTwo", Some(22))
          )
        )
    }
  }

  "CPG for code with `try` control structures" should {
    val cpg = code("""
        |import kotlin.random.Random
        |
        |fun doSomething(p: Int): Int {
        |  var tickOne = 0
        |  try {
        |    0x41414141 / 0
        |  } catch (e: Exception) {
        |    tickOne = p
        |  }
        |  val afterTryFromCatch = tickOne
        |  println(afterTryFromCatch)
        |
        |  var tickTwo = 0
        |  try {
        |    tickTwo = p
        |    0x41414141 / 0
        |  } catch (e: Exception) {
        |    println("NOPNOPNOPNOPNOPNOPNOPNOPNOPNOP")
        |  }
        |  val afterTryFromBody = tickTwo
        |  println(afterTryFromBody)
        |
        |  return 41
        |}
        |""".stripMargin)

    "should find a flow through `catch`-block of `try` control structure" in {
      val source = cpg.method.name("doSomething").parameter
      val sink   = cpg.identifier.name("afterTryFromCatch")
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(
          List(
            ("doSomething(p)", Some(6)),
            ("tickOne = p", Some(11)),
            ("val afterTryFromCatch = tickOne", Some(13)),
            ("println(afterTryFromCatch)", Some(14))
          ),
          List(("doSomething(p)", Some(6)), ("tickOne = p", Some(11)), ("val afterTryFromCatch = tickOne", Some(13)))
        )
    }

    "should find a flow through body of `try` control structure" in {
      val source = cpg.method.name("doSomething").parameter
      val sink   = cpg.identifier.name("afterTryFromBody")
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(
          List(("doSomething(p)", Some(6)), ("tickTwo = p", Some(18)), ("val afterTryFromBody = tickTwo", Some(23))),
          List(
            ("doSomething(p)", Some(6)),
            ("tickTwo = p", Some(18)),
            ("val afterTryFromBody = tickTwo", Some(23)),
            ("println(afterTryFromBody)", Some(24))
          )
        )
    }
  }

  "CPG for code with `do-while` control structure" should {
    val cpg = code("""
        |fun doSomething(p: Int): Int {
        |  var someVal: Int
        |  do {
        |    someVal = p
        |  } while (false)
        |  val after = someVal
        |  return someVal
        |}
        |""".stripMargin)

    "should find a flow through body of the control structure" in {
      val source = cpg.method.name("doSomething").parameter
      val sink   = cpg.identifier.name("after")
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(List(("doSomething(p)", Some(2)), ("someVal = p", Some(5)), ("val after = someVal", Some(7))))
    }
  }

  "CPG for code with `while` control structure" should {
    val cpg = code("""
        |fun doSomething(p: Int): Int {
        |  var someVal: Int
        |  while (someVal == 0) {
        |    someVal = p
        |  }
        |  val after = someVal
        |  return someVal
        |}
        |""".stripMargin)

    "should find a flow through body of the control structure" in {
      val source = cpg.method.name("doSomething").parameter
      val sink   = cpg.identifier.name("after")
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(
          List(
            ("doSomething(p)", Some(4)),
            ("someVal = p", Some(7)),
            ("someVal == 0", Some(6)),
            ("val after = someVal", Some(9))
          )
        )
    }
  }

  "CPG for code with string interpolation" should {
    val cpg = code("""
        |package mypkg
        |
        |fun doSomething(p1: Int, p2: String): String {
        |    val interpolated = "BEGIN - $p1 - $p2 END"
        |    return "NOTHING"
        |}
        |""".stripMargin)

    "should find a flow through the interpolated values" in {
      def sink = cpg.identifier.name("interpolated")

      val sourceP1 = cpg.method.name("doSomething").parameter.order(1)
      val flows1   = sink.reachableByFlows(sourceP1)
      flows1.map(flowToResultPairs).toSet shouldBe
        Set(
          List(
            ("doSomething(p1, p2)", Some(4)),
            ("p1", Some(5)),
            ("\"BEGIN - $p1 - $p2 END\"", Some(5)),
            ("val interpolated = \"BEGIN - $p1 - $p2 END\"", Some(5))
          )
        )

      val sourceP2 = cpg.method.name("doSomething").parameter.order(2)
      val flows2   = sink.reachableByFlows(sourceP2)
      flows2.map(flowToResultPairs).toSet shouldBe
        Set(
          List(
            ("doSomething(p1, p2)", Some(4)),
            ("p2", Some(5)),
            ("p1", Some(5)),
            ("\"BEGIN - $p1 - $p2 END\"", Some(5)),
            ("val interpolated = \"BEGIN - $p1 - $p2 END\"", Some(5))
          )
        )
    }
  }

  "CPG for code with call to array index access" should {
    val cpg = code("""
        |package mypkg
        |
        |fun doSomething(p: String): String {
        |    val aList = listOf(p, "two", "three")
        |    val outOfList = aList[0]
        |    return aList
        |}
        |""".stripMargin)

    "should find a flow through index access operator" in {
      val source = cpg.method.name("doSomething").parameter
      val sink   = cpg.identifier.code("outOfList")
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(
          List(
            ("doSomething(p)", Some(4)),
            ("listOf(p, \"two\", \"three\")", Some(5)),
            ("val aList = listOf(p, \"two\", \"three\")", Some(5)),
            ("val outOfList = aList[0]", Some(6))
          )
        )
    }
  }

  "CPG for code with call to map index access" should {
    val cpg = code("""
        |package mypkg
        |
        |fun doSomething(p: String): String? {
        |    val aMap = mapOf("one" to p, "two" to "q")
        |    val outOfMap = aMap["one"]
        |    return outOfMap
        |}
        |""".stripMargin)

    "should find a flow through index access operator" in {
      val source = cpg.method.name("doSomething").parameter
      val sink   = cpg.identifier.code("outOfMap")
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(
          List(
            ("doSomething(p)", Some(4)),
            ("\"one\" to p", Some(5)),
            ("mapOf(\"one\" to p, \"two\" to \"q\")", Some(5)),
            ("val aMap = mapOf(\"one\" to p, \"two\" to \"q\")", Some(5)),
            ("val outOfMap = aMap[\"one\"]", Some(6))
          )
        )
    }
  }

  "CPG for code with member assignment" should {
    val cpg = code("""
        |package mypkg
        |
        |class AClass {
        |    var x: String = "INITIAL"
        |}
        |
        |fun doSomething(p1: String): String {
        |    val aClass = AClass()
        |    aClass.x = p1
        |    val aVal = aClass.x
        |    return "NOTHING"
        |}
        |""".stripMargin)

    "should find a flow through the assignment" in {
      val source = cpg.method.name("doSomething").parameter
      val sink   = cpg.identifier.code("aVal")
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(List(("doSomething(p1)", Some(8)), ("aClass.x = p1", Some(10)), ("val aVal = aClass.x", Some(11))))
    }
  }

  "CPG for code with user-defined class containing one member defined inline" should {
    val cpg = code("""
        |package mypkg
        |
        |class AClass {
        |    var x: String = "INITIAL"
        |}
        |
        |fun doSomething(p1: String): String {
        |    val aClass = AClass()
        |    aClass.x = p1
        |    val aVal = aClass.x
        |    return "NOTHING"
        |}
        |""".stripMargin)

    "should find a flow through an assignment call of its member" in {
      val source = cpg.method.name("doSomething").parameter
      val sink   = cpg.identifier.code("aVal")
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(List(("doSomething(p1)", Some(8)), ("aClass.x = p1", Some(10)), ("val aVal = aClass.x", Some(11))))
    }
  }

  "CPG for code with user-defined class containing one member defined inside its ctor" should {
    val cpg = code("""
        |package mypkg
        |
        |class AClass(var x: String)
        |
        |fun doSomething(p1: String): String {
        |    val aClass = AClass()
        |    aClass.x = p1
        |    val aVal = aClass.x
        |    return "NOTHING"
        |}
        |""".stripMargin)

    "should find a flow through an assignment call of its member" in {
      val source = cpg.method.name("doSomething").parameter
      val sink   = cpg.identifier.code("aVal")
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(List(("doSomething(p1)", Some(6)), ("aClass.x = p1", Some(8)), ("val aVal = aClass.x", Some(9))))
    }
  }
}
