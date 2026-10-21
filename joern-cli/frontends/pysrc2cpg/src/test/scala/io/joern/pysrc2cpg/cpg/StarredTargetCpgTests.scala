package io.joern.pysrc2cpg.cpg

import io.joern.pysrc2cpg.testfixtures.PySrc2CpgFixture
import io.joern.x2cpg.frontendspecific.pysrc2cpg.PythonOperators
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.codepropertygraph.generated.nodes.Identifier
import io.shiftleft.codepropertygraph.generated.nodes.Literal

class StarredTargetCpgTests extends PySrc2CpgFixture(withOssDataflow = false) {
  "starred target in assignment" should {
    "handle simple starred at end: x, *y = [1,2,3]" in {
      val cpg = code("""x, *y = [1,2,3]""".stripMargin)

      val sliceCall = cpg.call.methodFullName("<operator>.slice").head
      inside(sliceCall.argument.sortBy(_.argumentIndex).l) {
        case List(base: Identifier, lower: Literal, upper: Literal, step: Literal) =>
          base.code shouldBe "tmp0"
          lower.code shouldBe "1"
          upper.code shouldBe "None"
          step.code shouldBe "1"
      }
    }

    "handle starred in middle: x, *y, z = [1,2,3,4]" in {
      val cpg = code("""x, *y, z = [1,2,3,4]""".stripMargin)

      val sliceCall = cpg.call.methodFullName("<operator>.slice").head
      inside(sliceCall.argument.sortBy(_.argumentIndex).l) {
        case List(base: Identifier, lower: Literal, upper: Literal, step: Literal) =>
          base.code shouldBe "tmp0"
          lower.code shouldBe "1"
          upper.code shouldBe "-1"
          step.code shouldBe "1"
      }

      // Check x = tmp[0] and z = tmp[-1]
      val assignments = cpg.call.methodFullName(Operators.assignment).l
      val xAssign     = assignments.find(_.argument(1).code == "x").get
      inside(
        xAssign
          .argument(2)
          .asInstanceOf[Call]
          .argument
          .sortBy(_.argumentIndex)
          .l
      ) { case List(base: Identifier, index: Literal) =>
        base.code shouldBe "tmp0"
        index.code shouldBe "0"
      }
      val zAssign = assignments.find(_.argument(1).code == "z").get
      inside(
        zAssign
          .argument(2)
          .asInstanceOf[Call]
          .argument
          .sortBy(_.argumentIndex)
          .l
      ) { case List(base: Identifier, index: Literal) =>
        base.code shouldBe "tmp0"
        index.code shouldBe "-1"
      }
    }

    "handle multiple elements after starred: a, *b, c, d = [1,2,3,4,5]" in {
      val cpg = code("""a, *b, c, d = [1,2,3,4,5]""".stripMargin)

      val sliceCall = cpg.call.methodFullName("<operator>.slice").head
      inside(sliceCall.argument.sortBy(_.argumentIndex).l) {
        case List(base: Identifier, lower: Literal, upper: Literal, step: Literal) =>
          base.code shouldBe "tmp0"
          lower.code shouldBe "1"
          upper.code shouldBe "-2"
          step.code shouldBe "1"
      }

      // Check a = tmp[0], c = tmp[-2], d = tmp[-1]
      val assignments = cpg.call.methodFullName(Operators.assignment).l
      val aAssign     = assignments.find(_.argument(1).code == "a").get
      inside(
        aAssign
          .argument(2)
          .asInstanceOf[Call]
          .argument
          .sortBy(_.argumentIndex)
          .l
      ) { case List(base: Identifier, index: Literal) =>
        base.code shouldBe "tmp0"
        index.code shouldBe "0"
      }
      val cAssign = assignments.find(_.argument(1).code == "c").get
      inside(
        cAssign
          .argument(2)
          .asInstanceOf[Call]
          .argument
          .sortBy(_.argumentIndex)
          .l
      ) { case List(base: Identifier, index: Literal) =>
        base.code shouldBe "tmp0"
        index.code shouldBe "-2"
      }
      val dAssign = assignments.find(_.argument(1).code == "d").get
      inside(
        dAssign
          .argument(2)
          .asInstanceOf[Call]
          .argument
          .sortBy(_.argumentIndex)
          .l
      ) { case List(base: Identifier, index: Literal) =>
        base.code shouldBe "tmp0"
        index.code shouldBe "-1"
      }
    }

    "handle starred at beginning: *x, y, z = [1,2,3,4]" in {
      val cpg = code("""*x, y, z = [1,2,3,4]""".stripMargin)

      val sliceCall = cpg.call.methodFullName("<operator>.slice").head
      inside(sliceCall.argument.sortBy(_.argumentIndex).l) {
        case List(base: Identifier, lower: Literal, upper: Literal, step: Literal) =>
          base.code shouldBe "tmp0"
          lower.code shouldBe "0"
          upper.code shouldBe "-2"
          step.code shouldBe "1"
      }

      // Check y = tmp[-2], z = tmp[-1]
      val assignments = cpg.call.methodFullName(Operators.assignment).l
      val yAssign     = assignments.find(_.argument(1).code == "y").get
      inside(
        yAssign
          .argument(2)
          .asInstanceOf[Call]
          .argument
          .sortBy(_.argumentIndex)
          .l
      ) { case List(base: Identifier, index: Literal) =>
        base.code shouldBe "tmp0"
        index.code shouldBe "-2"
      }
      val zAssign = assignments.find(_.argument(1).code == "z").get
      inside(
        zAssign
          .argument(2)
          .asInstanceOf[Call]
          .argument
          .sortBy(_.argumentIndex)
          .l
      ) { case List(base: Identifier, index: Literal) =>
        base.code shouldBe "tmp0"
        index.code shouldBe "-1"
      }
    }
  }

  "starred target in for loop" should {
    "handle simple starred at end: for x, *y in items" in {
      val cpg = code("""
for x, *y in [[1,2,3]]:
    pass
""".stripMargin)

      val sliceCall = cpg.call.methodFullName("<operator>.slice").head
      inside(sliceCall.argument.sortBy(_.argumentIndex).l) {
        case List(base: Identifier, lower: Literal, upper: Literal, step: Literal) =>
          base.code shouldBe "tmp2"
          lower.code shouldBe "1"
          upper.code shouldBe "None"
          step.code shouldBe "1"
      }
    }

    "handle starred in middle: for x, *y, z in items" in {
      val cpg = code("""
for x, *y, z in [[1,2,3,4]]:
    pass
""".stripMargin)

      val sliceCall = cpg.call.methodFullName("<operator>.slice").head
      inside(sliceCall.argument.sortBy(_.argumentIndex).l) {
        case List(base: Identifier, lower: Literal, upper: Literal, step: Literal) =>
          base.code shouldBe "tmp2"
          lower.code shouldBe "1"
          upper.code shouldBe "-1"
          step.code shouldBe "1"
      }

      // Check x = tmp[0] and z = tmp[-1]
      val assignments = cpg.call.methodFullName(Operators.assignment).l
      val xAssign     = assignments.find(_.argument(1).code == "x").get
      inside(
        xAssign
          .argument(2)
          .asInstanceOf[Call]
          .argument
          .sortBy(_.argumentIndex)
          .l
      ) { case List(base: Identifier, index: Literal) =>
        base.code shouldBe "tmp2"
        index.code shouldBe "0"
      }
      val zAssign = assignments.find(_.argument(1).code == "z").get
      inside(
        zAssign
          .argument(2)
          .asInstanceOf[Call]
          .argument
          .sortBy(_.argumentIndex)
          .l
      ) { case List(base: Identifier, index: Literal) =>
        base.code shouldBe "tmp2"
        index.code shouldBe "-1"
      }
    }
  }

  "starred target in list comprehension" should {
    "handle simple starred: [x for [*x, y] in items]" in {
      val cpg = code("""result = [a for [*a, b] in [[1,2,3]]]""".stripMargin)

      val sliceCall = cpg.call.methodFullName("<operator>.slice").head
      inside(sliceCall.argument.sortBy(_.argumentIndex).l) {
        case List(base: Identifier, lower: Literal, upper: Literal, step: Literal) =>
          base.code shouldBe "tmp3"
          lower.code shouldBe "0"
          upper.code shouldBe "-1"
          step.code shouldBe "1"
      }
    }

    "handle starred in tuple: [x for (a, *b, c) in items]" in {
      val cpg = code("""result = [a for (a, *b, c) in [[1,2,3,4]]]""".stripMargin)

      val sliceCall = cpg.call.methodFullName("<operator>.slice").head
      inside(sliceCall.argument.sortBy(_.argumentIndex).l) {
        case List(base: Identifier, lower: Literal, upper: Literal, step: Literal) =>
          base.code shouldBe "tmp3"
          lower.code shouldBe "1"
          upper.code shouldBe "-1"
          step.code shouldBe "1"
      }

      // Check a = tmp[0] and c = tmp[-1]
      val assignments = cpg.call.methodFullName(Operators.assignment).l
      val aAssign     = assignments.find(_.argument(1).code == "a").get
      inside(
        aAssign
          .argument(2)
          .asInstanceOf[Call]
          .argument
          .sortBy(_.argumentIndex)
          .l
      ) { case List(base: Identifier, index: Literal) =>
        base.code shouldBe "tmp3"
        index.code shouldBe "0"
      }
      val cAssign = assignments.find(_.argument(1).code == "c").get
      inside(
        cAssign
          .argument(2)
          .asInstanceOf[Call]
          .argument
          .sortBy(_.argumentIndex)
          .l
      ) { case List(base: Identifier, index: Literal) =>
        base.code shouldBe "tmp3"
        index.code shouldBe "-1"
      }
    }
  }
}
