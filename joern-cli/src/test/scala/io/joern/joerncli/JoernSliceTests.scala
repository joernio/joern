package io.joern.joerncli

import better.files.File
import io.joern.dataflowengineoss.slicing._
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.{Languages, Operators}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class JoernSliceTests extends AnyWordSpec with Matchers with AbstractJoernCliTest {

  "extracting a usage slice from JavaScript code" should withTestCpg(
    File(getClass.getClassLoader.getResource("testcode/jssrc-slice")),
    Languages.JSSRC
  ) { case (cpg: Cpg, _) =>
    val programSlice =
      UsageSlicing
        .calculateUsageSlice(cpg, UsagesConfig(excludeOperatorCalls = true))
        .asInstanceOf[ProgramUsageSlice]

    "extract 'express.js' slice" in {
      val Some(slice) = programSlice.objectSlices.get("main.js::program").flatMap(_.headOption)
      slice.definedBy shouldBe Some(DefComponent("express", "ANY"))
      slice.targetObj shouldBe DefComponent("app", "ANY")

      val List(inv1, inv2) = slice.invokedCalls
      inv1.callName shouldBe "get"
      inv1.paramTypes shouldBe List("__ecma.String", "LAMBDA")
      inv1.returnType shouldBe "ANY"

      inv2.callName shouldBe "listen"
      inv2.paramTypes shouldBe List("__ecma.Number", "LAMBDA")
      inv2.returnType shouldBe "ANY"

      val List((arg1, pos1), (arg2, pos2)) = slice.argToCalls

      pos1 shouldBe 1
      arg1.callName shouldBe "log"
      arg1.paramTypes shouldBe List("ANY")
      arg1.returnType shouldBe "ANY"

      pos2 shouldBe 1
      arg2.callName shouldBe "debug"
      arg2.paramTypes shouldBe List("ANY")
      arg2.returnType shouldBe "ANY"
    }

    "extract 'Car' UDTs" in {
      val Some(carUdt) = programSlice.userDefinedTypes.headOption
      carUdt.name shouldBe "main.js::program:Car"
      val Some(carInit) = carUdt.procedures.headOption
      carInit.callName shouldBe "<init>"
      carInit.returnType shouldBe "Car"
    }

    "extract 'Car' object instantiation" in {
      val Some(slice) = programSlice.objectSlices.get("main.js::program:carTest").flatMap(_.headOption)
      slice.definedBy shouldBe Some(DefComponent("new Car", "ANY"))
      slice.targetObj shouldBe DefComponent("c", "main.js::program:Car")

      val List(inv1) = slice.invokedCalls
      inv1.callName shouldBe "rev"
      inv1.paramTypes shouldBe List.empty
      inv1.returnType shouldBe "ANY"

      val List((arg1, pos1)) = slice.argToCalls

      pos1 shouldBe 1
      arg1.callName shouldBe "Car"
      arg1.paramTypes shouldBe List("__ecma.String", "__ecma.Number")
      arg1.returnType shouldBe "ANY"
    }
  }

  "extracting a usage slice from TypeScript code" should withTestCpg(
    File(getClass.getClassLoader.getResource("testcode/tssrc-slice")),
    Languages.JSSRC
  ) { case (cpg: Cpg, _) =>
    val programSlice =
      UsageSlicing
        .calculateUsageSlice(cpg, UsagesConfig())
        .asInstanceOf[ProgramUsageSlice]

    "extract 'name' parameter slice from 'startScene'" in {
      val Some(slice) = programSlice.objectSlices.get("main.ts::program:Game:startScene").flatMap(_.headOption)
      slice.definedBy shouldBe Some(DefComponent("name", "__ecma.String"))
      slice.targetObj shouldBe DefComponent("name", "__ecma.String")

      val List(_, _, (arg1, pos1)) = slice.argToCalls

      pos1 shouldBe 2
      arg1.callName shouldBe Operators.formatString
      arg1.paramTypes shouldBe List("__ecma.String", "__ecma.String", "__ecma.String")
      arg1.returnType shouldBe "ANY"
    }

    "extract 'loader' object slice from the main program" in {
      val Some(slice) = programSlice.objectSlices.get("main.ts::program").flatMap(_.headOption)
      slice.definedBy shouldBe Some(DefComponent("new Loader", "ANY"))
      slice.targetObj shouldBe DefComponent("loader", "ANY")

      val List((arg1, pos1)) = slice.argToCalls

      pos1 shouldBe 1
      arg1.callName shouldBe "Loader"
      arg1.returnType shouldBe "ANY"
    }

    "extract 'time' parameter slice from the lambda in 'loop'" in {
      val Some(slice) = programSlice.objectSlices.get("main.ts::program:Game:loop:anonymous").flatMap(_.headOption)
      slice.definedBy shouldBe Some(DefComponent("time", "__ecma.Number"))
      slice.targetObj shouldBe DefComponent("time", "__ecma.Number")

      val List((arg1, pos1)) = slice.argToCalls

      pos1 shouldBe 1
      arg1.callName shouldBe "loop"
      arg1.paramTypes shouldBe List("__ecma.Number")
      arg1.returnType shouldBe "ANY"
    }
  }

}
