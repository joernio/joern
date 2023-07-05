package io.joern.joerncli

import better.files.File
import io.joern.dataflowengineoss.slicing.{ObservedCallWithArgPos, _}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.{Languages, Operators}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class JoernSliceJS1 extends AnyWordSpec with Matchers with AbstractJoernCliTest {

  "extracting a usage slice from JavaScript code" should withTestCpg(
    File(getClass.getClassLoader.getResource("testcode/jssrc-slice-1")),
    Languages.JSSRC
  ) { case (cpg: Cpg, _) =>
    val programSlice =
      UsageSlicing
        .calculateUsageSlice(cpg, UsagesConfig(excludeOperatorCalls = true))
        .asInstanceOf[ProgramUsageSlice]

    "extract 'express.js' slice" in {
      val Some(slice) = programSlice.objectSlices.get("main.js::program").flatMap(_.slices.headOption): @unchecked
      slice.definedBy shouldBe Some(CallDef("express", "ANY", Option("express"), Option(2), Option(12)))
      slice.targetObj shouldBe LocalDef("app", "ANY")

      val List(inv1, inv2) = slice.invokedCalls
      inv1.callName shouldBe "get"
      inv1.paramTypes shouldBe List("__ecma.String", "LAMBDA")
      inv1.returnType shouldBe "ANY"

      inv2.callName shouldBe "listen"
      inv2.paramTypes shouldBe List("__ecma.Number", "LAMBDA")
      inv2.returnType shouldBe "ANY"

      val List(arg1, arg2) = slice.argToCalls

      arg1.position shouldBe Right(1)
      arg1.callName shouldBe "log"
      arg1.paramTypes shouldBe List("ANY")
      arg1.returnType shouldBe "ANY"

      arg2.position shouldBe Right(1)
      arg2.callName shouldBe "debug"
      arg2.paramTypes shouldBe List("ANY")
      arg2.returnType shouldBe "ANY"
    }

    "extract 'Car' UDTs" in {
      val Some(carUdt) = programSlice.userDefinedTypes.headOption: @unchecked
      carUdt.name shouldBe "main.js::program:Car"
      val Some(carInit) = carUdt.procedures.headOption: @unchecked
      carInit.callName shouldBe "<init>"
      carInit.returnType shouldBe "main.js::program:Car:<init>"
    }

    "extract 'Car' object instantiation" in {
      val Some(slice) =
        programSlice.objectSlices.get("main.js::program:carTest").flatMap(_.slices.headOption): @unchecked
      slice.definedBy shouldBe Some(
        CallDef("new Car", "main.js::program:Car", Option("main.js::program:Car"), Some(32), Some(14))
      )
      slice.targetObj shouldBe LocalDef("c", "main.js::program:Car")

      val List(inv1) = slice.invokedCalls
      inv1.callName shouldBe "rev"
      inv1.paramTypes shouldBe List.empty
      inv1.returnType shouldBe "ANY"

      val List(arg1: ObservedCallWithArgPos) = slice.argToCalls

      arg1.position shouldBe Right(1)
      arg1.callName shouldBe "Car"
      arg1.paramTypes shouldBe List("__ecma.String", "__ecma.Number")
      arg1.returnType shouldBe "main.js::program:Car"
    }
  }
}
class JoernSliceJS2 extends AnyWordSpec with Matchers with AbstractJoernCliTest {
  "extracting an interprocedural usage slice from JavaScript code 2" should withTestCpg(
    File(getClass.getClassLoader.getResource("testcode/jssrc-slice-2")),
    Languages.JSSRC
  ) { case (cpg: Cpg, _) =>
    val programSlice =
      UsageSlicing
        .calculateUsageSlice(cpg, UsagesConfig(excludeOperatorCalls = true))
        .asInstanceOf[ProgramUsageSlice]

    "extract 'y' local variable" in {
      val Some(slice) = programSlice.objectSlices.get("main.js::program:bar").flatMap(_.slices.headOption): @unchecked
      slice.targetObj shouldBe ParamDef("y", "ANY", 1, Some(14), Some(13))
      slice.definedBy shouldBe Option(ParamDef("y", "ANY", 1, Some(14), Some(13)))

      val List(inv1) = slice.invokedCalls

      inv1.callName shouldBe "getA"
      inv1.resolvedMethod shouldBe Some("main.js::program:Foo:getA")
      inv1.paramTypes shouldBe List.empty
      inv1.returnType shouldBe "ANY"
    }

    "extract 'x' local variable" in {
      val Some(slice) = programSlice.objectSlices.get("main.js::program").flatMap(_.slices.headOption): @unchecked
      slice.targetObj shouldBe LocalDef("x", "main.js::program:Foo")
      slice.definedBy shouldBe Option(
        CallDef("new Foo", "main.js::program:Foo", Some("main.js::program:Foo"), Some(17), Some(10))
      )

      val List(arg1, arg2) = slice.argToCalls

      arg1 shouldBe ObservedCallWithArgPos(
        "Foo",
        Some("main.js::program:Foo"),
        List("__ecma.Number", "__ecma.Number"),
        "main.js::program:Foo",
        Right(1)
      )

      arg2 shouldBe ObservedCallWithArgPos(
        "bar",
        Some("main.js::program:bar"),
        List("main.js::program:Foo"),
        "ANY",
        Right(1)
      )
    }
  }

}

class JoernSliceTS1 extends AnyWordSpec with Matchers with AbstractJoernCliTest {
  "extracting a usage slice from TypeScript code" should withTestCpg(
    File(getClass.getClassLoader.getResource("testcode/tssrc-slice")),
    Languages.JSSRC
  ) { case (cpg: Cpg, _) =>
    val programSlice =
      UsageSlicing
        .calculateUsageSlice(cpg, UsagesConfig())
        .asInstanceOf[ProgramUsageSlice]

    "extract 'name' parameter slice from 'startScene'" in {
      val Some(slice) =
        programSlice.objectSlices.get("main.ts::program:Game:startScene").flatMap(_.slices.headOption): @unchecked
      slice.definedBy shouldBe Some(ParamDef("name", "__ecma.String", 1, Some(56), Some(22)))
      slice.targetObj shouldBe ParamDef("name", "__ecma.String", 1, Some(56), Some(22))

      val List(_, _, arg1) = slice.argToCalls

      arg1.position shouldBe Right(2)
      arg1.callName shouldBe Operators.formatString
      arg1.paramTypes shouldBe List("__ecma.String", "__ecma.String", "__ecma.String")
      arg1.returnType shouldBe "ANY"
    }

    "extract 'loader' object slice from the main program" in {
      val Some(slice) = programSlice.objectSlices.get("main.ts::program").flatMap(_.slices.headOption): @unchecked
      slice.definedBy shouldBe Some(CallDef("new Loader", "Loader", Option("Loader"), Some(24), Some(21)))
      slice.targetObj shouldBe LocalDef("loader", "loader:Loader")

      val List(arg1) = slice.argToCalls

      arg1.position shouldBe Right(1)
      arg1.callName shouldBe "Loader"
      arg1.returnType shouldBe "Loader"
    }

    "extract 'time' parameter slice from the lambda in 'loop'" in {
      val Some(slice) =
        programSlice.objectSlices.get("main.ts::program:Game:loop:anonymous").flatMap(_.slices.headOption): @unchecked
      slice.definedBy shouldBe Some(ParamDef("time", "__ecma.Number", 1, Some(68), Some(31)))
      slice.targetObj shouldBe ParamDef("time", "__ecma.Number", 1, Some(68), Some(31))

      val List(arg1) = slice.argToCalls

      arg1.position shouldBe Right(1)
      arg1.callName shouldBe "loop"
      arg1.paramTypes shouldBe List("DOMHighResTimeStamp")
      arg1.returnType shouldBe "ANY"
    }
  }

}
