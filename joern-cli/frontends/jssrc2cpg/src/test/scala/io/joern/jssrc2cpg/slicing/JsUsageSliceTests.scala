package io.joern.jssrc2cpg.slicing

import io.joern.dataflowengineoss.slicing.*
import io.joern.jssrc2cpg.testfixtures.DataFlowCodeToCpgSuite

class JsUsageSliceTests extends DataFlowCodeToCpgSuite {

  private val config = UsagesConfig(excludeOperatorCalls = true).withParallelism(1)

  "extracting a usage slice from a JavaScript module" should {

    val cpg = code(
      """const express = require('express')
        |const app = express()
        |const port = 3000
        |
        |app.get('/', (req, res) => {
        |    res.send('Hello World!')
        |})
        |
        |app.listen(port, () => {
        |    console.log(`Example app listening on port ${port}`)
        |})
        |
        |console.log(app)
        |
        |function notHiddenByClosure() {
        |    console.debug(app)
        |}
        |
        |class Car {
        |    constructor(name, year) {
        |        this.name = name;
        |        this.year = year;
        |    }
        |
        |    rev() {
        |        return "vroom";
        |    }
        |
        |}
        |
        |function carTest() {
        |    const c = new Car("Noodle", 2012);
        |    c.rev();
        |}
        |
        |""".stripMargin,
      "main.js"
    )

    val programSlice = UsageSlicing.calculateUsageSlice(cpg, config)

    "extract 'express.js' slice" in {
      val slice = programSlice.objectSlices.find(x => x.fullName == "main.js::program").flatMap(_.slices.headOption).get
      slice.definedBy shouldBe Some(CallDef("express", "ANY", Option("express"), Option(2), Option(12)))
      slice.targetObj shouldBe LocalDef("app", "express:<returnValue>", Some(2), Some(6))

      val inv1 = slice.invokedCalls.find(_.callName == "get").get
      val inv2 = slice.invokedCalls.find(_.callName == "listen").get

      inv1.callName shouldBe "get"
      inv1.paramTypes shouldBe List("__ecma.String", "LAMBDA")
      inv1.returnType shouldBe "ANY"

      inv2.callName shouldBe "listen"
      inv2.paramTypes shouldBe List("__ecma.Number", "LAMBDA")
      inv2.returnType shouldBe "ANY"

      val arg1 = slice.argToCalls.find(_.callName == "log").get
      val arg2 = slice.argToCalls.find(_.callName == "debug").get

      arg1.position shouldBe Right(1)
      arg1.paramTypes shouldBe List("express:<returnValue>")
      arg1.returnType shouldBe "ANY"

      arg2.position shouldBe Right(1)
      arg2.paramTypes shouldBe List("express:<returnValue>")
      arg2.returnType shouldBe "ANY"
    }

    "extract 'Car' UDTs" in {
      val carUdt = programSlice.userDefinedTypes.head
      carUdt.name shouldBe "main.js::program:Car"
      val carInit = carUdt.procedures.head
      carInit.callName shouldBe "<init>"
      carInit.returnType shouldBe "ANY"
    }

    "extract 'Car' object instantiation" in {
      val slice =
        programSlice.objectSlices.find(x => x.fullName == "main.js::program:carTest").flatMap(_.slices.headOption).get
      slice.definedBy shouldBe Some(
        CallDef("new Car", "main.js::program:Car", Option("main.js::program:Car"), Some(32), Some(14))
      )
      slice.targetObj shouldBe LocalDef("c", "main.js::program:Car", Some(32), Some(10))

      val inv1 = slice.invokedCalls.find(_.callName == "Car").get
      val inv2 = slice.invokedCalls.find(_.callName == "rev").get

      inv1.paramTypes shouldBe List("__ecma.String", "__ecma.Number")
      inv1.returnType shouldBe "main.js::program:Car"

      inv2.paramTypes shouldBe List.empty
      inv2.returnType shouldBe "ANY"
    }

  }

  "extracting a usage slice from object parameters" should {
    val cpg = code(
      """class Foo {
        |
        |    constructor(a, b) {
        |        this.a = a;
        |        this.b = b;
        |    }
        |
        |    getA() {
        |        return this.a;
        |    }
        |}
        |
        |
        |function bar(y) {
        |    y.getA();
        |}
        |const x = new Foo(1, 2)
        |
        |bar(x)
        |
        |""".stripMargin,
      "main.js"
    )

    val programSlice = UsageSlicing.calculateUsageSlice(cpg, config)

    "extract 'y' local variable" in {
      val slice =
        programSlice.objectSlices.find(x => x.fullName == "main.js::program:bar").flatMap(_.slices.headOption).get
      slice.targetObj shouldBe ParamDef("y", "ANY", 1, Some(14), Some(13))
      slice.definedBy shouldBe Option(ParamDef("y", "ANY", 1, Some(14), Some(13)))

      val inv1 = slice.invokedCalls.find(_.callName == "getA").get

      inv1.resolvedMethod shouldBe Some("main.js::program:Foo:getA")
      inv1.paramTypes shouldBe List.empty
      inv1.returnType shouldBe "ANY"
    }

    "extract 'x' local variable" in {
      val slice = programSlice.objectSlices.find(x => x.fullName == "main.js::program").flatMap(_.slices.headOption).get
      slice.targetObj shouldBe LocalDef("x", "main.js::program:Foo", Some(17), Some(6))
      slice.definedBy shouldBe Option(
        CallDef("new Foo", "main.js::program:Foo", Some("main.js::program:Foo"), Some(17), Some(10))
      )

      val inv1 = slice.invokedCalls.find(_.callName == "Foo").get

      inv1.callName shouldBe "Foo"
      inv1.resolvedMethod shouldBe Some("main.js::program:Foo")
      inv1.paramTypes shouldBe List("__ecma.Number", "__ecma.Number")
      inv1.returnType shouldBe "main.js::program:Foo"

      val arg1 = slice.argToCalls.find(_.callName == "bar").get

      arg1 shouldBe ObservedCallWithArgPos(
        "bar",
        Some("main.js::program:bar"),
        List("main.js::program:Foo"),
        "ANY",
        Right(1),
        Some(19),
        Some(0)
      )
    }
  }
}
