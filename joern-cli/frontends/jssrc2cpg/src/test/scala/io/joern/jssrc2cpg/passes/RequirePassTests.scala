package io.joern.jssrc2cpg.passes

import io.joern.dataflowengineoss.language.*
import io.joern.jssrc2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.shiftleft.semanticcpg.language.*

import java.io.File

class RequirePassTests extends DataFlowCodeToCpgSuite {

  "methods imported via `require` should be resolved correctly" in {
    val cpg = code(
      """
         |const externalfunc = require('./sampleone');
         |function testone() {
         |  var name = "foo";
         |  console.log(name);
         |  externalfunc(name);
         |}
         |
         |testone();
      """.stripMargin,
      "sample.js"
    ).moreCode(
      """
        |module.exports = function (nameparam) {
        |  console.log( "external func" + nameparam);
        |}
        |""".stripMargin,
      "sampleone.js"
    )

    cpg.call("externalfunc").methodFullName.l shouldBe List("sampleone.js::program:<lambda>0")
    implicit val callResolver: NoResolve.type = NoResolve
    cpg.call("externalfunc").callee.fullName.l shouldBe List("sampleone.js::program:<lambda>0")

    val sink   = cpg.call("log").argument(1)
    val source = cpg.literal.codeExact("\"foo\"")
    sink.reachableByFlows(source).size shouldBe 2
  }

  "methods imported via `import` should be resolved correctly" in {
    val cpg = code(
      """
         |import {foo, bar} from './sampleone.mjs';
         |var x = "literal";
         |foo(x);
         |bar(x);
      """.stripMargin,
      "sample.js"
    ).moreCode(
      """
        |export function foo(x) {
        |  console.log(x);
        |}
        |
        |export function bar(x) {
        |  console.log(x);
        |}
        |""".stripMargin,
      "sampleone.mjs"
    )

    implicit val callResolver: NoResolve.type = NoResolve
    cpg.call("foo").methodFullName.l shouldBe List("sampleone.mjs::program:foo")
    cpg.call("foo").callee.fullName.l shouldBe List("sampleone.mjs::program:foo")
    cpg.call("bar").methodFullName.l shouldBe List("sampleone.mjs::program:bar")
    cpg.call("bar").callee.fullName.l shouldBe List("sampleone.mjs::program:bar")

    val sink   = cpg.call("log").argument(1)
    val source = cpg.literal.codeExact("\"literal\"")
    sink.reachableByFlows(source).size shouldBe 2
  }

  "methods imported in TypeScript via relative importing" in {
    lazy val cpg = code(
      """
        |export function foo() {}
        |""".stripMargin,
      "foo.ts"
    ).moreCode(
      """
        |import { foo } from "../../foo.ts";
        |
        |foo();
        |export function bar() {}
        |""".stripMargin,
      Seq("d1", "d2", "bar.ts").mkString(File.separator)
    ).moreCode(
      """
        |import { bar } from "./d2/bar.ts";
        |
        |bar();
        |""".stripMargin,
      Seq("d1", "baz.ts").mkString(File.separator)
    )

    cpg.call("bar").methodFullName.headOption shouldBe Option(
      Seq("d1", "d2", "bar.ts::program:bar").mkString(File.separator)
    )
    cpg.call("foo").methodFullName.headOption shouldBe Option("foo.ts::program:foo")
  }

}
