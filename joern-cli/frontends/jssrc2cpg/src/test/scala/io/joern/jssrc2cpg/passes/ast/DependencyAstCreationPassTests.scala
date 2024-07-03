package io.joern.jssrc2cpg.passes.ast

import io.joern.jssrc2cpg.testfixtures.AstJsSrc2CpgSuite
import io.joern.x2cpg.layers.Base
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.semanticcpg.language.*

class DependencyAstCreationPassTests extends AstJsSrc2CpgSuite {

  "AST generation for global builtins" should {
    "have correct structure for JSON.parse" in {
      val cpg               = code("""JSON.parse("foo");""")
      val List(program)     = cpg.method.nameExact(":program").l
      val List(methodBlock) = program.astChildren.isBlock.l
      val List(parseCall)   = methodBlock.astChildren.isCall.l
      parseCall.name shouldBe "parse"
      parseCall.methodFullName shouldBe "JSON.parse"
      parseCall.code shouldBe """JSON.parse("foo")"""
      parseCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

      val List(argument) = parseCall.astChildren.isLiteral.codeExact(""""foo"""").l
      argument.order shouldBe 1
      argument.argumentIndex shouldBe 1
    }

    "have correct structure for JSON.stringify" in {
      val cpg               = code("""JSON.stringify(foo);""")
      val List(program)     = cpg.method.nameExact(":program").l
      val List(methodBlock) = program.astChildren.isBlock.l
      val List(parseCall)   = methodBlock.astChildren.isCall.l
      parseCall.name shouldBe "stringify"
      parseCall.methodFullName shouldBe "JSON.stringify"
      parseCall.code shouldBe """JSON.stringify(foo)"""
      parseCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

      val List(argument) = parseCall.astChildren.isIdentifier.nameExact("foo").l
      argument.code shouldBe "foo"
      argument.order shouldBe 1
      argument.argumentIndex shouldBe 1
    }

    "not create static builtin call for calls not exactly matching dictionary" in {
      val cpg               = code("""JSON.parse.apply("foo");""")
      val List(program)     = cpg.method.nameExact(":program").l
      val List(methodBlock) = program.astChildren.isBlock.l
      val List(parseCall)   = methodBlock.astChildren.isCall.l
      parseCall.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
    }
  }

  "AST generation for dependencies" should {
    "have no dependencies if none are declared at all" in {
      val cpg = code("var x = 1;")
      cpg.dependency.l.size shouldBe 0
    }

    "have correct dependencies (imports)" in {
      val cpg = code("""
        |import {a} from "depA";
        |import {b} from "depB";
        |""".stripMargin)
      val List(depA, depB) = cpg.dependency.l

      depA.name shouldBe "a"
      depA.version shouldBe "import"
      depA.dependencyGroupId shouldBe Option("depA")

      depB.name shouldBe "b"
      depB.version shouldBe "import"
      depB.dependencyGroupId shouldBe Option("depB")
    }

    "have correct locals and require calls for imports" in {
      val cpg             = code("import path = require('path')")
      val List(localPath) = cpg.local.l
      localPath.name shouldBe "path"
      localPath.referencingIdentifiers.head.name shouldBe "path"

      val List(reqCall) = cpg.call.codeExact("require(\"path\")").l
      reqCall.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
      reqCall.receiver.head.code shouldBe "require"
      reqCall.argument.argumentIndex(1).head.code shouldBe "\"path\""
    }

    "have correct import nodes" in {
      val cpg = code("""
        |import {a} from "depA";
        |import {b} from "depB";
        |import {c} from "";
        |import * as d from "depD";
        |""".stripMargin)
      Base.passes(cpg).foreach(_.createAndApply())
      val List(a, b, c, d) = cpg.imports.l
      a.code shouldBe "import {a} from \"depA\""
      a.importedEntity shouldBe Option("depA:a")
      a.importedAs shouldBe Option("a")
      b.code shouldBe "import {b} from \"depB\""
      b.importedEntity shouldBe Option("depB:b")
      b.importedAs shouldBe Option("b")
      c.code shouldBe "import {c} from \"\""
      c.importedEntity shouldBe Option(":c")
      c.importedAs shouldBe Option("c")
      d.code shouldBe "import * as d from \"depD\""
      d.importedEntity shouldBe Option("depD:d")
      d.importedAs shouldBe Option("d")
      val List(n) = a.namespaceBlock.l
      n.fullName shouldBe "Test0.js:<global>"
    }

    "have correct dependencies (require)" in {
      val cpg = code("""
        |const a = require("depA");
        |const b = require("depB");
        |""".stripMargin)
      val List(depA, depB) = cpg.dependency.l

      depA.name shouldBe "a"
      depA.dependencyGroupId shouldBe Option("depA")
      depA.version shouldBe "require"
      depB.name shouldBe "b"
      depB.dependencyGroupId shouldBe Option("depB")
      depB.version shouldBe "require"
    }

    "have correct dependencies (strange requires)" in {
      val cpg = code("""
       |var _ = require("depA");
       |var b = require("depB").some.strange().call().here;
       |var { c } = require('depC');
       |var { d, e } = require('depD');
       |var [ f, g ] = require('depE');
       |""".stripMargin)
      val List(depA, depB, depC, depD, depE, depF, depG) = cpg.dependency.l

      depA.name shouldBe "_"
      depA.dependencyGroupId shouldBe Option("depA")
      depA.version shouldBe "require"

      depB.name shouldBe "b"
      depB.dependencyGroupId shouldBe Option("depB")
      depB.version shouldBe "require"

      depC.name shouldBe "c"
      depC.dependencyGroupId shouldBe Option("depC")
      depC.version shouldBe "require"

      depD.name shouldBe "d"
      depD.dependencyGroupId shouldBe Option("depD")
      depD.version shouldBe "require"

      depE.name shouldBe "e"
      depE.dependencyGroupId shouldBe Option("depD")
      depE.version shouldBe "require"

      depF.name shouldBe "f"
      depF.dependencyGroupId shouldBe Option("depE")
      depF.version shouldBe "require"

      depG.name shouldBe "g"
      depG.dependencyGroupId shouldBe Option("depE")
      depG.version shouldBe "require"
    }

    "have correct dependencies (mixed)" in {
      val cpg = code("""
        |import {a} from "depA";
        |const b = require("depB");
        |""".stripMargin)
      val List(depA, depB) = cpg.dependency.l

      depA.name shouldBe "a"
      depA.dependencyGroupId shouldBe Option("depA")
      depA.version shouldBe "import"

      depB.name shouldBe "b"
      depB.dependencyGroupId shouldBe Option("depB")
      depB.version shouldBe "require"
    }

    "have correct dependencies (different variations of import)" in {
      val cpg = code("""
       |import name from "module-name";
       |import * as otherName from "module-name";
       |import { member1 } from "module-name";
       |import { member2 as alias1 } from "module-name";
       |import { member3 , member4 } from "module-name";
       |import { member5 , member6 as alias2 } from "module-name";
       |import defaultMember1, * as alias3 from "module-name";
       |import defaultMember2 from "module-name";
       |import "module-name";
       |""".stripMargin)
      cpg.local.code.l shouldBe List(
        "name",
        "otherName",
        "member1",
        "alias1",
        "member3",
        "member4",
        "member5",
        "alias2",
        "defaultMember1",
        "alias3",
        "defaultMember2",
        "module-name"
      )
      cpg.assignment.code.l shouldBe List(
        "var name = require(\"module-name\")",
        "var otherName = require(\"module-name\")",
        "var member1 = require(\"module-name\").member1",
        "var alias1 = require(\"module-name\").member2",
        "var member3 = require(\"module-name\").member3",
        "var member4 = require(\"module-name\").member4",
        "var member5 = require(\"module-name\").member5",
        "var alias2 = require(\"module-name\").member6",
        "var defaultMember1 = require(\"module-name\")",
        "var alias3 = require(\"module-name\")",
        "var defaultMember2 = require(\"module-name\")",
        "var module-name = require(\"module-name\")"
      )

      val List(
        name,
        otherName,
        member1,
        alias1,
        member3,
        member4,
        member5,
        alias2,
        defaultMember1,
        alias3,
        defaultMember2,
        moduleName
      ) = cpg.dependency.l

      name.name shouldBe "name"
      name.dependencyGroupId shouldBe Option("module-name")
      name.version shouldBe "import"

      otherName.name shouldBe "otherName"
      otherName.dependencyGroupId shouldBe Option("module-name")
      otherName.version shouldBe "import"

      member1.name shouldBe "member1"
      member1.dependencyGroupId shouldBe Option("module-name")
      member1.version shouldBe "import"

      alias1.name shouldBe "alias1"
      alias1.dependencyGroupId shouldBe Option("module-name")
      alias1.version shouldBe "import"

      member3.name shouldBe "member3"
      member3.dependencyGroupId shouldBe Option("module-name")
      member3.version shouldBe "import"

      member4.name shouldBe "member4"
      member4.dependencyGroupId shouldBe Option("module-name")
      member4.version shouldBe "import"

      member5.name shouldBe "member5"
      member5.dependencyGroupId shouldBe Option("module-name")
      member5.version shouldBe "import"

      alias2.name shouldBe "alias2"
      alias2.dependencyGroupId shouldBe Option("module-name")
      alias2.version shouldBe "import"

      defaultMember1.name shouldBe "defaultMember1"
      defaultMember1.dependencyGroupId shouldBe Option("module-name")
      defaultMember1.version shouldBe "import"

      alias3.name shouldBe "alias3"
      alias3.dependencyGroupId shouldBe Option("module-name")
      alias3.version shouldBe "import"

      defaultMember2.name shouldBe "defaultMember2"
      defaultMember2.dependencyGroupId shouldBe Option("module-name")
      defaultMember2.version shouldBe "import"

      moduleName.name shouldBe "module-name"
      moduleName.dependencyGroupId shouldBe Option("module-name")
      moduleName.version shouldBe "import"
    }
  }

  "AST generation for exports" should {
    "have correct structure for simple names and aliases" in {
      val cpg = code("""
        |var name1, name2, name3, name6;
        |var variable4, variable5;
        |export { name1, name2, name3 };
        |export { variable4 as name4, variable5 as name5, name6 };
        |export let name7, name8, name9;
        |export let name10 = "10", name11 = "11", name12;
        |""".stripMargin)
      cpg.local.code.l.sorted shouldBe List(
        "name1",
        "name10",
        "name11",
        "name12",
        "name2",
        "name3",
        "name6",
        "name7",
        "name8",
        "name9",
        "variable4",
        "variable5"
      )
      cpg.assignment.code.l.sorted shouldBe List(
        "exports.name1 = name1",
        "exports.name10 = name10",
        "exports.name11 = name11",
        "exports.name12 = name12",
        "exports.name2 = name2",
        "exports.name3 = name3",
        "exports.name4 = variable4",
        "exports.name5 = variable5",
        "exports.name6 = name6",
        "exports.name7 = name7",
        "exports.name8 = name8",
        "exports.name9 = name9",
        "let name10 = \"10\"",
        "let name11 = \"11\""
      )
    }

    "have correct structure export assignments" in {
      val cpg = code("""
        |var foo = 1;
        |var bar = 2;
        |export = foo;
        |export = bar;
        |export = function func(param) {};
        |export = function () {}; // anonymous
        |export = class ClassA {};
        |""".stripMargin)
      cpg.local.code.l shouldBe List("foo", "bar", "func", "<lambda>0")
      cpg.typeDecl.name.l should contain allElementsOf List("func", "ClassA")
      cpg.assignment.code.l shouldBe List(
        "var foo = 1",
        "var bar = 2",
        "exports.foo = foo",
        "exports.bar = bar",
        "function func = function func(param) {}",
        "exports.func = func",
        "function <lambda>0 = function () {}",
        "exports.<lambda>0 = <lambda>0",
        "exports.ClassA = ClassA"
      )
    }

    "have correct structure for defaults" in {
      val cpg = code("""
        |var name1;
        |export { name1 as default };
        |export default name2 = "2";
        |export default function foo(param) {};
        |""".stripMargin)
      cpg.local.code.l shouldBe List("name1", "foo", "name2")
      cpg.assignment.code.l shouldBe List(
        "exports[\"default\"] = name1",
        "name2 = \"2\"",
        "exports[\"default\"] = name2",
        "function foo = function foo(param) {}",
        "exports[\"default\"] = foo"
      )
      cpg.method("foo").code.l shouldBe List("function foo(param) {}")
    }

    "have correct structure for export with from clause with path" in {
      val cpg = code("""
        |export { def as Header } from "./path/to/header";
        |""".stripMargin)
      val List(header) = cpg.dependency.l
      header.name shouldBe "Header"
      header.dependencyGroupId shouldBe Option("./path/to/header")
      header.version shouldBe "require"
      cpg.assignment.code.l shouldBe
        List("var _header = require(\"./path/to/header\")", "exports.Header = _header.def")
    }

    "have correct structure for export with from clause" in {
      val cpg = code("""
       |export { import1 as name1, import2 as name2, name3 } from "Foo";
       |export bar from "Bar";
       |""".stripMargin)
      val List(name1, name2, name3, bar) = cpg.dependency.l

      name1.name shouldBe "name1"
      name1.dependencyGroupId shouldBe Option("Foo")
      name1.version shouldBe "require"

      name2.name shouldBe "name2"
      name2.dependencyGroupId shouldBe Option("Foo")
      name2.version shouldBe "require"

      name3.name shouldBe "name3"
      name3.dependencyGroupId shouldBe Option("Foo")
      name3.version shouldBe "require"

      bar.name shouldBe "bar"
      bar.dependencyGroupId shouldBe Option("Bar")
      bar.version shouldBe "require"

      cpg.assignment.code.l shouldBe List(
        "var _Foo = require(\"Foo\")",
        "exports.name1 = _Foo.import1",
        "exports.name2 = _Foo.import2",
        "exports.name3 = _Foo.name3",
        "var _Bar = require(\"Bar\")",
        "exports.bar = _Bar.bar"
      )
    }

    "have correct structure for export all with from clause" in {
      val cpg = code("""
       |export * from "Foo";
       |export * as B from "Bar";
       |export * from "./some/ModuleA";
       |export * from './some/ModuleB';
       |""".stripMargin)
      val List(dep1, dep2, dep3, dep4) = cpg.dependency.l

      dep1.name shouldBe "Foo"
      dep1.dependencyGroupId shouldBe Option("Foo")
      dep1.version shouldBe "require"

      dep2.name shouldBe "B"
      dep2.dependencyGroupId shouldBe Option("Bar")
      dep2.version shouldBe "require"

      dep3.name shouldBe "ModuleA"
      dep3.dependencyGroupId shouldBe Option("./some/ModuleA")
      dep3.version shouldBe "require"

      dep4.name shouldBe "ModuleB"
      dep4.dependencyGroupId shouldBe Option("./some/ModuleB")
      dep4.version shouldBe "require"

      cpg.assignment.code.l shouldBe List(
        "var _Foo = require(\"Foo\")",
        "exports.Foo = _Foo",
        "var _Bar = require(\"Bar\")",
        "exports.B = _Bar",
        "var _ModuleA = require(\"./some/ModuleA\")",
        "exports.ModuleA = _ModuleA",
        "var _ModuleB = require(\"./some/ModuleB\")",
        "exports.ModuleB = _ModuleB"
      )

      val List(dep4ReqAssignment) = cpg.assignment.codeExact("var _ModuleB = require(\"./some/ModuleB\")").l
      dep4ReqAssignment.source.code shouldBe "require(\"./some/ModuleB\")"
      dep4ReqAssignment.target.code shouldBe "_ModuleB"
      val List(dep4ExpAssignment) = cpg.assignment.codeExact("exports.ModuleB = _ModuleB").l
      dep4ExpAssignment.source.code shouldBe "_ModuleB"
      dep4ExpAssignment.target.code shouldBe "exports.ModuleB"
    }

  }

}
