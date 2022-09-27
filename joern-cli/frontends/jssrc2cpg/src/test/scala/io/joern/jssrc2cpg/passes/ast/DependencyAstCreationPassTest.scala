package io.joern.jssrc2cpg.passes.ast

import io.joern.jssrc2cpg.passes.AbstractPassTest
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.semanticcpg.language._

class DependencyAstCreationPassTest extends AbstractPassTest {

  "AST generation for global builtins" should {
    "have correct structure for JSON.parse" in AstFixture("""JSON.parse("foo");""") { cpg =>
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

    "have correct structure for JSON.stringify" in AstFixture("""JSON.stringify(foo);""") { cpg =>
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

    "not create static builtin call for calls not exactly matching dictionary" in AstFixture(
      """JSON.parse.apply("foo");"""
    ) { cpg =>
      val List(program)     = cpg.method.nameExact(":program").l
      val List(methodBlock) = program.astChildren.isBlock.l
      val List(parseCall)   = methodBlock.astChildren.isCall.l
      parseCall.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
    }
  }

  "AST generation for dependencies" should {
    "have no dependencies if none are declared at all" in AstFixture("var x = 1;") { cpg =>
      cpg.dependency.l.size shouldBe 0
    }

    "have correct dependencies (imports)" in AstFixture("""
        |import {a} from "depA";
        |import {b} from "depB";
        |""".stripMargin) { cpg =>
      val List(depA, depB) = cpg.dependency.l

      depA.name shouldBe "a"
      depA.version shouldBe "import"
      depA.dependencyGroupId shouldBe Some("depA")

      depB.name shouldBe "b"
      depB.version shouldBe "import"
      depB.dependencyGroupId shouldBe Some("depB")
    }

    "have correct import nodes" in AstFixture("""
        |import {a} from "depA";
        |import {b} from "depB";
        |import {c} from "";
        |import * as d from "depD";
        |""".stripMargin) { cpg =>
      val List(a, b, c, d) = cpg.staticImport.l
      a.code shouldBe "import {a} from \"depA\""
      a.importedEntity shouldBe Some("depA")
      a.importedAs shouldBe Some("a")
      b.code shouldBe "import {b} from \"depB\""
      b.importedEntity shouldBe Some("depB")
      b.importedAs shouldBe Some("b")
      c.code shouldBe "import {c} from \"\""
      c.importedEntity should not be defined
      c.importedAs shouldBe Some("c")
      d.code shouldBe "import * as d from \"depD\""
      d.importedEntity shouldBe Some("depD")
      d.importedAs shouldBe Some("d")
      val List(n) = a.namespaceBlock.l
      n.fullName shouldBe "code.js:<global>"
    }

    "have correct dependencies (require)" in AstFixture("""
        |const a = require("depA");
        |const b = require("depB");
        |""".stripMargin) { cpg =>
      val List(depA, depB) = cpg.dependency.l

      depA.name shouldBe "a"
      depA.dependencyGroupId shouldBe Some("depA")
      depA.version shouldBe "require"
      depB.name shouldBe "b"
      depB.dependencyGroupId shouldBe Some("depB")
      depB.version shouldBe "require"
    }

    "have correct dependencies (strange requires)" in AstFixture("""
       |var _ = require("depA");
       |var b = require("depB").some.strange().call().here;
       |var { c } = require('depC');
       |var { d, e } = require('depD');
       |var [ f, g ] = require('depE');
       |""".stripMargin) { cpg =>
      val List(depA, depB, depC, depD, depE, depF, depG) = cpg.dependency.l

      depA.name shouldBe "_"
      depA.dependencyGroupId shouldBe Some("depA")
      depA.version shouldBe "require"

      depB.name shouldBe "b"
      depB.dependencyGroupId shouldBe Some("depB")
      depB.version shouldBe "require"

      depC.name shouldBe "c"
      depC.dependencyGroupId shouldBe Some("depC")
      depC.version shouldBe "require"

      depD.name shouldBe "d"
      depD.dependencyGroupId shouldBe Some("depD")
      depD.version shouldBe "require"

      depE.name shouldBe "e"
      depE.dependencyGroupId shouldBe Some("depD")
      depE.version shouldBe "require"

      depF.name shouldBe "f"
      depF.dependencyGroupId shouldBe Some("depE")
      depF.version shouldBe "require"

      depG.name shouldBe "g"
      depG.dependencyGroupId shouldBe Some("depE")
      depG.version shouldBe "require"
    }

    "have correct dependencies (mixed)" in AstFixture("""
        |import {a} from "depA";
        |const b = require("depB");
        |""".stripMargin) { cpg =>
      val List(depA, depB) = cpg.dependency.l

      depA.name shouldBe "a"
      depA.dependencyGroupId shouldBe Some("depA")
      depA.version shouldBe "import"

      depB.name shouldBe "b"
      depB.dependencyGroupId shouldBe Some("depB")
      depB.version shouldBe "require"
    }

    "have correct dependencies (different variations of import)" in AstFixture("""
       |import name from "module-name";
       |import * as otherName from "module-name";
       |import { member1 } from "module-name";
       |import { member2 as alias1 } from "module-name";
       |import { member3 , member4 } from "module-name";
       |import { member5 , member6 as alias2 } from "module-name";
       |import defaultMember1, * as alias3 from "module-name";
       |import defaultMember2 from "module-name";
       |import "module-name";
       |
       |function foo() {
       |  var alias1  = "";
       |  alias1.call();
       |}
       |function bar() {
       |  alias1.call();
       |  member1.call();
       |  name.call();
       |}
       |""".stripMargin) { cpg =>
      val aliasIdentInFoo = cpg.method("foo").ast.isIdentifier
      aliasIdentInFoo.dynamicTypeHintFullName shouldBe empty // its overridden in the scope of foo

      val List(alias1Ident, alias1RecIdent, member1Ident, member1RecIdent, nameIdent, nameRecIdent) =
        cpg.method("bar").ast.isIdentifier.name("alias1", "member1", "name").l
      alias1Ident.dynamicTypeHintFullName shouldBe List("^module-name^.member2")
      alias1RecIdent.dynamicTypeHintFullName shouldBe List("^module-name^.member2")
      member1Ident.dynamicTypeHintFullName shouldBe List("^module-name^.member1")
      member1RecIdent.dynamicTypeHintFullName shouldBe List("^module-name^.member1")
      nameIdent.dynamicTypeHintFullName shouldBe List("module-name")
      nameRecIdent.dynamicTypeHintFullName shouldBe List("module-name")

      val List(
        nameLocal,
        otherNameLocal,
        member1Local,
        alias1Local,
        member3Local,
        member4Local,
        member5Local,
        alias2Local,
        defaultMember1Local,
        alias3Local,
        defaultMember2Local,
        moduleNameLocal,
        _, // ignore local for method foo
        _  // ignore local for method bar
      ) = cpg.method.nameExact(":program").local.l

      nameLocal.code shouldBe "name"
      nameLocal.dynamicTypeHintFullName shouldBe Seq("module-name")
      otherNameLocal.code shouldBe "otherName"
      otherNameLocal.dynamicTypeHintFullName shouldBe Seq("module-name")
      member1Local.code shouldBe "member1"
      member1Local.dynamicTypeHintFullName shouldBe Seq("^module-name^.member1")
      alias1Local.code shouldBe "alias1"
      alias1Local.dynamicTypeHintFullName shouldBe Seq("^module-name^.member2")
      member3Local.code shouldBe "member3"
      member3Local.dynamicTypeHintFullName shouldBe Seq("^module-name^.member3")
      member4Local.code shouldBe "member4"
      member4Local.dynamicTypeHintFullName shouldBe Seq("^module-name^.member4")
      member5Local.code shouldBe "member5"
      member5Local.dynamicTypeHintFullName shouldBe Seq("^module-name^.member5")
      alias2Local.code shouldBe "alias2"
      alias2Local.dynamicTypeHintFullName shouldBe Seq("^module-name^.member6")
      defaultMember1Local.code shouldBe "defaultMember1"
      defaultMember1Local.dynamicTypeHintFullName shouldBe Seq("module-name")
      alias3Local.code shouldBe "alias3"
      alias3Local.dynamicTypeHintFullName shouldBe Seq("module-name")
      defaultMember2Local.code shouldBe "defaultMember2"
      defaultMember2Local.dynamicTypeHintFullName shouldBe Seq("module-name")
      moduleNameLocal.code shouldBe "module-name"
      moduleNameLocal.dynamicTypeHintFullName shouldBe Seq("module-name")

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
      name.dependencyGroupId shouldBe Some("module-name")
      name.version shouldBe "import"

      otherName.name shouldBe "otherName"
      otherName.dependencyGroupId shouldBe Some("module-name")
      otherName.version shouldBe "import"

      member1.name shouldBe "member1"
      member1.dependencyGroupId shouldBe Some("module-name")
      member1.version shouldBe "import"

      alias1.name shouldBe "alias1"
      alias1.dependencyGroupId shouldBe Some("module-name")
      alias1.version shouldBe "import"

      member3.name shouldBe "member3"
      member3.dependencyGroupId shouldBe Some("module-name")
      member3.version shouldBe "import"

      member4.name shouldBe "member4"
      member4.dependencyGroupId shouldBe Some("module-name")
      member4.version shouldBe "import"

      member5.name shouldBe "member5"
      member5.dependencyGroupId shouldBe Some("module-name")
      member5.version shouldBe "import"

      alias2.name shouldBe "alias2"
      alias2.dependencyGroupId shouldBe Some("module-name")
      alias2.version shouldBe "import"

      defaultMember1.name shouldBe "defaultMember1"
      defaultMember1.dependencyGroupId shouldBe Some("module-name")
      defaultMember1.version shouldBe "import"

      alias3.name shouldBe "alias3"
      alias3.dependencyGroupId shouldBe Some("module-name")
      alias3.version shouldBe "import"

      defaultMember2.name shouldBe "defaultMember2"
      defaultMember2.dependencyGroupId shouldBe Some("module-name")
      defaultMember2.version shouldBe "import"

      moduleName.name shouldBe "module-name"
      moduleName.dependencyGroupId shouldBe Some("module-name")
      moduleName.version shouldBe "import"
    }
  }

  "have correct dependencies (internal / external)" in AstFixture("""
      |import name1 from "./dep";
      |import name2 from "dep";
      |import { member1 } from "./dep";
      |import { member2 } from "dep";
      |import { member3 as alias1 } from "./dep";
      |import { member4 as alias2 } from "dep";
      |import { member5 as alias3 } from "./some/../path/dep"
      |""".stripMargin) { cpg =>
    val List(name1, name2, member1, member2, alias1, alias2, alias3) = cpg.local.l
    name1.dynamicTypeHintFullName shouldBe List("./dep")
    name2.dynamicTypeHintFullName shouldBe List("dep")
    member1.dynamicTypeHintFullName shouldBe List("./dep.member1")
    member2.dynamicTypeHintFullName shouldBe List("^dep^.member2")
    alias1.dynamicTypeHintFullName shouldBe List("./dep.member3")
    alias2.dynamicTypeHintFullName shouldBe List("^dep^.member4")
    alias3.dynamicTypeHintFullName shouldBe List("./path/dep.member5")
  }

  "AST generation for exports" should {
    "have correct structure for simple names and aliases" in AstFixture("""
        |var name1, name2, name3, name6;
        |var variable4, variable5;
        |export { name1, name2, name3 };
        |export { variable4 as name4, variable5 as name5, name6 };
        |export let name7, name8, name9;
        |export let name10 = "10", name11 = "11", name12;
        |""".stripMargin) { cpg =>
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

    "have correct structure export assignments" in AstFixture("""
        |var foo = 1;
        |var bar = 2;
        |export = foo;
        |export = bar;
        |export = function func(param) {};
        |export = class ClassA {};
        |""".stripMargin) { cpg =>
      cpg.local.code.l shouldBe List("foo", "bar", "func")
      cpg.typeDecl.name.l should contain allElementsOf List("func", "ClassA")
      cpg.assignment.code.l shouldBe List(
        "var foo = 1",
        "var bar = 2",
        "exports.foo = foo",
        "exports.bar = bar",
        "function func = function func(param) {}",
        "exports.func = func",
        "exports.ClassA = ClassA"
      )
    }

    "have correct structure for defaults" in AstFixture("""
        |var name1;
        |export { name1 as default };
        |export default name2 = "2";
        |export default function foo(param) {};
        |""".stripMargin) { cpg =>
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

    "have correct structure for export with from clause" in AstFixture("""
       |export { import1 as name1, import2 as name2, name3 } from "Foo";
       |export bar from "Bar";
       |""".stripMargin) { cpg =>
      val List(name1, name2, name3, bar) = cpg.dependency.l

      name1.name shouldBe "name1"
      name1.dependencyGroupId shouldBe Some("Foo")
      name1.version shouldBe "require"

      name2.name shouldBe "name2"
      name2.dependencyGroupId shouldBe Some("Foo")
      name2.version shouldBe "require"

      name3.name shouldBe "name3"
      name3.dependencyGroupId shouldBe Some("Foo")
      name3.version shouldBe "require"

      bar.name shouldBe "bar"
      bar.dependencyGroupId shouldBe Some("Bar")
      bar.version shouldBe "require"

      cpg.assignment.code.l shouldBe List(
        "var _Foo = require(\"Foo\")",
        "_Foo.name1 = import1",
        "_Foo.name2 = import2",
        "_Foo.name3 = name3",
        "var _Bar = require(\"Bar\")",
        "_Bar.bar = bar"
      )
    }

    "have correct structure for export all with from clause" in AstFixture("""
       |export * from "Foo";
       |export * as B from "Bar";
       |export * from "./some/Module";
       |""".stripMargin) { cpg =>
      val List(dep1, dep2, dep3) = cpg.dependency.l

      dep1.name shouldBe "Foo"
      dep1.dependencyGroupId shouldBe Some("Foo")
      dep1.version shouldBe "require"

      dep2.name shouldBe "B"
      dep2.dependencyGroupId shouldBe Some("Bar")
      dep2.version shouldBe "require"

      dep3.name shouldBe "Module"
      dep3.dependencyGroupId shouldBe Some("./some/Module")
      dep3.version shouldBe "require"

      cpg.assignment.code.l shouldBe List(
        "var _Foo = require(\"Foo\")",
        "exports.Foo = _Foo",
        "var _Bar = require(\"Bar\")",
        "exports.B = _Bar",
        "var _Module = require(\"./some/Module\")",
        "exports.Module = _Module"
      )
    }

  }

}
