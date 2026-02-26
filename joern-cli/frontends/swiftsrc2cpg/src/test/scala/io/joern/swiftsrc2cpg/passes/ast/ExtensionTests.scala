package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.SwiftSrc2CpgSuite
import io.joern.x2cpg.frontendspecific.swiftsrc2cpg.Defines
import io.shiftleft.codepropertygraph.generated.EvaluationStrategies
import io.shiftleft.codepropertygraph.generated.nodes.Identifier
import io.shiftleft.semanticcpg.language.*

class ExtensionTests extends SwiftSrc2CpgSuite {

  "ExtensionTests" should {

    "create stable ASTs from multiple files" in {
      val fooCode =
        """
          |class Foo {
          |  var a = 1
          |  var b: String
          |  static var c = 2
          |  func someFunc() {}
          |}""".stripMargin

      val ext1Code =
        """
          |extension Foo {
          |  var d: Int { return 1 }
          |
          |  func someFooFunc() {}
          |}
          |""".stripMargin

      val ext2Code =
        """
          |extension Foo : Bar {
          |  var e: String { return "hello" }
          |
          |  func someOtherFooFunc() {}
          |}
          |""".stripMargin

      val cpg = code(fooCode, "Foo.swift").moreCode(ext1Code, "Ext1.swift").moreCode(ext2Code, "Ext2.swift")

      val List(fooTypeDecl) = cpg.typeDecl.fullNameExact("Foo.swift:<global>.Foo").l
      fooTypeDecl.name shouldBe "Foo"
      val List(fooConstructor) = fooTypeDecl.method.nameExact("init").isConstructor.l
      fooConstructor.fullName shouldBe s"Foo.swift:<global>.Foo.init:()->Foo.swift:<global>.Foo"
      fooConstructor.block.astChildren.assignment.code.l.sorted shouldBe List("var a = 1")
      val List(fooStaticInit) =
        fooTypeDecl.method.nameExact(io.joern.x2cpg.Defines.StaticInitMethodName).isConstructor.l
      fooStaticInit.fullName shouldBe s"Foo.swift:<global>.Foo.${io.joern.x2cpg.Defines.StaticInitMethodName}:()->Foo.swift:<global>.Foo"
      fooStaticInit.block.astChildren.assignment.code.l.sorted shouldBe List("var c = 2")

      val List(someFooFunc) = cpg.method.nameExact("someFooFunc").l
      someFooFunc.fullName shouldBe "Ext1.swift:<global>.Foo<extension>.someFooFunc:()->ANY"

      val List(someOtherFooFunc) = cpg.method.nameExact("someOtherFooFunc").l
      someOtherFooFunc.fullName shouldBe "Ext2.swift:<global>.Foo<extension>.someOtherFooFunc:()->ANY"

      val Seq(a, b, c, d, e) = fooTypeDecl.member.sortBy(_.name)
      a.name shouldBe "a"
      b.name shouldBe "b"
      c.name shouldBe "c"
      d.name shouldBe "d"
      e.name shouldBe "e"
      a.typeFullName shouldBe "ANY" // we don't have type inference without compiler support
      b.typeFullName shouldBe "Swift.String"
      c.typeFullName shouldBe "ANY" // we don't have type inference without compiler support
      d.typeFullName shouldBe "Swift.Int"
      e.typeFullName shouldBe "Swift.String"
    }

    "do not create illegal ref edges for multiple function annotations" in {
      val fooCode = "class Foo {}"

      val ext1Code =
        """
          |extension Foo {
          |  @objc @inline(__always) func handleCenterPanelPanLeft(_ gesture: UIScreenEdgePanGestureRecognizer) {
          |    handleCenterPanelPan(gesture)
          |  }
          |  @objc @inline(__always) func handleCenterPanelPanRight(_ gesture: UIScreenEdgePanGestureRecognizer) {
          |    handleCenterPanelPan(gesture)
          |  }
          |}
          |""".stripMargin
      val cpg = code(fooCode, "Foo.swift").moreCode(ext1Code, "Ext1.swift")
      cpg.identifier.nameExact("gesture").refOut.isParameter.method.fullName.l shouldBe List(
        "Ext1.swift:<global>.Foo<extension>.handleCenterPanelPanLeft:(_:UIScreenEdgePanGestureRecognizer)->ANY",
        "Ext1.swift:<global>.Foo<extension>.handleCenterPanelPanRight:(_:UIScreenEdgePanGestureRecognizer)->ANY"
      )
    }

    "do not create illegal ref edges for subscript functions" in {
      val fooCode = "class Foo {}"

      val ext1Code =
        """
          |extension Foo {
          |    public subscript(path: [JSONSubscriptType]) -> JSON {
          |        get {
          |            return path.reduce(self) { $0[sub: $1] }
          |        }
          |    }
          |
          |    public subscript(path: JSONSubscriptType...) -> JSON {
          |        get {
          |            return self[path]
          |        }
          |    }
          |}
          |""".stripMargin
      val cpg = code(fooCode, "Foo.swift").moreCode(ext1Code, "Ext1.swift")
      inside(cpg.call.nameExact("reduce").receiver.l) { case List(id: Identifier) =>
        id.name shouldBe "path"
        id.typeFullName shouldBe Defines.Array
        val local = id._localViaRefOut.get
        local.name shouldBe "path"
        local.typeFullName shouldBe Defines.Array
        local.closureBindingId.loneElement shouldBe "Ext1.swift:<global>.Foo.subscript:(path:Swift.Array)->JSON:get:ANY:path"
      }

      val List(subscriptMethod1, subscriptMethod2) = cpg.method.nameExact("subscript").l

      val List(subscriptMethod1Get)      = subscriptMethod1.ast.isMethod.nameExact("get").l
      val List(subscriptMethod1GetBlock) = subscriptMethod1Get.astChildren.isBlock.l
      val List(pathParam1)               = subscriptMethod1.parameter.nameExact("path").l
      val List(getRef1)                  = subscriptMethod1.ast.isMethodRef.code("get").l
      val List(closureBindingPath1)      = getRef1.captureOut.l
      closureBindingPath1.closureBindingId shouldBe Option(
        "Ext1.swift:<global>.Foo.subscript:(path:Swift.Array)->JSON:get:ANY:path"
      )
      closureBindingPath1.refOut.head shouldBe pathParam1
      closureBindingPath1.evaluationStrategy shouldBe EvaluationStrategies.BY_REFERENCE

      inside(cpg.call.codeExact("self[path]").argument(2).l) { case List(id: Identifier) =>
        id.name shouldBe "path"
        id.typeFullName shouldBe "JSONSubscriptType"
        val local = id._localViaRefOut.get
        local.name shouldBe "path"
        local.typeFullName shouldBe "JSONSubscriptType"
        local.closureBindingId.loneElement shouldBe "Ext1.swift:<global>.Foo.subscript:(path:JSONSubscriptType)->JSON:get:ANY:path"
      }
      val List(subscriptMethod2Get)      = subscriptMethod2.ast.isMethod.nameExact("get").l
      val List(subscriptMethod2GetBlock) = subscriptMethod2Get.astChildren.isBlock.l
      val List(pathParam2)               = subscriptMethod2.parameter.nameExact("path").l
      val List(getRef2)                  = subscriptMethod2.ast.isMethodRef.code("get").l
      val List(closureBindingPath2)      = getRef2.captureOut.l
      closureBindingPath2.closureBindingId shouldBe Option(
        "Ext1.swift:<global>.Foo.subscript:(path:JSONSubscriptType)->JSON:get:ANY:path"
      )
      closureBindingPath2.refOut.head shouldBe pathParam2
      closureBindingPath2.evaluationStrategy shouldBe EvaluationStrategies.BY_REFERENCE
    }

    "do not create illegal ref edges for global variables accessed in functions" in {
      val fooCode = "class Foo {}"

      val ext1Code =
        """
          |var vulnerabilities = ["Plist", "UserDefaults"]
          |
          |extension Foo {
          |    func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
          |        return vulnerabilities.count
          |    }
          |    func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
          |        let cell = self.tableView.dequeueReusableCell(withIdentifier: "vulnerabilitiesCell", for: indexPath)
          |        cell.textLabel?.text = vulnerabilities[indexPath.item]
          |        cell.accessoryType = .disclosureIndicator
          |        return cell
          |    }
          |}
          |""".stripMargin
      val cpg          = code(fooCode, "Foo.swift").moreCode(ext1Code, "Ext1.swift")
      val List(v1, v2) = cpg.method.nameExact("tableView").ast.isIdentifier.nameExact("vulnerabilities").l
      v1._localViaRefOut.closureBindingId.loneElement shouldBe
        "Ext1.swift:<global>.Foo<extension>.tableView:(_:UITableView,numberOfRowsInSection:Swift.Int)->Swift.Int:vulnerabilities"
      v2._localViaRefOut.closureBindingId.loneElement shouldBe
        "Ext1.swift:<global>.Foo<extension>.tableView:(_:UITableView,cellForRowAt:IndexPath)->UITableViewCell:vulnerabilities"
    }

  }

}
