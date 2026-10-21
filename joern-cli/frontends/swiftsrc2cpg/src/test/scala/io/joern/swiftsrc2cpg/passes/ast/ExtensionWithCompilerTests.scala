package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.SwiftCompilerSrc2CpgSuite
import io.joern.x2cpg.frontendspecific.swiftsrc2cpg.Defines
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{Identifier, MethodParameterIn}
import io.shiftleft.semanticcpg.language.*

class ExtensionWithCompilerTests extends SwiftCompilerSrc2CpgSuite {

  "ExtensionWithCompilerTests" should {

    "be correct for static extension methods" in {
      val cpg = codeWithSwiftSetup("""
         |struct Factory {}
         |
         |extension Factory {
         |  static func id(x: Int) -> Int {
         |    return x
         |  }
         |}
         |func main(source: Int) {
         |  Factory.id(x: source)
         |}
         |""".stripMargin)
      val List(idCall) = cpg.call("id").l
      idCall.code shouldBe "Factory.id(x: source)"
      idCall.methodFullName shouldBe "SwiftTest.Factory<extension>.id:(x:Swift.Int)->Swift.Int"
      val List(idMethod) = cpg.method("id").l
      idMethod.fullName shouldBe "SwiftTest.Factory<extension>.id:(x:Swift.Int)->Swift.Int"
    }

    "be correct for mutating extension method" in {
      val cpg = codeWithSwiftSetup("""
          |struct Point {
          |  var x: Int
          |}
          |
          |extension Point {
          |  mutating func setX(v: Int) {
          |    self.x = v
          |  }
          |}
          |""".stripMargin)
      cpg.method.fullName.sorted shouldBe Seq(
        "<operator>.arrayInitializer",
        "<operator>.assignment",
        "<operator>.fieldAccess",
        "Package.swift:<global>",
        "Sources/main.swift:<global>",
        "SwiftTest.Point.init:()->SwiftTest.Point",
        "SwiftTest.Point<extension>.setX:(v:Swift.Int)->()"
      )
    }

    "be correct for property methods from extension methods" in {
      val cpg = codeWithSwiftSetup("""
          |class Foo {
          |  var a: Int {
          |    get {
          |      return 1
          |    }
          |    set(newValue) {}
          |  }
          |  var x: Int {
          |    return 42
          |  }
          |}
          |
          |extension Foo {
          |  var b: Int {
          |    get {
          |      return 2
          |    }
          |    set(newValue) {}
          |  }
          |  var y: Int {
          |    return 42
          |  }
          |}
          |func main() {
          |  let foo = Foo()
          |  print(foo.a)
          |  print(foo.b)
          |  print(foo.x)
          |  print(foo.y)
          |
          |  foo.a = 10
          |  foo.b = 20
          |}
          |""".stripMargin)
      cpg.method.fullName.sorted shouldBe Seq(
        "<operator>.alloc",
        "<operator>.arrayInitializer",
        "<operator>.assignment",
        "Package.swift:<global>",
        "Sources/main.swift:<global>",
        "Swift.print:(_:Any...,separator:Swift.String,terminator:Swift.String)->()",
        "SwiftTest.Foo.a.getter:Swift.Int",
        "SwiftTest.Foo.a.setter:Swift.Int",
        "SwiftTest.Foo.init:()->SwiftTest.Foo",
        "SwiftTest.Foo.x:Swift.Int",
        "SwiftTest.Foo<extension>.b.getter:Swift.Int",
        "SwiftTest.Foo<extension>.b.setter:Swift.Int",
        "SwiftTest.Foo<extension>.y:Swift.Int",
        "SwiftTest.main:()->()"
      )

      cpg.call.isAssignment.code.l should not contain allElementsOf(List("foo.a = 10", "foo.b = 20"))
      cpg.call.methodFullName.sorted should contain allElementsOf Seq(
        "SwiftTest.Foo.a.getter:Swift.Int",
        "SwiftTest.Foo<extension>.b.getter:Swift.Int",
        "SwiftTest.Foo.x:Swift.Int",
        "SwiftTest.Foo<extension>.y:Swift.Int",
        "SwiftTest.Foo.a.setter:Swift.Int",
        "SwiftTest.Foo<extension>.b.setter:Swift.Int"
      )

      val List(aGetter) = cpg.call.nameExact("a.getter").codeExact("foo.a").l
      aGetter.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
      aGetter.methodFullName shouldBe "SwiftTest.Foo.a.getter:Swift.Int"
      aGetter.receiver.argumentIndex.loneElement shouldBe 0
      aGetter.receiver.code.loneElement shouldBe "foo"

      val List(bGetter) = cpg.call.nameExact("b.getter").codeExact("foo.b").l
      bGetter.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      bGetter.methodFullName shouldBe "SwiftTest.Foo<extension>.b.getter:Swift.Int"
      bGetter.receiver shouldBe empty

      val List(xGetter) = cpg.call.nameExact("x").l
      xGetter.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
      xGetter.code shouldBe "foo.x"
      xGetter.methodFullName shouldBe "SwiftTest.Foo.x:Swift.Int"
      xGetter.receiver.argumentIndex.loneElement shouldBe 0
      xGetter.receiver.code.loneElement shouldBe "foo"

      val List(yGetter) = cpg.call.nameExact("y").l
      yGetter.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      yGetter.code shouldBe "foo.y"
      yGetter.methodFullName shouldBe "SwiftTest.Foo<extension>.y:Swift.Int"
      yGetter.receiver shouldBe empty

      cpg.typeDecl.nameExact("Foo").member.name.sorted shouldBe Seq("a", "b", "x", "y")

      val List(setACall) = cpg.call.nameExact("a.setter").codeExact("foo.a = 10").l
      setACall.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
      setACall.methodFullName shouldBe "SwiftTest.Foo.a.setter:Swift.Int"
      setACall.receiver.argumentIndex.loneElement shouldBe 0
      setACall.receiver.code.loneElement shouldBe "foo"
      setACall.argument(1).code shouldBe "10"

      val List(setBCall) = cpg.call.nameExact("b.setter").codeExact("foo.b = 20").l
      setBCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      setBCall.methodFullName shouldBe "SwiftTest.Foo<extension>.b.setter:Swift.Int"
      setBCall.receiver shouldBe empty
      setBCall.argument(1).code shouldBe "20"
    }

    "be correct for subscript functions without accessors" in {
      val fooCode =
        """
          |struct TimesTable {
          |  let multiplier: Int
          |  subscript(index: Int) -> Int {
          |    return multiplier * index
          |  }
          |}
          |
          |func main() {
          |  let table = TimesTable(multiplier: 3)
          |  print(table[6])
          |}
          |""".stripMargin
      val cpg = codeWithSwiftSetup(fooCode)

      val subscriptExt =
        cpg.method
          .fullNameExact("SwiftTest.TimesTable.subscript:(Swift.Int)->Swift.Int")
          .loneElement
      inside(subscriptExt.ast.isIdentifier.nameExact("index").l) { case List(id: Identifier) =>
        id.typeFullName shouldBe Defines.Int
        val param = id.refsTo.collectAll[MethodParameterIn].loneElement
        param.name shouldBe "index"
        param.typeFullName shouldBe Defines.Int
        param.method.fullName shouldBe subscriptExt.fullName
      }
      cpg.call.nameExact(Operators.indexAccess) shouldBe empty
      inside(cpg.call.codeExact("table[6]").l) { case List(subscriptCall) =>
        subscriptCall.methodFullName shouldBe "SwiftTest.TimesTable.subscript:(Swift.Int)->Swift.Int"
        subscriptCall.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
        subscriptCall.receiver.argumentIndex.loneElement shouldBe 0
        subscriptCall.receiver.code.loneElement shouldBe "table"
        subscriptCall.argument(1).code shouldBe "6"
      }
    }

    "be correct for subscript functions with getter" in {
      val fooCode =
        """
          |struct TimesTable {
          |  let multiplier: Int
          |  subscript(index: Int) -> Int {
          |    get {
          |      return multiplier * index
          |    }
          |  }
          |}
          |
          |func main() {
          |  let table = TimesTable(multiplier: 3)
          |  print(table[6])
          |}
          |""".stripMargin
      val cpg = codeWithSwiftSetup(fooCode)

      val subscriptExt =
        cpg.method
          .fullNameExact("SwiftTest.TimesTable.subscript.getter:(Swift.Int)->Swift.Int")
          .loneElement
      inside(subscriptExt.ast.isIdentifier.nameExact("index").l) { case List(id: Identifier) =>
        id.typeFullName shouldBe Defines.Int
        val param = id.refsTo.collectAll[MethodParameterIn].loneElement
        param.name shouldBe "index"
        param.typeFullName shouldBe Defines.Int
        param.method.fullName shouldBe subscriptExt.fullName
      }
      cpg.call.nameExact(Operators.indexAccess) shouldBe empty
      inside(cpg.call.codeExact("table[6]").l) { case List(subscriptCall) =>
        subscriptCall.methodFullName shouldBe "SwiftTest.TimesTable.subscript.getter:(Swift.Int)->Swift.Int"
        subscriptCall.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
        subscriptCall.receiver.argumentIndex.loneElement shouldBe 0
        subscriptCall.receiver.code.loneElement shouldBe "table"
        subscriptCall.argument(1).code shouldBe "6"
      }
    }

    "be correct for subscript functions without accessors in extension" in {
      val fooCode =
        """
          |struct TimesTable {
          |  let multiplier: Int
          |}
          |
          |extension TimesTable {
          |  subscript(index: Int) -> Int {
          |    return multiplier * index
          |  }
          |}
          |
          |func main() {
          |  let table = TimesTable(multiplier: 3)
          |  print(table[6])
          |}
          |""".stripMargin
      val cpg = codeWithSwiftSetup(fooCode)

      val subscriptExt =
        cpg.method
          .fullNameExact("SwiftTest.TimesTable<extension>.subscript:(Swift.Int)->Swift.Int")
          .loneElement
      inside(subscriptExt.ast.isIdentifier.nameExact("index").l) { case List(id: Identifier) =>
        id.typeFullName shouldBe Defines.Int
        val param = id.refsTo.collectAll[MethodParameterIn].loneElement
        param.name shouldBe "index"
        param.typeFullName shouldBe Defines.Int
        param.method.fullName shouldBe subscriptExt.fullName
      }
      cpg.call.nameExact(Operators.indexAccess) shouldBe empty
      inside(cpg.call.codeExact("table[6]").l) { case List(subscriptCall) =>
        subscriptCall.methodFullName shouldBe "SwiftTest.TimesTable<extension>.subscript:(Swift.Int)->Swift.Int"
        subscriptCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
        subscriptCall.receiver shouldBe empty
        subscriptCall.argument(1).code shouldBe "6"
      }
    }

    "be correct for subscript functions with getter in extension" in {
      val fooCode =
        """
          |struct TimesTable {
          |  let multiplier: Int
          |}
          |
          |extension TimesTable {
          |  subscript(index: Int) -> Int {
          |    get {
          |      return multiplier * index
          |    }
          |  }
          |}
          |
          |func main() {
          |  let table = TimesTable(multiplier: 3)
          |  print(table[6])
          |}
          |""".stripMargin
      val cpg = codeWithSwiftSetup(fooCode)

      val subscriptExt =
        cpg.method
          .fullNameExact("SwiftTest.TimesTable<extension>.subscript.getter:(Swift.Int)->Swift.Int")
          .loneElement
      inside(subscriptExt.ast.isIdentifier.nameExact("index").l) { case List(id: Identifier) =>
        id.typeFullName shouldBe Defines.Int
        val param = id.refsTo.collectAll[MethodParameterIn].loneElement
        param.name shouldBe "index"
        param.typeFullName shouldBe Defines.Int
        param.method.fullName shouldBe subscriptExt.fullName
      }
      cpg.call.nameExact(Operators.indexAccess) shouldBe empty
      inside(cpg.call.codeExact("table[6]").l) { case List(subscriptCall) =>
        subscriptCall.methodFullName shouldBe "SwiftTest.TimesTable<extension>.subscript.getter:(Swift.Int)->Swift.Int"
        subscriptCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
        subscriptCall.receiver shouldBe empty
        subscriptCall.argument(1).code shouldBe "6"
      }
    }

    "be correct for subscript functions with setter" in {
      val fooCode =
        """
          |struct TimesTable {
          |  let multiplier: Int
          |  subscript(index: Int) -> Int {
          |    get {
          |      return multiplier * index
          |    }
          |    set {
          |      print("Setting value \(newValue) at index \(index)")
          |    }
          |  }
          |}
          |
          |func main() {
          |  var table = TimesTable(multiplier: 3)
          |  table[6] = 1
          |}
          |""".stripMargin
      val cpg = codeWithSwiftSetup(fooCode)

      cpg.method.fullName.sorted shouldBe Seq(
        "<operator>.alloc",
        "<operator>.arrayInitializer",
        "<operator>.assignment",
        "<operator>.fieldAccess",
        "<operator>.formatString",
        "<operator>.multiplication",
        "Package.swift:<global>",
        "Sources/main.swift:<global>",
        "Swift.print:(_:Any...,separator:Swift.String,terminator:Swift.String)->()",
        "SwiftTest.TimesTable.init:()->SwiftTest.TimesTable",
        "SwiftTest.TimesTable.init:(multiplier:Swift.Int)->SwiftTest.TimesTable",
        "SwiftTest.TimesTable.subscript.getter:(Swift.Int)->Swift.Int",
        "SwiftTest.TimesTable.subscript.setter:(Swift.Int)->Swift.Int",
        "SwiftTest.main:()->()"
      )

      cpg.call.nameExact(Operators.indexAccess) shouldBe empty
      cpg.call.isAssignment.code.l should not contain "table[6] = 1"

      cpg.call.methodFullName.sorted should contain("SwiftTest.TimesTable.subscript.setter:(Swift.Int)->Swift.Int")

      val List(setCall) = cpg.call.nameExact("subscript.setter").codeExact("table[6] = 1").l
      setCall.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
      setCall.methodFullName shouldBe "SwiftTest.TimesTable.subscript.setter:(Swift.Int)->Swift.Int"
      setCall.receiver.argumentIndex.loneElement shouldBe 0
      setCall.receiver.code.loneElement shouldBe "table"
      setCall.argument(1).code shouldBe "1"
      setCall.argument(2).code shouldBe "6"
    }

    "be correct for subscript functions with setter in extension" in {
      val fooCode =
        """
          |struct TimesTable {
          |  let multiplier: Int
          |}
          |
          |extension TimesTable {
          |  subscript(index: Int) -> Int {
          |    get {
          |      return multiplier * index
          |    }
          |    set {
          |      print("Setting value \(newValue) at index \(index)")
          |    }
          |  }
          |}
          |
          |func main() {
          |  var table = TimesTable(multiplier: 3)
          |  table[6] = 1
          |}
          |""".stripMargin
      val cpg = codeWithSwiftSetup(fooCode)

      cpg.method.fullName.sorted shouldBe Seq(
        "<operator>.alloc",
        "<operator>.arrayInitializer",
        "<operator>.assignment",
        "<operator>.fieldAccess",
        "<operator>.formatString",
        "<operator>.multiplication",
        "Package.swift:<global>",
        "Sources/main.swift:<global>",
        "Swift.print:(_:Any...,separator:Swift.String,terminator:Swift.String)->()",
        "SwiftTest.TimesTable.init:()->SwiftTest.TimesTable",
        "SwiftTest.TimesTable.init:(multiplier:Swift.Int)->SwiftTest.TimesTable",
        "SwiftTest.TimesTable<extension>.subscript.getter:(Swift.Int)->Swift.Int",
        "SwiftTest.TimesTable<extension>.subscript.setter:(Swift.Int)->Swift.Int",
        "SwiftTest.main:()->()"
      )

      cpg.call.nameExact(Operators.indexAccess) shouldBe empty
      cpg.call.isAssignment.code.l should not contain "table[6] = 1"

      cpg.call.methodFullName.sorted should contain(
        "SwiftTest.TimesTable<extension>.subscript.setter:(Swift.Int)->Swift.Int"
      )

      val List(setCall) = cpg.call.nameExact("subscript.setter").codeExact("table[6] = 1").l
      setCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      setCall.methodFullName shouldBe "SwiftTest.TimesTable<extension>.subscript.setter:(Swift.Int)->Swift.Int"
      setCall.receiver shouldBe empty
      setCall.argument(1).code shouldBe "1"
      setCall.argument(2).code shouldBe "6"
    }

  }

}
