// This test file has been translated from swift/test/Parse/init_deinit.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.AstSwiftSrc2CpgSuite

class InitDeinitTests extends AstSwiftSrc2CpgSuite {

  "InitDeinitTests" should {

    "testInitDeinit1" ignore {
      val cpg = code("""
        |struct FooStructConstructorA {
        |  init()
        |}
        |""".stripMargin)
      ???
    }

    "testInitDeinit2" ignore {
      val cpg = code("""
        |struct FooStructConstructorA {
        |  init() {}
        |}
        |""".stripMargin)
      ???
    }

    "testInitDeinit3" ignore {
      val cpg = code("""
        |struct FooStructConstructorA {
        |  init<T>() {}
        |}
        |""".stripMargin)
      ???
    }

    "testInitDeinit4" ignore {
      val cpg = code("""
        |struct FooStructConstructorA {
        |  init?() { self.init() }
        |}
        |""".stripMargin)
      ???
    }

    "testInitDeinit5" ignore {
      val cpg = code("""
        |struct FooStructDeinitializerA {
        |  deinit
        |}
        |""".stripMargin)
      ???
    }

    "testInitDeinit6" ignore {
      val cpg = code("""
        |struct FooStructDeinitializerA {
        |  deinit {}
        |}
        |""".stripMargin)
      ???
    }

    "testInitDeinit7" ignore {
      val cpg = code("""
        |class FooStructDeinitializerA {
        |  deinit {}
        |}
        |""".stripMargin)
      ???
    }

    "testInitDeinit12" ignore {
      val cpg = code("""
        |deinit {}
        |deinit
        |deinit {}
        |""".stripMargin)
      ???
    }

    "testInitDeinit13" ignore {
      val cpg = code("""
        |struct BarStruct {
        |  init() {}
        |  deinit {}
        |}
        |""".stripMargin)
      ???
    }

    "testInitDeinit14" ignore {
      val cpg = code("""
        |extension BarStruct {
        |  init(x : Int) {}
        |  deinit {}
        |}
        |""".stripMargin)
      ???
    }

    "testInitDeinit15" ignore {
      val cpg = code("""
        |enum BarUnion {
        |  init() {}
        |  deinit {}
        |}
        |""".stripMargin)
      ???
    }

    "testInitDeinit16" ignore {
      val cpg = code("""
        |extension BarUnion {
        |  init(x : Int) {}
        |  deinit {}
        |}
        |""".stripMargin)
      ???
    }

    "testInitDeinit17" ignore {
      val cpg = code("""
        |class BarClass {
        |  init() {}
        |  deinit {}
        |}
        |""".stripMargin)
      ???
    }

    "testInitDeinit19" ignore {
      val cpg = code("""
        |protocol BarProtocol {
        |  init() {}
        |  deinit {}
        |}
        |""".stripMargin)
      ???
    }

    "testInitDeinit20" ignore {
      val cpg = code("""
        |extension BarProtocol {
        |  init(x : Int) {}
        |  deinit {}
        |}
        |""".stripMargin)
      ???
    }

    "testInitDeinit21" ignore {
      val cpg = code("""
        |func fooFunc() {
        |  init() {}
        |  deinit {}
        |}
        |""".stripMargin)
      ???
    }

    "testInitDeinit24" ignore {
      val cpg = code("""
        |class Aaron {
        |  convenience init() { init(x: 1) }
        |}
        |""".stripMargin)
      ???
    }

    "testDeinitInSwiftinterfaceIsFollowedByFinalFunc" ignore {
      val cpg = code("""
        |class Foo {
        |  deinit
        |  final func foo()
        |}
        |""".stripMargin)
      ???
    }

    "testDeinitAsync" ignore {
      val cpg = code("""
        |class FooClassDeinitializerA {
        |  deinit async {}
        |}
        |""".stripMargin)
      ???
    }

    "testAsyncDeinit" ignore {
      val cpg = code(
        // This is expected for now.
        // `async` is parsed as a modifier like `public` because you can have an `async var x: Int`.
        """
        |class FooClassDeinitializerA {
        |  async deinit {}
        |}
        |""".stripMargin
      )
      ???
    }

  }

}
