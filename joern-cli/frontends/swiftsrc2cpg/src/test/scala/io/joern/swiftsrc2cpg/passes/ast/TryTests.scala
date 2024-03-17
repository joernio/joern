// This test file has been translated from swift/test/Parse/try.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.AstSwiftSrc2CpgSuite

class TryTests extends AstSwiftSrc2CpgSuite {

  "TryTests" should {

    "testTry4" ignore {
      val cpg = code("""
        |var x = try foo() + bar()
        |x = try foo() + bar()
        |x += try foo() + bar()
        |x += try foo() %%%% bar()
        |x += try foo() %%% bar()
        |x = foo() + try bar()
        |""".stripMargin)
      ???
    }

    "testTry5" ignore {
      val cpg = code("""
        |var y = true ? try foo() : try bar() + 0
        |var z = true ? try foo() : try bar() %%% 0
        |""".stripMargin)
      ???
    }

    "testTry6" ignore {
      val cpg = code("""
        |var a = try! foo() + bar()
        |a = try! foo() + bar()
        |a += try! foo() + bar()
        |a += try! foo() %%%% bar()
        |a += try! foo() %%% bar()
        |a = foo() + try! bar()
        |""".stripMargin)
      ???
    }

    "testTry7" ignore {
      val cpg = code("""
        |var b = true ? try! foo() : try! bar() + 0
        |var c = true ? try! foo() : try! bar() %%% 0
        |""".stripMargin)
      ???
    }

    "testTry9" ignore {
      val cpg = code("""
        |var i = try? foo() + bar()
        |let _: Double = i
        |i = try? foo() + bar()
        |i ?+= try? foo() + bar()
        |i ?+= try? foo() %%%% bar()
        |i ?+= try? foo() %%% bar()
        |_ = foo() == try? bar()
        |_ = (try? foo()) == bar()
        |_ = foo() == (try? bar())
        |_ = (try? foo()) == (try? bar())
        |""".stripMargin)
      ???
    }

    "testTry10" ignore {
      val cpg = code("""
        |let j = true ? try? foo() : try? bar() + 0
        |let k = true ? try? foo() : try? bar() %%% 0
        |""".stripMargin)
      ???
    }

    "testTry18" ignore {
      val cpg = code("let _ = (try? foo())!!")
      ???
    }

    "testTry32" ignore {
      val cpg = code("""
        |let _: Int? = try? produceAny() as? Int
        |let _: Int?? = (try? produceAny()) as? Int // good
        |let _: String = try? produceAny() as? Int
        |let _: String = (try? produceAny()) as? Int
        |""".stripMargin)
      ???
    }

  }

}
