// This test file has been translated from swift/test/Parse/try.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class TryTests extends AbstractPassTest {

  "TryTests" should {

    "testTry4" ignore AstFixture("""
        |var x = try foo() + bar()
        |x = try foo() + bar()
        |x += try foo() + bar()
        |x += try foo() %%%% bar()
        |x += try foo() %%% bar()
        |x = foo() + try bar()
        |""".stripMargin) { cpg => ??? }

    "testTry5" ignore AstFixture("""
        |var y = true ? try foo() : try bar() + 0
        |var z = true ? try foo() : try bar() %%% 0
        |""".stripMargin) { cpg => ??? }

    "testTry6" ignore AstFixture("""
        |var a = try! foo() + bar()
        |a = try! foo() + bar()
        |a += try! foo() + bar()
        |a += try! foo() %%%% bar()
        |a += try! foo() %%% bar()
        |a = foo() + try! bar()
        |""".stripMargin) { cpg => ??? }

    "testTry7" ignore AstFixture("""
        |var b = true ? try! foo() : try! bar() + 0
        |var c = true ? try! foo() : try! bar() %%% 0
        |""".stripMargin) { cpg => ??? }

    "testTry9" ignore AstFixture("""
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
        |""".stripMargin) { cpg => ??? }

    "testTry10" ignore AstFixture("""
        |let j = true ? try? foo() : try? bar() + 0
        |let k = true ? try? foo() : try? bar() %%% 0
        |""".stripMargin) { cpg => ??? }

    "testTry18" ignore AstFixture("let _ = (try? foo())!!") { cpg => ??? }

    "testTry32" ignore AstFixture("""
        |let _: Int? = try? produceAny() as? Int
        |let _: Int?? = (try? produceAny()) as? Int // good
        |let _: String = try? produceAny() as? Int
        |let _: String = (try? produceAny()) as? Int
        |""".stripMargin) { cpg => ??? }

  }

}
