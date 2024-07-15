package io.joern.rubysrc2cpg.deprecated.passes.cfg

import io.joern.rubysrc2cpg.testfixtures.RubyCfgTestCpg
import io.joern.x2cpg.passes.controlflow.cfgcreation.Cfg.AlwaysEdge
import io.joern.x2cpg.testfixtures.CfgTestFixture
import io.shiftleft.codepropertygraph.generated.Cpg

class SimpleCfgCreationPassTest extends CfgTestFixture(() => new RubyCfgTestCpg(useDeprecatedFrontend = true)) {

  "CFG generation for simple fragments" should {
    "have correct structure for empty array literal" ignore {
      implicit val cpg: Cpg = code("x = []")
      succOf(":program") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("x = []", AlwaysEdge))
      succOf("x = []") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

    "have correct structure for array literal with values" in {
      implicit val cpg: Cpg = code("x = [1, 2]")
      succOf("1") should contain theSameElementsAs expected(("2", AlwaysEdge))
      succOf("x = [1, 2]") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

    "assigning a literal value" in {
      implicit val cpg: Cpg = code("x = 1")
      succOf(":program") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x = 1") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

    "assigning a string literal value" in {
      implicit val cpg: Cpg = code("x = 'some literal'")
      succOf(":program") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x = 'some literal'") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

    "addition of two numbers" in {
      implicit val cpg: Cpg = code("x = 1 + 2")
      succOf(":program") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x = 1 + 2") should contain theSameElementsAs expected(("RET", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("1", AlwaysEdge))
      succOf("2") should contain theSameElementsAs expected(("1 + 2", AlwaysEdge))
      succOf("1") should contain theSameElementsAs expected(("2", AlwaysEdge))
      succOf("1 + 2") should contain theSameElementsAs expected(("x = 1 + 2", AlwaysEdge))
    }

    "addition of two string" in {
      implicit val cpg: Cpg = code("x = 1 + 2")
      succOf(":program") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("x = 1 + 2") should contain theSameElementsAs expected(("RET", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("1", AlwaysEdge))
      succOf("2") should contain theSameElementsAs expected(("1 + 2", AlwaysEdge))
      succOf("1") should contain theSameElementsAs expected(("2", AlwaysEdge))
      succOf("1 + 2") should contain theSameElementsAs expected(("x = 1 + 2", AlwaysEdge))
    }

    "addition of multiple string" in {
      implicit val cpg: Cpg = code("""
          |a = "Nice to meet you"
          |b = ", "
          |c = "do you like blueberries?"
          |a+b+c
          |""".stripMargin)
      succOf(":program") should contain theSameElementsAs expected(("a", AlwaysEdge))
      succOf("a") should contain theSameElementsAs expected(("\"Nice to meet you\"", AlwaysEdge))
      succOf("b") should contain theSameElementsAs expected(("\", \"", AlwaysEdge))
      succOf("c") should contain theSameElementsAs expected(("\"do you like blueberries?\"", AlwaysEdge))
      succOf("a+b+c") should contain theSameElementsAs expected(("RET", AlwaysEdge))
      succOf("a+b") should contain theSameElementsAs expected(("c", AlwaysEdge))
      succOf("\"Nice to meet you\"") should contain theSameElementsAs expected(("a = \"Nice to meet you\"", AlwaysEdge))
      succOf("\", \"") should contain theSameElementsAs expected(("b = \", \"", AlwaysEdge))
      succOf("\"do you like blueberries?\"") should contain theSameElementsAs expected(
        ("c = \"do you like blueberries?\"", AlwaysEdge)
      )
    }

    "addition of multiple string and assign to variable" in {
      implicit val cpg: Cpg = code("""
          |a = "Nice to meet you"
          |b = ", "
          |c = "do you like blueberries?"
          |x = a+b+c
          |""".stripMargin)
      succOf(":program") should contain theSameElementsAs expected(("a", AlwaysEdge))
      succOf("a") should contain theSameElementsAs expected(("\"Nice to meet you\"", AlwaysEdge))
      succOf("b") should contain theSameElementsAs expected(("\", \"", AlwaysEdge))
      succOf("c") should contain theSameElementsAs expected(("\"do you like blueberries?\"", AlwaysEdge))
      succOf("a+b+c") should contain theSameElementsAs expected(("x = a+b+c", AlwaysEdge))
      succOf("a+b") should contain theSameElementsAs expected(("c", AlwaysEdge))
      succOf("\"Nice to meet you\"") should contain theSameElementsAs expected(("a = \"Nice to meet you\"", AlwaysEdge))
      succOf("\", \"") should contain theSameElementsAs expected(("b = \", \"", AlwaysEdge))
      succOf("\"do you like blueberries?\"") should contain theSameElementsAs expected(
        ("c = \"do you like blueberries?\"", AlwaysEdge)
      )
      succOf("x") should contain theSameElementsAs expected(("a", AlwaysEdge))
    }

    "single hierarchy of if else statement" in {
      implicit val cpg: Cpg = code("""
          |x = 1
          |if x > 2
          |   puts "x is greater than 2"
          |end
          |""".stripMargin)
      succOf(":program") should contain theSameElementsAs expected(("puts", AlwaysEdge))
      succOf("puts") should contain theSameElementsAs expected(("__builtin.puts", AlwaysEdge))
      succOf("__builtin.puts") should contain theSameElementsAs expected(("puts = __builtin.puts", AlwaysEdge))
      succOf("puts = __builtin.puts") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("1") should contain theSameElementsAs expected(("x = 1", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("1", AlwaysEdge))
      succOf("2") should contain theSameElementsAs expected(("x > 2", AlwaysEdge))
    }

    "multiple hierarchy of if else statement" ignore {
      implicit val cpg: Cpg = code("""
          |x = 1
          |if x > 2
          |   puts "x is greater than 2"
          |elsif x <= 2 and x!=0
          |   puts "x is 1"
          |else
          |   puts "I can't guess the number"
          |end
          |""".stripMargin)
      succOf(":program") should contain theSameElementsAs expected(("puts", AlwaysEdge))
      succOf("puts") should contain theSameElementsAs expected(("__builtin.puts", AlwaysEdge))
      succOf("__builtin.puts") should contain theSameElementsAs expected(("puts = __builtin.puts", AlwaysEdge))
      succOf("puts = __builtin.puts") should contain theSameElementsAs expected(("x", AlwaysEdge))
      succOf("1") should contain theSameElementsAs expected(("x = 1", AlwaysEdge))
      succOf("x") should contain theSameElementsAs expected(("1", AlwaysEdge))
      succOf("2") should contain theSameElementsAs expected(("x > 2", AlwaysEdge))
      succOf("x <= 2 and x!=0") should contain theSameElementsAs expected(("\"x is 1\"", AlwaysEdge))
      succOf("x <= 2 and x!=0") should contain theSameElementsAs expected(("RET", AlwaysEdge))
    }

  }
}
