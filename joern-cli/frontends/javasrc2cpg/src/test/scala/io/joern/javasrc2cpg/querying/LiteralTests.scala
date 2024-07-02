package io.joern.javasrc2cpg.querying

import com.github.javaparser.ast.expr.LiteralExpr
import io.joern.javasrc2cpg.testfixtures.JavaSrcCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.{Identifier, Literal}
import io.shiftleft.semanticcpg.language.*

class LiteralTests extends JavaSrcCode2CpgFixture {

  val cpg = code("""
      |class Test {
      |  public void foo() {
      |    byte a = 0b10110010;
      |    short b = 0;
      |    int c = 0175;
      |    int d = 0xABCD;
      |    long e = 9223372036854775807;
      |    float f = 0.42f;
      |    double g = 11d;
      |    double h = 11.0;
      |    double i = 1.0e2D;
      |    char j = 'j';
      |    char k = 062;
      |    char l = '\n';
      |    String m = "Hello, world!";
      |    String n = null;
      |    boolean o = true;
      |    boolean p = false;
      |  }
      |}
      |""".stripMargin)

  val expectedOutput = List(
    ("a", "0b10110010", "byte"),
    ("b", "0", "short"),
    ("c", "0175", "int"),
    ("d", "0xABCD", "int"),
    ("e", "9223372036854775807", "long"),
    ("f", "0.42f", "float"),
    ("g", "11d", "double"),
    ("h", "11.0", "double"),
    ("i", "1.0e2D", "double"),
    ("j", "'j'", "char"),
    ("k", "062", "char"),
    ("l", "'\\n'", "char"),
    ("m", "\"Hello, world!\"", "java.lang.String"),
    ("n", "null", "java.lang.String"),
    ("o", "true", "boolean"),
    ("p", "false", "boolean")
  )

  "should correctly parse literals of all types" in {
    val valueMap = cpg.assignment.map { a =>
      val List(identifier: Identifier, value: Literal) = a.argument.l: @unchecked
      identifier.name -> (identifier, value)
    }.toMap

    expectedOutput.foreach { case (identifier, value, typ) =>
      withClue(s"$identifier should have value $value") {
        val (actualIdentifier: Identifier, actualValue: Literal) = valueMap(identifier)
        actualValue.code shouldBe value
        actualIdentifier.typeFullName shouldBe typ
      }
    }
  }
}
