package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.JavaSrc2CpgTestContext
import io.shiftleft.codepropertygraph.generated.edges.Capture
import io.shiftleft.codepropertygraph.generated.nodes.ClosureBinding
import io.shiftleft.semanticcpg.language._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class LambdaTests extends AnyFreeSpec with Matchers {

  "CPG for code with a simple lambda which captures a method parameter" - {
    lazy val cpg = JavaSrc2CpgTestContext.buildCpg(
    """
      |import java.util.Comparator;
      |
      |public class Foo {
      |
      |    int value;
      |
      |    public Foo(int value) {
      |        this.value = value;
      |    }
      |
      |    public static Comparator<Foo> getComparator(int offset) {
      |        return (fst, snd) -> fst.value - snd.value + offset;
      |    }
      |
      |    public static void main(String[] args) {
      |        Comparator<Foo> valueComparator = getComparator(1);
      |
      |        Foo t1 = new Foo(5);
      |        Foo t2 = new Foo(3);
      |
      |        if (valueComparator.compare(t1, t2) > 0) {
      |            System.out.println("T1 is bigger");
      |        }
      |    }
      |}
      |""".stripMargin)

    "it should contain a METHOD_REF node" in {
      cpg.methodRef.size shouldBe 1
    }

    "it should contain a capture edge for the methodRef" in {
      cpg.methodRef.outE.count(_.isInstanceOf[Capture]) shouldBe 1
    }

    "it should contain a LOCAL node for the captured `offset`" in {
      cpg.local.count(_.name == "offset") shouldBe 1
    }

    "it should contain a CLOSURE_BINDING node for the captured `offset`" in {
      cpg.all.count(_.isInstanceOf[ClosureBinding]) shouldBe 1
    }

    "the CLOSURE_BINDING node should contain a REF edge to METHOD_PARAMETER_IN" in {
      cpg.all.filter(_.isInstanceOf[ClosureBinding]).outE.size shouldBe 1
    }
  }

  "CPG for code with a simple lambda which captures a local" - {
    lazy val cpg = JavaSrc2CpgTestContext.buildCpg(
      """
        |import java.util.Comparator;
        |
        |public class Foo {
        |
        |    int value;
        |
        |    public Foo(int value) {
        |        this.value = value;
        |    }
        |
        |    public static Comparator<Foo> getComparator(int offset) {
        |        int diff = offset + 12;
        |        return (fst, snd) -> fst.value - snd.value + diff;
        |    }
        |
        |    public static void main(String[] args) {
        |        Comparator<Foo> valueComparator = getComparator(1);
        |
        |        Foo t1 = new Foo(5);
        |        Foo t2 = new Foo(3);
        |
        |        if (valueComparator.compare(t1, t2) > 0) {
        |            System.out.println("T1 is bigger");
        |        }
        |    }
        |}
        |""".stripMargin)

    "should contain a METHOD_REF node" in {
      cpg.methodRef.size shouldBe 1
    }

    "should contain a capture edge for the methodRef" in {
      cpg.methodRef.outE.count(_.isInstanceOf[Capture]) shouldBe 1
    }

    "should contain a LOCAL node for the captured `diff`" in {
      // The code for the `getComparator` local is `int diff`, so
      // this pattern matches only the local created for the lambda.
      cpg.local.code("diff").size shouldBe 1
    }

    "should contain a CLOSURE_BINDING node for the captured `diff`" in {
      cpg.all.count(_.isInstanceOf[ClosureBinding]) shouldBe 1
    }
  }
}
