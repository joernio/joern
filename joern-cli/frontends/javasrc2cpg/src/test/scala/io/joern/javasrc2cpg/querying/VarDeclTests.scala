package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.Config
import io.joern.javasrc2cpg.testfixtures.JavaSrcCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.{Block, Call, Local}
import io.shiftleft.semanticcpg.language.*

class VarDeclTests extends JavaSrcCode2CpgFixture {

  "it should correctly parse a combined declaration and assignment" in {
    val cpg = code("""
        |public class Foo {
        |      public void test1() {
        |           int x = 1;
        |      }
        |}
        |""".stripMargin)
    val methodBody = cpg.method.name("test1").astChildren.collect { case b: Block => b }.head
    methodBody.astChildren.size shouldBe 2

    val (local, assig) = methodBody.astChildren.l match {
      case List(local: Local, assig: Call) => (local, assig)

      case res => fail(s"Expected List(Local, Call) but got ${res.map(_.label)}")
    }

    local.name shouldBe "x"
    local.order shouldBe 1

    assig.code shouldBe "int x = 1"
    assig.order shouldBe 2
  }

  "it should correctly parse separated declarations and assignments" in {
    val cpg = code("""
        |public class Foo {
        |    public void test2() {
        |        int x;
        |        x = 1;
        |    }
        |}
        |""".stripMargin)
    val methodBody = cpg.method.name("test2").astChildren.collect { case b: Block => b }.head
    methodBody.astChildren.size shouldBe 2

    val (local, assig) = methodBody.astChildren.l match {
      case List(local: Local, assig: Call) => (local, assig)

      case res => fail(s"Expected List(Local, Call) but got ${res.map(_.label)}")
    }

    local.name shouldBe "x"
    local.order shouldBe 1

    assig.code shouldBe "x = 1"
    assig.order shouldBe 2
  }

  "it should correctly parse multiple declarations in a single statement" in {
    val cpg = code("""
        |public class Foo {
        |    public void test3() {
        |        int x, y;
        |        x = 1;
        |        y = 2;
        |    }
        |}
        |""".stripMargin)
    val methodBody = cpg.method.name("test3").astChildren.collect { case b: Block => b }.head
    methodBody.astChildren.size shouldBe 4

    val (localX, localY, assigX, assigY) = methodBody.astChildren.l match {
      case List(localX: Local, localY: Local, assigX: Call, assigY: Call) => (localX, localY, assigX, assigY)

      case res => fail(s"Expected List(Local, Local, Call, Call) but got ${res.map(_.label)}")
    }

    localX.name shouldBe "x"
    localX.order shouldBe 1
    localY.name shouldBe "y"
    localY.order shouldBe 2

    assigX.code shouldBe "x = 1"
    assigX.order shouldBe 3
    assigY.code shouldBe "y = 2"
    assigY.order shouldBe 4
  }

  "it should correctly parse mixed declarations and assignments in a single statement" in {
    val cpg = code("""
        |public class Foo {
        |    public void test4() {
        |        int x, y = 4, z;
        |        x = 1;
        |        z = 2;
        |    }
        |}
        |""".stripMargin)
    val methodBody = cpg.method.name("test4").astChildren.collect { case b: Block => b }.head
    methodBody.astChildren.size shouldBe 6

    val (localX, localY, localZ, assigY, assigX, assigZ) = methodBody.astChildren.l match {
      case List(lX: Local, lY: Local, lZ: Local, aY: Call, aX: Call, aZ: Call) => (lX, lY, lZ, aY, aX, aZ)

      case res => fail(s"Expected List(Local, Local, Local, Call, Call, Call) but got ${res.map(_.label)}")
    }

    localX.name shouldBe "x"
    localX.order shouldBe 1
    localY.name shouldBe "y"
    localY.order shouldBe 2
    localZ.name shouldBe "z"
    localZ.order shouldBe 3

    assigY.code shouldBe "int y = 4"
    assigY.order shouldBe 4
    assigX.code shouldBe "x = 1"
    assigX.order shouldBe 5
    assigZ.code shouldBe "z = 2"
    assigZ.order shouldBe 6
  }

  "it should correctly parse mixed declarations and assignments across statements" in {
    val cpg = code("""
        |public class Foo {
        |    public void test5() {
        |        int x, y = 2;
        |        int z = 3;
        |        x = 1;
        |    }
        |}
        |""".stripMargin)
    val methodBody = cpg.method.name("test5").astChildren.collect { case b: Block => b }.head
    methodBody.astChildren.size shouldBe 6

    val (localX, localY, assigY, localZ, assigZ, assigX) = methodBody.astChildren.l match {
      case List(lX: Local, lY: Local, aY: Call, lZ: Local, aZ: Call, aX: Call) => (lX, lY, aY, lZ, aZ, aX)

      case res => fail(s"Expected List(Local, Local, Call, Local, Call, Call) but got ${res.map(_.label)}")
    }

    localX.name shouldBe "x"
    localX.order shouldBe 1
    localY.name shouldBe "y"
    localY.order shouldBe 2

    assigY.code shouldBe "int y = 2"
    assigY.order shouldBe 3

    localZ.name shouldBe "z"
    localZ.order shouldBe 4

    assigZ.code shouldBe "int z = 3"
    assigZ.order shouldBe 5

    assigX.code shouldBe "x = 1"
    assigX.order shouldBe 6
  }

  "generics with 'keep type arguments' config" should {

    "show the fully qualified type arguments for stdlib `List and `Map` objects" in {
      val cpg = code("""
          |import java.util.ArrayList;
          |import java.util.List;
          |import java.util.HashMap;
          |
          |public class Main {
          |    public static void main(String[] args) {
          |        // Create a List of Strings
          |        List<String> stringList = new ArrayList<>();
          |        var stringIntMap = new HashMap<String, Integer>();
          |    }
          |}
          |
          |""".stripMargin)
        .withConfig(Config().withKeepTypeArguments(true))

      cpg.identifier("stringList").typeFullName.head shouldBe "java.util.List<java.lang.String>"
      cpg.identifier("stringIntMap").typeFullName.head shouldBe "java.util.HashMap<java.lang.String,java.lang.Integer>"
    }

    "show the fully qualified names of external types" in {
      val cpg = code("""
          |import org.apache.flink.streaming.api.datastream.DataStream;
          |import org.apache.flink.streaming.api.environment.StreamExecutionEnvironment;
          |import org.apache.flink.streaming.connectors.kafka.FlinkKafkaProducer;
          |import org.apache.flink.streaming.util.serialization.SimpleStringSchema;
          |
          |import java.util.Properties;
          |
          |public class FlinkKafkaExample {
          |    public static void main() throws Exception {
          |        Properties kafkaProps = new Properties();
          |        SimpleStringSchema schema = new SimpleStringSchema();
          |        FlinkKafkaProducer<String> kafkaProducer = new FlinkKafkaProducer<String>("kafka-topic", schema, kafkaProps);
          |    }
          |}
          |""".stripMargin).withConfig(Config().withKeepTypeArguments(true))

      cpg.call
        .codeExact("new FlinkKafkaProducer<String>(\"kafka-topic\", schema, kafkaProps)")
        .filterNot(_.name == Operators.alloc)
        .map(_.methodFullName)
        .head shouldBe "org.apache.flink.streaming.connectors.kafka.FlinkKafkaProducer.<init>:<unresolvedSignature>(3)"
    }

  }

}
