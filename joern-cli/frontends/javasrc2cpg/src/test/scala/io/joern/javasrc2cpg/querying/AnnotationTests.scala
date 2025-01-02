package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class AnnotationTests extends JavaSrcCode2CpgFixture {
  "annotations that cannot be resolved from imports" should {
    val cpg = code("""
        |package foo;
        |
        |@interface TestMarker {}
        |""".stripMargin)
      .moreCode("""
          |package bar;
          |
          |import foo.*;
          |import bar.*;
          |
          |public class Bar {
          |  @TestMarker
          |  public void bar() {}
          |}
          |""".stripMargin)

    "have the annotation type be resolved" in {
      cpg.method.name("bar").annotation.fullName.l shouldBe List("foo.TestMarker")
    }
  }

  "normal value annotations" should {
    lazy val cpg = code("""
        |import some.NormalAnnotation;
        |public class SomeClass {
        |
        |  @NormalAnnotation(value = "classAnnotation")
        |  void function() {
        |
        |  }
        |}
        |""".stripMargin)

    "test annotation node properties" in {
      val annotationNode = cpg.method.name("function").annotation.head
      annotationNode.code shouldBe "@NormalAnnotation(value = \"classAnnotation\")"
      annotationNode.name shouldBe "NormalAnnotation"
      annotationNode.fullName shouldBe "some.NormalAnnotation"
      annotationNode.lineNumber shouldBe Some(5)
      annotationNode.columnNumber shouldBe Some(3)
      annotationNode.asInstanceOf[CfgNode].method.fullName shouldBe "SomeClass.function:void()"
    }

    "test annotation node parameter assignment child" in {
      val Seq(paramAssign) = cpg.method.name("function").annotation.parameterAssign.l
      paramAssign.code shouldBe "value = \"classAnnotation\""
      paramAssign.order shouldBe 1
    }

    "test annotation node parameter child" in {
      val Seq(param) = cpg.method.name("function").annotation.parameterAssign.parameter.l
      param.code shouldBe "value"
      param.order shouldBe 1
    }

    "test annotation node parameter value" in {
      val Seq(paramValue) = cpg.method.name("function").annotation.parameterAssign.value.l
      paramValue.code shouldBe "classAnnotation"
      paramValue.order shouldBe 2
      paramValue.argumentIndex shouldBe 2
    }
  }

  "single annotations" should {
    lazy val cpg = code("""
        |import some.SingleAnnotation;
        |public class SomeClass {
        |
        |  @SingleAnnotation("classAnnotation")
        |  void function() {
        |
        |  }
        |}
        |""".stripMargin)

    "test annotation node properties" in {
      val annotationNode = cpg.method.name("function").annotation.head
      annotationNode.code shouldBe "@SingleAnnotation(\"classAnnotation\")"
      annotationNode.name shouldBe "SingleAnnotation"
      annotationNode.fullName shouldBe "some.SingleAnnotation"
      annotationNode.lineNumber shouldBe Some(5)
      annotationNode.columnNumber shouldBe Some(3)
    }

    "test annotation node parameter assignment child" in {
      val Seq(paramAssign) = cpg.method.name("function").annotation.parameterAssign.l
      paramAssign.code shouldBe "\"classAnnotation\""
      paramAssign.order shouldBe 1
    }

    "test annotation node parameter child" in {
      val Seq(param) = cpg.method.name("function").annotation.parameterAssign.parameter.l
      param.code shouldBe "value"
      param.order shouldBe 1
    }

    "test annotation node parameter value" in {
      val Seq(paramValue) = cpg.method.name("function").annotation.parameterAssign.value.l
      paramValue.code shouldBe "classAnnotation"
      paramValue.order shouldBe 2
      paramValue.argumentIndex shouldBe 2
      paramValue.method.fullName shouldBe "SomeClass.function:void()"
    }
  }

  "function marker annotations" should {
    lazy val cpg = code("""
        |import some.MarkerAnnotation;
        |public class SomeClass {
        |
        |  @MarkerAnnotation()
        |  void function() {
        |
        |  }
        |}
        |""".stripMargin)
    "test annotation node properties" in {
      val annotationNode = cpg.method.name("function").annotation.head
      annotationNode.code shouldBe "@MarkerAnnotation()"
      annotationNode.name shouldBe "MarkerAnnotation"
      annotationNode.fullName shouldBe "some.MarkerAnnotation"
      annotationNode.lineNumber shouldBe Some(5)
      annotationNode.columnNumber shouldBe Some(3)
    }

    "test annotation node parameter assignment child" in {
      cpg.method.name("function").annotation.parameterAssign.isEmpty shouldBe true
    }
  }

  "class marker annotations" should {
    lazy val cpg = code("""
        |import some.MarkerAnnotation;
        |public class SomeClass {
        |
        |  @MarkerAnnotation()
        |  public SomeClass() {
        |
        |  }
        |}
        |""".stripMargin)

    "test annotation node properties" in {
      val annotationNode = cpg.method.nameExact(io.joern.x2cpg.Defines.ConstructorMethodName).annotation.head
      annotationNode.code shouldBe "@MarkerAnnotation()"
      annotationNode.name shouldBe "MarkerAnnotation"
      annotationNode.fullName shouldBe "some.MarkerAnnotation"
      annotationNode.lineNumber shouldBe Some(5)
      annotationNode.columnNumber shouldBe Some(3)
    }

    "test annotation node parameter assignment child" in {
      cpg.method.name("function").annotation.parameterAssign.isEmpty shouldBe true
    }
  }

  "parameter annotations" should {
    lazy val cpg = code("""
        |import some.MarkerAnnotation;
        |public class SomeClass {
        |
        |  void function(@MarkerAnnotation int x) {
        |
        |  }
        |}
        |""".stripMargin)
    "test annotation node properties" in {
      val annotationNode = cpg.method.name("function").parameter.name("x").annotation.head
      annotationNode.code shouldBe "@MarkerAnnotation"
      annotationNode.name shouldBe "MarkerAnnotation"
      annotationNode.fullName shouldBe "some.MarkerAnnotation"
      annotationNode.lineNumber shouldBe Some(5)
      annotationNode.columnNumber shouldBe Some(17)
    }
  }

  "field annotations" should {
    lazy val cpg = code("""
        |import some.MarkerAnnotation;
        |public class SomeClass {
        |  @MarkerAnnotation int x;
        |}
        |""".stripMargin)

    "test annotation node properties" in {
      val annotationNode = cpg.typeDecl.name("SomeClass").member.name("x").annotation.head
      annotationNode.code shouldBe "@MarkerAnnotation"
      annotationNode.name shouldBe "MarkerAnnotation"
      annotationNode.fullName shouldBe "some.MarkerAnnotation"
      annotationNode.lineNumber shouldBe Some(4)
      annotationNode.columnNumber shouldBe Some(3)
    }
  }

  "function value annotations" should {
    lazy val cpg = code("""
        |import some.NormalAnnotation;
        |public class SomeClass {
        |
        |  @NormalAnnotation(value = 2)
        |  void function() {
        |
        |  }
        |}
        |""".stripMargin)
    "test annotation node properties" in {
      val annotationNode = cpg.method.name("function").annotation.head
      annotationNode.code shouldBe "@NormalAnnotation(value = 2)"
      annotationNode.name shouldBe "NormalAnnotation"
      annotationNode.fullName shouldBe "some.NormalAnnotation"
      annotationNode.lineNumber shouldBe Some(5)
      annotationNode.columnNumber shouldBe Some(3)
    }

    "test annotation node parameter value" in {
      val Seq(paramValue: AnnotationLiteral) =
        cpg.method.name("function").annotation.parameterAssign.value.isAnnotationLiteral.l
      paramValue.code shouldBe "2"
      paramValue.order shouldBe 2
      paramValue.argumentIndex shouldBe 2
    }
  }

  "function value annotations with array initializers" should {
    lazy val cpg = code("""
        |import some.NormalAnnotation;
        |public class SomeClass {
        |
        |  @NormalAnnotation(value = {"aaa", "bbb"})
        |  void function() {
        |
        |  }
        |}
        |""".stripMargin)
    "test annotation node properties" in {
      val annotationNode = cpg.method.name("function").annotation.head
      annotationNode.code shouldBe "@NormalAnnotation(value = { \"aaa\", \"bbb\" })"
      annotationNode.name shouldBe "NormalAnnotation"
      annotationNode.fullName shouldBe "some.NormalAnnotation"
      annotationNode.lineNumber shouldBe Some(5)
      annotationNode.columnNumber shouldBe Some(3)
    }

    "test annotation node parameter assignment child" in {
      val Seq(paramAssign) = cpg.method.name("function").annotation.parameterAssign.l
      paramAssign.code shouldBe "value = { \"aaa\", \"bbb\" }"
      paramAssign.order shouldBe 1
    }

    "test annotation node parameter child" in {
      val Seq(param) = cpg.method.name("function").annotation.parameterAssign.parameter.l
      param.code shouldBe "value"
      param.order shouldBe 1
    }

    "test annotation node parameter value" in {
      val Seq(paramValue: ArrayInitializer) =
        cpg.method.name("function").annotation.parameterAssign.value.isArrayInitializer.l
      paramValue.code shouldBe "{ \"aaa\", \"bbb\" }"
      paramValue.order shouldBe 2
      paramValue.argumentIndex shouldBe 2
    }

    "test annotation node array initializer children" in {
      val children = cpg.method.name("function").annotation.parameterAssign.value.astChildren.isExpression.s
      children.find(_.code == "aaa").map(_.order) shouldBe Some(1)
      children.find(_.code == "aaa").map(_.argumentIndex) shouldBe Some(1)
      children.find(_.code == "bbb").map(_.order) shouldBe Some(2)
      children.find(_.code == "bbb").map(_.argumentIndex) shouldBe Some(2)
    }
  }

  "nested annotations" should {
    lazy val cpg = code("""
        |import some.NormalAnnotation;
        |import some.OtherAnnotation;
        |public class SomeClass {
        |
        |  @NormalAnnotation(value = @OtherAnnotation)
        |  void function() {
        |
        |  }
        |}
        |""".stripMargin)

    "test annotation node properties" in {
      val annotationNode = cpg.method.name("function").annotation.head
      annotationNode.code shouldBe "@NormalAnnotation(value = @OtherAnnotation)"
      annotationNode.name shouldBe "NormalAnnotation"
      annotationNode.fullName shouldBe "some.NormalAnnotation"
      annotationNode.lineNumber shouldBe Some(6)
      annotationNode.columnNumber shouldBe Some(3)
    }

    "test annotation node parameter value" in {
      val Seq(paramValue: Annotation) = cpg.method.name("function").annotation.parameterAssign.value.isAnnotation.l
      paramValue.code shouldBe "@OtherAnnotation"
      paramValue.fullName shouldBe "some.OtherAnnotation"
      paramValue.order shouldBe 2
    }
  }

  "stacked annotations from wildcard imports" should {
    val cpg = code("""
				|import a.Specific;
				|import b.*;
				|
				|@Specific
				|@Wildcard1
				|@Wildcard2
        |class Foo { }
				|""".stripMargin)

    "have correct types set from imports" in {
      cpg.typeDecl.name("Foo").annotation.l match {
        case List(specific, wildcard1, wildcard2) =>
          specific.name shouldBe "Specific"
          specific.fullName shouldBe "a.Specific"
          specific.code shouldBe "@Specific"

          wildcard1.name shouldBe "Wildcard1"
          wildcard1.fullName shouldBe "b.Wildcard1"
          wildcard1.code shouldBe "@Wildcard1"
          wildcard1.lineNumber shouldBe Some(6)

          wildcard2.name shouldBe "Wildcard2"
          wildcard2.fullName shouldBe "b.Wildcard2"
          wildcard2.code shouldBe "@Wildcard2"
          wildcard2.lineNumber shouldBe Some(7)

        case result => fail(s"Expected 3 annotations for Foo but got $result")
      }
    }
  }

  "CPG for code with a custom annotation" should {
    val cpg = code("""
        |package mypak;
        |
        |import retrofit2.http.Body;
        |import retrofit2.http.POST;
        |
        |public interface User {
        |
        |    @POST("/name")
        |    public Call<Response> getUser(@Body Request request);
        |}
        |""".stripMargin)

    "contain an ANNOTATION node" in {
      cpg.all.collectAll[Annotation].codeExact("@POST(\"/name\")").size shouldBe 1
    }

    "the ANNOTATION node should have correct full name" in {
      cpg.all.collectAll[Annotation].codeExact("@POST(\"/name\")").fullName.head shouldBe "retrofit2.http.POST"
    }
  }
}
