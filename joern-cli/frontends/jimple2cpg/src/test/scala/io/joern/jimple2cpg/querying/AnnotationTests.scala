package io.joern.jimple2cpg.querying

import io.joern.jimple2cpg.testfixtures.JimpleCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Annotation, AnnotationLiteral, ArrayInitializer}
import io.shiftleft.semanticcpg.language._

class AnnotationTests extends JimpleCode2CpgFixture {

  "annotation type" should {

    lazy val cpg: Cpg = code("""
        |@interface NormalAnnotation {
        |    public String value() default "";
        |}
        |@NormalAnnotation("annotation")
        |class SomeClass {}
        |""".stripMargin).cpg

    "test annotation node properties" in {
      val annotationNode = cpg.typeDecl("SomeClass").annotation.head
      annotationNode.code shouldBe """@NormalAnnotation(value = "annotation")"""
      annotationNode.name shouldBe "NormalAnnotation"
      annotationNode.fullName shouldBe "NormalAnnotation"
    }
  }

  "annotation type method 1" should {

    lazy val cpg: Cpg = code("""import java.lang.annotation.*;
        |
        |@Retention(RetentionPolicy.RUNTIME)
        |@Target(ElementType.METHOD)
        |@interface NormalAnnotation {
        |
        | public String value() default "";
        |
        |}
        |
        |class SomeClass {
        |
        |  @NormalAnnotation(value = "classAnnotation")
        |  void function() {
        |
        |  }
        |}
        |""".stripMargin).cpg

    "test annotation node properties" in {
      val annotationNode = cpg.method.name("function").annotation.head
      annotationNode.code shouldBe "@NormalAnnotation(value = \"classAnnotation\")"
      annotationNode.name shouldBe "NormalAnnotation"
      annotationNode.fullName shouldBe "NormalAnnotation"
    }

    "test annotation node parameter assignment child" in {
      val Seq(paramAssign) = cpg.method.name("function").annotation.parameterAssign.l
      paramAssign.code shouldBe "value = \"classAnnotation\""
    }

    "test annotation node parameter child" in {
      val Seq(param) = cpg.method.name("function").annotation.parameterAssign.parameter.l
      param.code shouldBe "value"
    }

    "test annotation node parameter value" in {
      val Seq(paramValue) = cpg.method.name("function").annotation.parameterAssign.value.l
      paramValue.code shouldBe "\"classAnnotation\""
      paramValue.argumentIndex shouldBe 2
    }

  }

  "annotation type method 2" should {

    lazy val cpg: Cpg = code("""import java.lang.annotation.*;
        |
        |@Retention(RetentionPolicy.RUNTIME)
        |@Target(ElementType.METHOD)
        |@interface MarkerAnnotation { }
        |
        |class SomeClass {
        |
        |  @MarkerAnnotation()
        |  void function() {
        |
        |  }
        |}
        |""".stripMargin).cpg

    "test annotation node properties" in {
      val annotationNode = cpg.method.name("function").annotation.head
      annotationNode.code shouldBe "@MarkerAnnotation()"
      annotationNode.name shouldBe "MarkerAnnotation"
      annotationNode.fullName shouldBe "MarkerAnnotation"
    }

    "test annotation node parameter assignment child" in {
      cpg.method.name("function").annotation.parameterAssign.isEmpty shouldBe true
    }

  }

  "annotation type constructor" should {

    lazy val cpg: Cpg = code("""import java.lang.annotation.*;
        |
        |@Retention(RetentionPolicy.RUNTIME)
        |@Target(ElementType.CONSTRUCTOR)
        |@interface MarkerAnnotation { }
        |
        |class SomeClass {
        |
        |  @MarkerAnnotation()
        |  public SomeClass() {
        |
        |  }
        |}
        |""".stripMargin).cpg

    "test annotation node properties" in {
      val annotationNode = cpg.method.fullNameExact("SomeClass.<init>:void()").annotation.head
      annotationNode.code shouldBe "@MarkerAnnotation()"
      annotationNode.name shouldBe "MarkerAnnotation"
      annotationNode.fullName shouldBe "MarkerAnnotation"
    }

    "test annotation node parameter assignment child" in {
      cpg.method.name("function").annotation.parameterAssign.isEmpty shouldBe true
    }

  }

  "annotation type parameter" should {

    lazy val cpg: Cpg = code("""import java.lang.annotation.*;
        |
        |@Retention(RetentionPolicy.RUNTIME)
        |@Target(ElementType.PARAMETER)
        |@interface MarkerAnnotation { }
        |
        |class SomeClass {
        |
        |  void function(@MarkerAnnotation int x, int y) {
        |
        |  }
        |}
        |""".stripMargin).cpg

    "test annotation node properties" in {
      val annotationNode = cpg.method.name("function").parameter.name("x").annotation.head
      annotationNode.code shouldBe "@MarkerAnnotation()"
      annotationNode.name shouldBe "MarkerAnnotation"
      annotationNode.fullName shouldBe "MarkerAnnotation"
    }

  }

  "annotation type field" should {

    lazy val cpg: Cpg = code("""import java.lang.annotation.*;
        |
        |@Retention(RetentionPolicy.RUNTIME)
        |@Target(ElementType.FIELD)
        |@interface MarkerAnnotation { }
        |
        |class SomeClass {
        |  @MarkerAnnotation int x;
        |}
        |""".stripMargin).cpg

    "test annotation node properties" in {
      val annotationNode = cpg.typeDecl.name("SomeClass").member.name("x").annotation.head
      annotationNode.code shouldBe "@MarkerAnnotation()"
      annotationNode.name shouldBe "MarkerAnnotation"
      annotationNode.fullName shouldBe "MarkerAnnotation"
    }

  }

  "annotation test value 1" should {

    lazy val cpg: Cpg = code("""import java.lang.annotation.*;
        |
        |@Retention(RetentionPolicy.RUNTIME)
        |@Target(ElementType.METHOD)
        |@interface NormalAnnotation {
        |
        | public String[] value() default {};
        |
        |}
        |
        |class SomeClass {
        |
        |  @NormalAnnotation(value = {"aaa", "bbb"})
        |  void function() {
        |
        |  }
        |}
        |""".stripMargin).cpg

    "test annotation node properties" in {
      val annotationNode = cpg.method.name("function").annotation.head
      annotationNode.code shouldBe "@NormalAnnotation(value = {\"aaa\", \"bbb\"})"
      annotationNode.name shouldBe "NormalAnnotation"
      annotationNode.fullName shouldBe "NormalAnnotation"
    }

    "test annotation node parameter assignment child" in {
      val Seq(paramAssign) = cpg.method.name("function").annotation.parameterAssign.l
      paramAssign.code shouldBe "value = {\"aaa\", \"bbb\"}"
      paramAssign.order shouldBe 1
    }

    "test annotation node parameter child" in {
      val Seq(param) = cpg.method.name("function").annotation.parameterAssign.parameter.l
      param.code shouldBe "value"
      param.order shouldBe 1
    }

    "test annotation node parameter value" in {
      val Seq(paramValue: ArrayInitializer) = cpg.method.name("function").annotation.parameterAssign.value.l: @unchecked
      paramValue.code shouldBe "{\"aaa\", \"bbb\"}"
      paramValue.order shouldBe 2
      paramValue.argumentIndex shouldBe 2
    }

    "test annotation node array initializer children" in {
      val children = cpg.method.name("function").annotation.parameterAssign.value.astChildren.isExpression.s
      children.find(_.code == "\"aaa\"").map(_.order) shouldBe Some(1)
      children.find(_.code == "\"aaa\"").map(_.argumentIndex) shouldBe Some(1)
      children.find(_.code == "\"bbb\"").map(_.order) shouldBe Some(2)
      children.find(_.code == "\"bbb\"").map(_.argumentIndex) shouldBe Some(2)
    }

  }

  "annotation test value 2" should {

    lazy val cpg: Cpg = code("""import java.lang.annotation.*;
        |
        |@Retention(RetentionPolicy.RUNTIME)
        |@Target(ElementType.METHOD)
        |@interface NormalAnnotation {
        |
        | public int value() default 0;
        |
        |}
        |
        |class SomeClass {
        |
        |  @NormalAnnotation(value = 2)
        |  void function() {
        |
        |  }
        |}
        |""".stripMargin).cpg

    "test annotation node properties" in {
      val annotationNode = cpg.method.name("function").annotation.head
      annotationNode.code shouldBe "@NormalAnnotation(value = 2)"
      annotationNode.name shouldBe "NormalAnnotation"
      annotationNode.fullName shouldBe "NormalAnnotation"
    }

    "test annotation node parameter value" in {
      val Seq(paramValue: AnnotationLiteral) =
        cpg.method.name("function").annotation.parameterAssign.value.l: @unchecked
      paramValue.code shouldBe "2"
      paramValue.order shouldBe 2
      paramValue.argumentIndex shouldBe 2
    }

  }

  "annotation test value 3" should {

    lazy val cpg: Cpg = code("""import java.lang.annotation.*;
        |
        |@Retention(RetentionPolicy.RUNTIME)
        |@Target(ElementType.METHOD)
        |@interface NormalAnnotation {
        |
        | public OtherAnnotation value();
        |
        |}
        |
        |@Retention(RetentionPolicy.RUNTIME)
        |@Target(ElementType.METHOD)
        |@interface OtherAnnotation { }
        |
        |class SomeClass {
        |
        |  @NormalAnnotation(value = @OtherAnnotation)
        |  void function() {
        |
        |  }
        |}
        |""".stripMargin).cpg

    "test annotation node properties" in {
      val annotationNode = cpg.method.name("function").annotation.head
      annotationNode.code shouldBe "@NormalAnnotation(value = @OtherAnnotation())"
      annotationNode.name shouldBe "NormalAnnotation"
      annotationNode.fullName shouldBe "NormalAnnotation"
    }

    "test annotation node parameter value" in {
      val Seq(paramValue: Annotation) = cpg.method.name("function").annotation.parameterAssign.value.l: @unchecked
      paramValue.code shouldBe "@OtherAnnotation()"
      paramValue.fullName shouldBe "OtherAnnotation"
      paramValue.order shouldBe 2
    }

  }

}
