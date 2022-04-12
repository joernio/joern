package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCodeToCpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.{Annotation, AnnotationLiteral, ArrayInitializer}
import io.shiftleft.semanticcpg.language._

class AnnotationTestMethod1 extends JavaSrcCodeToCpgFixture {
  override val code =
    """
      |import some.NormalAnnotation;
      |public class SomeClass {
      |
      |  @NormalAnnotation(value = "classAnnotation")
      |  void function() {
      |
      |  }
      |}
      |""".stripMargin
  "test annotation node properties" in {
    val annotationNode = cpg.method.name("function").annotation.head
    annotationNode.code shouldBe "@NormalAnnotation(value = \"classAnnotation\")"
    annotationNode.name shouldBe "NormalAnnotation"
    annotationNode.fullName shouldBe "some.NormalAnnotation"
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

class AnnotationTestMethod2 extends JavaSrcCodeToCpgFixture {
  override val code =
    """
      |import some.SingleAnnotation;
      |public class SomeClass {
      |
      |  @SingleAnnotation("classAnnotation")
      |  void function() {
      |
      |  }
      |}
      |""".stripMargin
  "test annotation node properties" in {
    val annotationNode = cpg.method.name("function").annotation.head
    annotationNode.code shouldBe "@SingleAnnotation(\"classAnnotation\")"
    annotationNode.name shouldBe "SingleAnnotation"
    annotationNode.fullName shouldBe "some.SingleAnnotation"
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
  }
}

class AnnotationTestMethod3 extends JavaSrcCodeToCpgFixture {
  override val code =
    """
      |import some.MarkerAnnotation;
      |public class SomeClass {
      |
      |  @MarkerAnnotation()
      |  void function() {
      |
      |  }
      |}
      |""".stripMargin
  "test annotation node properties" in {
    val annotationNode = cpg.method.name("function").annotation.head
    annotationNode.code shouldBe "@MarkerAnnotation()"
    annotationNode.name shouldBe "MarkerAnnotation"
    annotationNode.fullName shouldBe "some.MarkerAnnotation"
  }

  "test annotation node parameter assignment child" in {
    cpg.method.name("function").annotation.parameterAssign.isEmpty shouldBe true
  }
}

class AnnotationTestConstructor extends JavaSrcCodeToCpgFixture {
  override val code =
    """
      |import some.MarkerAnnotation;
      |public class SomeClass {
      |
      |  @MarkerAnnotation()
      |  public SomeClass() {
      |
      |  }
      |}
      |""".stripMargin
  "test annotation node properties" in {
    import scala.jdk.CollectionConverters._
    val annotationNode = cpg.method.nameExact("SomeClass").annotation.head
    annotationNode.code shouldBe "@MarkerAnnotation()"
    annotationNode.name shouldBe "MarkerAnnotation"
    annotationNode.fullName shouldBe "some.MarkerAnnotation"
  }

  "test annotation node parameter assignment child" in {
    cpg.method.name("function").annotation.parameterAssign.isEmpty shouldBe true
  }
}

class AnnotationTestParameter extends JavaSrcCodeToCpgFixture {
  override val code =
    """
      |import some.MarkerAnnotation;
      |public class SomeClass {
      |
      |  void function(@MarkerAnnotation int x) {
      |
      |  }
      |}
      |""".stripMargin
  "test annotation node properties" in {
    val annotationNode = cpg.method.name("function").parameter.name("x").annotation.head
    annotationNode.code shouldBe "@MarkerAnnotation"
    annotationNode.name shouldBe "MarkerAnnotation"
    annotationNode.fullName shouldBe "some.MarkerAnnotation"
  }
}

class AnnotationTestField extends JavaSrcCodeToCpgFixture {
  override val code =
    """
      |import some.MarkerAnnotation;
      |public class SomeClass {
      |  @MarkerAnnotation int x;
      |}
      |""".stripMargin
  "test annotation node properties" in {
    val annotationNode = cpg.typeDecl.name("SomeClass").member.name("x").annotation.head
    annotationNode.code shouldBe "@MarkerAnnotation"
    annotationNode.name shouldBe "MarkerAnnotation"
    annotationNode.fullName shouldBe "some.MarkerAnnotation"
  }
}

class AnnotationTestValue1 extends JavaSrcCodeToCpgFixture {
  override val code =
    """
      |import some.NormalAnnotation;
      |public class SomeClass {
      |
      |  @NormalAnnotation(value = 2)
      |  void function() {
      |
      |  }
      |}
      |""".stripMargin
  "test annotation node properties" in {
    val annotationNode = cpg.method.name("function").annotation.head
    annotationNode.code shouldBe "@NormalAnnotation(value = 2)"
    annotationNode.name shouldBe "NormalAnnotation"
    annotationNode.fullName shouldBe "some.NormalAnnotation"
  }

  "test annotation node parameter value" in {
    val Seq(paramValue: AnnotationLiteral) = cpg.method.name("function").annotation.parameterAssign.value.l
    paramValue.code shouldBe "2"
    paramValue.order shouldBe 2
    paramValue.argumentIndex shouldBe 2
  }
}

class AnnotationTestValue2 extends JavaSrcCodeToCpgFixture {
  override val code =
    """
      |import some.NormalAnnotation;
      |public class SomeClass {
      |
      |  @NormalAnnotation(value = {"aaa", "bbb"})
      |  void function() {
      |
      |  }
      |}
      |""".stripMargin
  "test annotation node properties" in {
    val annotationNode = cpg.method.name("function").annotation.head
    annotationNode.code shouldBe "@NormalAnnotation(value = { \"aaa\", \"bbb\" })"
    annotationNode.name shouldBe "NormalAnnotation"
    annotationNode.fullName shouldBe "some.NormalAnnotation"
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
    val Seq(paramValue: ArrayInitializer) = cpg.method.name("function").annotation.parameterAssign.value.l
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

class AnnotationTestValue3 extends JavaSrcCodeToCpgFixture {
  override val code =
    """
      |import some.NormalAnnotation;
      |import some.OtherAnnotation;
      |public class SomeClass {
      |
      |  @NormalAnnotation(value = @OtherAnnotation)
      |  void function() {
      |
      |  }
      |}
      |""".stripMargin
  "test annotation node properties" in {
    val annotationNode = cpg.method.name("function").annotation.head
    annotationNode.code shouldBe "@NormalAnnotation(value = @OtherAnnotation)"
    annotationNode.name shouldBe "NormalAnnotation"
    annotationNode.fullName shouldBe "some.NormalAnnotation"
  }

  "test annotation node parameter value" in {
    val Seq(paramValue: Annotation) = cpg.method.name("function").annotation.parameterAssign.value.l
    paramValue.code shouldBe "@OtherAnnotation"
    paramValue.fullName shouldBe "some.OtherAnnotation"
    paramValue.order shouldBe 2
  }
}
