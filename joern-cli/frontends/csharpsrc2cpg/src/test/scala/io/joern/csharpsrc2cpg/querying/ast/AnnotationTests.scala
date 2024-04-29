package io.joern.csharpsrc2cpg.querying.ast

import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class AnnotationTests extends CSharpCode2CpgFixture {
  "annotations for methods" should {
    "have correct attributes" in {
      val cpg = code("""
          |using System;
          |
          |namespace Foo {
          | public class Bar {
          |   [Obsolete("Dep Method", false)]
          |   public static void Main() {}
          | }
          |}
          |""".stripMargin)

      inside(cpg.method("Main").annotation.l) { case obsolete :: Nil =>
        obsolete.code shouldBe "Obsolete(\"Dep Method\", false)"
        obsolete.name shouldBe "Obsolete"
        obsolete.lineNumber shouldBe Some(5)
        obsolete.columnNumber shouldBe Some(4)
        obsolete.fullName shouldBe "System.ObsoleteAttribute"
      }
    }
  }

  "annotations for classes" should {
    "have correct attributes" in {
      val cpg = code("""
          |using System;
          |
          |namespace Foo {
          | [Obsolete("Dep Class", false)]
          | public class Bar {
          |   public static void Main() {}
          | }
          |}
          |""".stripMargin)

      inside(cpg.typeDecl("Bar").annotation.l) { case obsolete :: Nil =>
        obsolete.code shouldBe "Obsolete(\"Dep Class\", false)"
        obsolete.name shouldBe "Obsolete"
        obsolete.lineNumber shouldBe Some(4)
        obsolete.columnNumber shouldBe Some(2)
        obsolete.fullName shouldBe "System.ObsoleteAttribute"
      }
    }

    "have correct code for Route attribute" in {
      val cpg = code("""
          |using System;
          |
          |namespace Foo {
          | [Route("api/v{version:number}/some/[controller]")]
          | public class Controller {
          |   public static void Main() {}
          | }
          |}
          |""".stripMargin)

      inside(cpg.typeDecl("Controller").annotation.l) { case route :: Nil =>
        route.code shouldBe "Route(\"api/v{version:number}/some/[controller]\")"
        route.name shouldBe "Route"
        route.fullName shouldBe "RouteAttribute"
      }

    }
  }

  "annotations for members" should {
    "have correct attributes" in {
      val cpg = code("""
          |using System;
          |
          |namespace Foo {
          | public class Bar {
          |   [Serializable] public string firstName;
          | }
          |}
          |""".stripMargin)

      inside(cpg.member("firstName").annotation.l) { case serializable :: Nil =>
        serializable.code shouldBe "Serializable"
        serializable.name shouldBe "Serializable"
        serializable.lineNumber shouldBe Some(5)
        serializable.columnNumber shouldBe Some(4)
        serializable.fullName shouldBe "System.SerializableAttribute"
      }
    }
  }
}
