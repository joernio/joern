package io.joern.php2cpg.querying

import io.joern.php2cpg.testfixtures.PhpCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class AnnotationTests extends PhpCode2CpgFixture {
  "annotations for nodes" should {
    "be populated" in {
      val cpg = code("""
          |<?php
          | #[Route("/api")]
          | class Foo {
          |   #[Route("/edit", name: "hello")]
          |   public function bar(#[SomeAttr] $pBar){}
          | }
          |>
          |""".stripMargin)

      inside(cpg.typeDecl("Foo").annotation.l) { case route :: Nil =>
        route.name shouldBe "Route"
        inside(route.astChildren.l) { case arg1 :: Nil =>
          arg1.code shouldBe "\"/api\""
        }
      }

      inside(cpg.method("bar").annotation.l) { case route :: Nil =>
        route.name shouldBe "Route"
        inside(route.astChildren.l) { case arg1 :: arg2 :: Nil =>
          arg1.code shouldBe "\"/edit\""
          arg2.code shouldBe "\"hello\""
        }
      }

      inside(cpg.parameter("pBar").annotation.l) { case someAttr :: Nil =>
        someAttr.name shouldBe "SomeAttr"
      }
    }
  }
}
