package io.joern.php2cpg.querying

import io.joern.php2cpg.testfixtures.PhpCode2CpgFixture
import io.joern.php2cpg.parser.Domain
import io.shiftleft.semanticcpg.language.*

class TypeNodeTests extends PhpCode2CpgFixture {
  "TypeDecls with inheritsFrom types" should {
    val cpg = code("""<?php
     |namespace foo;
     |class A extends B implements C, D {}
     |""".stripMargin)

    "have type nodes created for the TypeDecl and inherited types" in {
      cpg.typ.fullName.toSet shouldEqual Set("ANY", "foo\\A", "foo\\B", "foo\\C", "foo\\D")
    }

    "have TypeDecl stubs created for inherited types" in {
      cpg.typeDecl.external.fullName.toSet shouldEqual Set("ANY", "foo\\B", "foo\\C", "foo\\D")
    }
  }

  "TypeDecls with inheritsFrom edges" should {
    val cpg = code("""<?php
        |interface Foo {}
        |class Bar {}
        |trait TraitA {}
        |trait TraitB {}
        |class Baz extends Bar implements Foo {
        |  use TraitA, TraitB;
        |}
        |""".stripMargin)

    "have baseTypeDecl for dynamic TYPE_DECL" in {
      cpg.typeDecl.name("Baz").baseTypeDecl.l.map(_.name) shouldBe List("Bar", "TraitA", "TraitB", "Foo")
      cpg.typeDecl.name("Baz").baseTypeDeclTransitive.l.map(_.name) shouldBe List("Bar", "TraitA", "TraitB", "Foo")
    }
  }

  "known types without explicit typeDecls" should {
    val cpg = code("""<?php
        |$x = 3;
        |""".stripMargin)

    "have corresponding type nodes created" in {
      cpg.typ.fullName.toSet shouldEqual Set("ANY", "int")
    }

    "have corresponding type decls created" in {
      cpg.typeDecl.external.fullName.toSet shouldEqual Set("ANY", "int")
    }
  }
}
