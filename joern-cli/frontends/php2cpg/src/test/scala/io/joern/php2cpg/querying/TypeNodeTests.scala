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
      cpg.typ.fullName.toSet shouldEqual Set(
        "ANY",
        "foo\\A",
        "foo\\B",
        "foo\\C",
        s"foo\\B${Domain.MetaTypeDeclExtension}",
        s"foo\\D${Domain.MetaTypeDeclExtension}",
        "foo\\D",
        s"foo\\A${Domain.MetaTypeDeclExtension}",
        s"foo\\C${Domain.MetaTypeDeclExtension}"
      )
    }

    "have TypeDecl stubs created for inherited types" in {
      cpg.typeDecl.external.fullName.toSet shouldEqual Set(
        "ANY",
        "foo\\B",
        s"foo\\B${Domain.MetaTypeDeclExtension}",
        "foo\\C",
        s"foo\\C${Domain.MetaTypeDeclExtension}",
        "foo\\D",
        s"foo\\D${Domain.MetaTypeDeclExtension}"
      )
    }
  }

  "TypeDecls with inheritsFrom edges" should {
    val cpg = code("""<?php
        |interface Foo {}
        |class Bar {}
        |class Baz extends Bar implements Foo {}
        |""".stripMargin)

    "have baseTypeDecl for dynamic TYPE_DECL" in {
      cpg.typeDecl.name("Baz").baseTypeDecl.l.map(_.name) shouldBe List("Bar", "Foo")
      cpg.typeDecl.name("Baz").baseTypeDeclTransitive.l.map(_.name) shouldBe List("Bar", "Foo")
    }

    "have baseTypeDecl steps for meta TYPE_DECL (<metaclass>)" in {
      cpg.typeDecl.name(s"Baz${Domain.MetaTypeDeclExtension}").baseTypeDecl.l.map(_.name) shouldBe List(
        s"Bar${Domain.MetaTypeDeclExtension}",
        s"Foo${Domain.MetaTypeDeclExtension}"
      )
      cpg.typeDecl.name(s"Baz${Domain.MetaTypeDeclExtension}").baseTypeDeclTransitive.l.map(_.name) shouldBe List(
        s"Bar${Domain.MetaTypeDeclExtension}",
        s"Foo${Domain.MetaTypeDeclExtension}"
      )
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
