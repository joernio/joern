package io.joern.php2cpg.querying

import io.joern.php2cpg.testfixtures.PhpCode2CpgFixture
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.{ModifierTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Identifier, Literal, Local, Member, Method}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.codepropertygraph.generated.nodes.Block

class TypeNodeTests extends PhpCode2CpgFixture {
  "TypeDecls with inheritsFrom types" should {
    val cpg = code("""<?php
     |namespace foo;
     |class A extends B implements C, D {}
     |""".stripMargin)

    "have type nodes created for the TypeDecl and inherited types" in {
      cpg.typ.fullName.toSet shouldEqual Set(
        "ANY",
        "foo\\B",
        "foo\\B.<class>",
        "foo\\C",
        "foo\\D",
        "foo\\A",
        "foo\\A.<class>",
        "foo\\C.<class>",
        "foo\\D.<class>"
      )
    }

    "have TypeDecl stubs created for inherited types" in {
      cpg.typeDecl.external.fullName.toSet shouldEqual Set(
        "ANY",
        "foo\\B",
        "foo\\B.<class>",
        "foo\\C",
        "foo\\C.<class>",
        "foo\\D",
        "foo\\D.<class>"
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

    "have baseTypeDecl steps for meta TYPE_DECL (.<class>)" in {
      cpg.typeDecl.name("Baz.<class>").baseTypeDecl.l.map(_.name) shouldBe List("Bar.<class>", "Foo.<class>")
      cpg.typeDecl.name("Baz.<class>").baseTypeDeclTransitive.l.map(_.name) shouldBe List("Bar.<class>", "Foo.<class>")
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
