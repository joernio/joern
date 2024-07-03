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
      cpg.typ.fullName.toSet shouldEqual Set("ANY", "foo\\A", "foo\\B", "foo\\C", "foo\\D")
    }

    "have TypeDecl stubs created for inherited types" in {
      cpg.typeDecl.external.fullName.toSet shouldEqual Set("ANY", "foo\\B", "foo\\C", "foo\\D")
    }
  }

  "known types without explicit typeDecls" should {
    val cpg = code("""<?php
        |$x = 3;
        |""".stripMargin)

    "have corresponding type nodes created" in {
      println(cpg.literal.toList)
      cpg.typ.fullName.toSet shouldEqual Set("ANY", "int")
    }

    "have corresponding type decls created" in {
      cpg.typeDecl.external.fullName.toSet shouldEqual Set("ANY", "int")
    }
  }
}
