package io.joern.php2cpg.querying

import io.joern.php2cpg.testfixtures.PhpCode2CpgFixture
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.{ModifierTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Identifier, Literal, Local, Member, Method}
import io.shiftleft.semanticcpg.language._
import io.shiftleft.codepropertygraph.generated.nodes.Block

class TypeNodeTests extends PhpCode2CpgFixture {

  private def isArtificialType(typeFullName: String): Boolean = {
    typeFullName == "ANY" || typeFullName.endsWith(".php:<global>")
  }

  "TypeDecls with inheritsFrom types" should {
    val cpg = code("""<?php
     |namespace foo;
     |class A extends B implements C, D {}
     |""".stripMargin)

    "have type nodes created for the TypeDecl and inherited types" in {
      cpg.typ.fullName.filterNot(isArtificialType).toSet shouldEqual Set("foo\\A", "foo\\B", "foo\\C", "foo\\D")
    }

    "have TypeDecl stubs created for inherited types" in {
      cpg.typeDecl.external.fullName.filterNot(isArtificialType).toSet shouldEqual Set("foo\\B", "foo\\C", "foo\\D")
    }
  }

  "known types without explicit typeDecls" should {
    val cpg = code("""<?php\n$x = 3;""")

    "have corresponding type nodes created" in {
      println(cpg.literal.toList)
      cpg.typ.fullName.filterNot(isArtificialType).toSet shouldEqual Set("int")
    }

    "have corresponding type decls created" in {
      cpg.typeDecl.external.fullName.filterNot(isArtificialType).toSet shouldEqual Set("int")
    }
  }
}
