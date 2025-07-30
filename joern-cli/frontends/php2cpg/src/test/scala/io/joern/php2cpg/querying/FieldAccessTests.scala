package io.joern.php2cpg.querying

import io.joern.php2cpg.testfixtures.PhpCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.{Call, FieldIdentifier, Identifier}
import io.shiftleft.semanticcpg.language.*

class FieldAccessTests extends PhpCode2CpgFixture {
  "a property fetch with an arbitrary expression rhs should be represented as an index access" in {
    val cpg = code("""<?php
      |$obj->{foo()};
      |""".stripMargin)

    inside(cpg.call.name(".*operator.*").l) { case List(indexAccess) =>
      indexAccess.name shouldBe Operators.indexAccess
      indexAccess.code shouldBe "$obj->{foo()}"
      inside(indexAccess.argument.l) { case List(objIdentifier: Identifier, fooCall: Call) =>
        objIdentifier.name shouldBe "obj"
        fooCall.name shouldBe "foo"
      }
    }
  }

  "simple property fetches should be represented as normal field accesses" in {
    val cpg = code("""<?php
      |$obj->field;
      |""".stripMargin)

    inside(cpg.call.l) { case List(fieldAccess) =>
      fieldAccess.name shouldBe Operators.fieldAccess
      fieldAccess.methodFullName shouldBe Operators.fieldAccess
      fieldAccess.code shouldBe "$obj->field"
      fieldAccess.lineNumber shouldBe Some(2)

      inside(fieldAccess.argument.l) { case List(objIdentifier: Identifier, fieldIdentifier: FieldIdentifier) =>
        objIdentifier.name shouldBe "obj"
        objIdentifier.code shouldBe "$obj"
        objIdentifier.lineNumber shouldBe Some(2)

        fieldIdentifier.canonicalName shouldBe "field"
        fieldIdentifier.code shouldBe "field"
        fieldIdentifier.lineNumber shouldBe Some(2)
      }
    }
  }

  "property fetches with expr args should be represented as an arbitrary field access" in {
    val cpg = code("""<?php
      |$$obj->$field;
      |""".stripMargin)

    inside(cpg.call.l) { case List(fieldAccess) =>
      fieldAccess.name shouldBe Operators.fieldAccess
      fieldAccess.methodFullName shouldBe Operators.fieldAccess
      fieldAccess.code shouldBe "$$obj->$field"
      fieldAccess.lineNumber shouldBe Some(2)

      inside(fieldAccess.argument.l) { case List(objIdentifier: Identifier, fieldIdentifier: FieldIdentifier) =>
        objIdentifier.name shouldBe "obj"
        objIdentifier.code shouldBe "$$obj"
        objIdentifier.lineNumber shouldBe Some(2)

        fieldIdentifier.canonicalName shouldBe "field"
        fieldIdentifier.code shouldBe "$field"
        fieldIdentifier.lineNumber shouldBe Some(2)
      }
    }
  }

  "nullsafe property fetches should be represented as normal field accesses with the correct code" in {
    val cpg = code("""<?php
      |$obj?->field;
      |""".stripMargin)

    inside(cpg.call.l) { case List(fieldAccess) =>
      fieldAccess.name shouldBe Operators.fieldAccess
      fieldAccess.methodFullName shouldBe Operators.fieldAccess
      fieldAccess.code shouldBe "$obj?->field"
      fieldAccess.lineNumber shouldBe Some(2)

      inside(fieldAccess.argument.l) { case List(objIdentifier: Identifier, fieldIdentifier: FieldIdentifier) =>
        objIdentifier.name shouldBe "obj"
        objIdentifier.code shouldBe "$obj"
        objIdentifier.lineNumber shouldBe Some(2)

        fieldIdentifier.canonicalName shouldBe "field"
        fieldIdentifier.code shouldBe "field"
        fieldIdentifier.lineNumber shouldBe Some(2)
      }
    }
  }

  "static property fetches should be represented as normal field accesses with the correct code" in {
    val cpg = code("""<?php
      |className::$field;
      |""".stripMargin)

    inside(cpg.call.l) { case List(fieldAccess) =>
      fieldAccess.name shouldBe Operators.fieldAccess
      fieldAccess.methodFullName shouldBe Operators.fieldAccess
      fieldAccess.code shouldBe "className::$field"
      fieldAccess.lineNumber shouldBe Some(2)

      inside(fieldAccess.argument.l) { case List(classIdentifier: Identifier, fieldIdentifier: FieldIdentifier) =>
        classIdentifier.name shouldBe "className"
        classIdentifier.code shouldBe "className"
        classIdentifier.lineNumber shouldBe Some(2)

        fieldIdentifier.canonicalName shouldBe "field"
        fieldIdentifier.code shouldBe "$field"
        fieldIdentifier.lineNumber shouldBe Some(2)
      }
    }
  }

}
