package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.{ModifierTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{
  Binding,
  Call,
  FieldIdentifier,
  Identifier,
  Literal,
  Method,
  Return
}
import io.shiftleft.semanticcpg.language.*

class LocalRecordTests extends JavaSrcCode2CpgFixture {

  "a simple local record" should {
    val cpg = code("""
        |package foo;
        |
        |class Foo {
        |    void enclosingMethod() {
        |        record LocalRecord(String value) {}
        |    }
        |}
        |""".stripMargin)

    @inline def localDecl = cpg.typeDecl.fullNameExact("foo.Foo.enclosingMethod:void().LocalRecord")

    "have only one LocalRecord typeDecl" in {
      cpg.typeDecl.name(".*LocalRecord.*").fullName.toSet shouldBe Set("foo.Foo.enclosingMethod:void().LocalRecord")
    }

    "have the correct code set" in {
      localDecl.code.l shouldBe List("record LocalRecord")
    }

    "extend java.lang.Record" in {
      localDecl.inheritsFromTypeFullName.l shouldBe List("java.lang.Record")
    }

    "have a private field for the record parameter" in {
      inside(localDecl.member.name("value").l) { case List(valueMember) =>
        valueMember.typeFullName shouldBe "java.lang.String"
        valueMember.modifier.modifierType.toSet shouldBe Set(ModifierTypes.PRIVATE, ModifierTypes.FINAL)
      }
    }

    "have a public accessor method for the record parameter" in {
      inside(localDecl.method.name("value").l) { case List(valueMethod) =>
        valueMethod.fullName shouldBe "foo.Foo.enclosingMethod:void().LocalRecord.value:java.lang.String()"

        inside(valueMethod.body.astChildren.l) { case List(returnStmt: Return) =>
          returnStmt.code shouldBe "return this.value"

          inside(returnStmt.astChildren.l) { case List(fieldAccess: Call) =>
            fieldAccess.name shouldBe Operators.fieldAccess
            fieldAccess.code shouldBe "this.value"
            fieldAccess.typeFullName shouldBe "java.lang.String"
          }
        }
      }
    }

    "have a default canonical constructor with a parameter for the record component" in {
      inside(localDecl.method.nameExact("<init>").l) { case List(constructor) =>
        constructor.fullName shouldBe "foo.Foo.enclosingMethod:void().LocalRecord.<init>:void(java.lang.String)"

        inside(constructor.parameter.l) { case List(thisParam, valueParam) =>
          thisParam.name shouldBe "this"
          thisParam.typeFullName shouldBe "foo.Foo.enclosingMethod:void().LocalRecord"
          thisParam.index shouldBe 0

          valueParam.name shouldBe "value"
          valueParam.typeFullName shouldBe "java.lang.String"
          valueParam.index shouldBe 1
        }

        inside(constructor.body.astChildren.l) { case List(valueAssign: Call) =>
          valueAssign.name shouldBe Operators.assignment
          valueAssign.code shouldBe "this.value = value"
        }
      }
    }

    "have the correct binding for the canonical constructor" in {
      inside(cpg.all.collectAll[Binding].nameExact("<init>").methodFullName(".*LocalRecord.*").l) {
        case List(initBinding) =>
          initBinding.methodFullName shouldBe "foo.Foo.enclosingMethod:void().LocalRecord.<init>:void(java.lang.String)"
          initBinding.bindingTypeDecl.fullName shouldBe "foo.Foo.enclosingMethod:void().LocalRecord"
          initBinding.signature shouldBe "void(java.lang.String)"
      }
    }
  }

  "a local record with multiple parameters" should {
    val cpg = code("""
        |package foo;
        |
        |class Foo {
        |    void enclosingMethod() {
        |        record Point(int x, int y) {}
        |    }
        |}
        |""".stripMargin)

    @inline def localDecl = cpg.typeDecl.fullNameExact("foo.Foo.enclosingMethod:void().Point")

    "have private fields for each record parameter" in {
      localDecl.member.name("x").typeFullName.l shouldBe List("int")
      localDecl.member.name("y").typeFullName.l shouldBe List("int")
    }

    "have accessor methods for each record parameter" in {
      localDecl.method.name("x").fullName.l shouldBe List("foo.Foo.enclosingMethod:void().Point.x:int()")
      localDecl.method.name("y").fullName.l shouldBe List("foo.Foo.enclosingMethod:void().Point.y:int()")
    }

    "have a canonical constructor with both parameters" in {
      inside(localDecl.method.nameExact("<init>").l) { case List(constructor) =>
        constructor.fullName shouldBe "foo.Foo.enclosingMethod:void().Point.<init>:void(int,int)"

        inside(constructor.parameter.l) { case List(thisParam, xParam, yParam) =>
          thisParam.name shouldBe "this"
          xParam.name shouldBe "x"
          xParam.typeFullName shouldBe "int"
          yParam.name shouldBe "y"
          yParam.typeFullName shouldBe "int"
        }
      }
    }
  }

  "a local record with a method and an explicit method" should {
    val cpg = code("""
        |package foo;
        |
        |class Foo {
        |    void enclosingMethod() {
        |        record LocalRecord(String value) {
        |            String upper() {
        |                return value.toUpperCase();
        |            }
        |        }
        |    }
        |}
        |""".stripMargin)

    @inline def localDecl = cpg.typeDecl.fullNameExact("foo.Foo.enclosingMethod:void().LocalRecord")

    "have the correct bindings for the explicit method" in {
      inside(cpg.all.collectAll[Binding].nameExact("upper").l) { case List(upperBinding) =>
        upperBinding.methodFullName shouldBe "foo.Foo.enclosingMethod:void().LocalRecord.upper:java.lang.String()"
        upperBinding.bindingTypeDecl.fullName shouldBe "foo.Foo.enclosingMethod:void().LocalRecord"
        upperBinding.signature shouldBe "java.lang.String()"
      }
    }

    "not generate an accessor for value since upper() is a different method" in {
      localDecl.method.name("value").l should not be empty
      localDecl.method.name("upper").l should not be empty
    }
  }

  "a local record with captures from enclosing scope" should {
    val cpg = code("""
        |package foo;
        |
        |class Foo {
        |    int capturedMember;
        |
        |    void enclosingMethod(int capturedParam) {
        |        int capturedLocal = 1;
        |        record LocalRecord(String value) {
        |            void usesCaptures() {
        |                sink(capturedParam, capturedLocal, capturedMember);
        |            }
        |        }
        |    }
        |}
        |""".stripMargin)

    @inline def localDecl = cpg.typeDecl.fullNameExact("foo.Foo.enclosingMethod:void(int).LocalRecord")

    "have private field for the record parameter" in {
      inside(localDecl.member.name("value").l) { case List(valueMember) =>
        valueMember.typeFullName shouldBe "java.lang.String"
        valueMember.modifier.modifierType.toSet shouldBe Set(ModifierTypes.PRIVATE, ModifierTypes.FINAL)
      }
    }

    "not have an outerClass member since records are implicitly static" in {
      localDecl.member.name("outerClass").isEmpty shouldBe true
    }

    "have members for captured variables" in {
      localDecl.member.name("capturedParam").typeFullName.l shouldBe List("int")
      localDecl.member.name("capturedLocal").typeFullName.l shouldBe List("int")
    }

    "have a canonical constructor with record params and capture params" in {
      inside(localDecl.method.nameExact("<init>").l) { case List(constructor) =>
        constructor.fullName shouldBe "foo.Foo.enclosingMethod:void(int).LocalRecord.<init>:void(java.lang.String)"

        val params = constructor.parameter.l
        params.map(_.name) should contain("this")
        params.map(_.name) should contain("value")
        params.map(_.name) should contain("capturedParam")
        params.map(_.name) should contain("capturedLocal")
      }
    }
  }

  "a local record in a static context" should {
    val cpg = code("""
        |package foo;
        |
        |class Foo {
        |    static int staticMember;
        |
        |    static void enclosingMethod(int capturedParam) {
        |        int capturedLocal = 1;
        |        record LocalRecord(String value) {
        |            void usesCaptures() {
        |                sink(capturedParam, capturedLocal);
        |            }
        |
        |            void staticAccess() {
        |                sink(staticMember);
        |            }
        |        }
        |    }
        |}
        |""".stripMargin)

    @inline def localDecl = cpg.typeDecl.fullNameExact("foo.Foo.enclosingMethod:void(int).LocalRecord")

    "not have an outerClass member" in {
      localDecl.member.name("outerClass").isEmpty shouldBe true
    }

    "have members for captures" in {
      localDecl.member.name("capturedParam").typeFullName.l shouldBe List("int")
      localDecl.member.name("capturedLocal").typeFullName.l shouldBe List("int")
    }

    "not have a member for the static member in the outer class" in {
      localDecl.member.name("staticMember").isEmpty shouldBe true
    }

    "have a private field for the record parameter" in {
      inside(localDecl.member.name("value").l) { case List(valueMember) =>
        valueMember.typeFullName shouldBe "java.lang.String"
        valueMember.modifier.modifierType.toSet shouldBe Set(ModifierTypes.PRIVATE, ModifierTypes.FINAL)
      }
    }
  }

  "a local record with a compact constructor" should {
    val cpg = code("""
        |package foo;
        |
        |class Foo {
        |    void enclosingMethod() {
        |        record LocalRecord(String value) {
        |            public LocalRecord {
        |                System.out.println(value);
        |            }
        |        }
        |    }
        |}
        |""".stripMargin)

    @inline def localDecl = cpg.typeDecl.fullNameExact("foo.Foo.enclosingMethod:void().LocalRecord")

    "have the correct constructor with assignment before body" in {
      inside(localDecl.method.nameExact("<init>").l) { case List(constructor) =>
        constructor.fullName shouldBe "foo.Foo.enclosingMethod:void().LocalRecord.<init>:void(java.lang.String)"

        inside(constructor.parameter.l) { case List(thisParam, valueParam) =>
          thisParam.name shouldBe "this"
          thisParam.typeFullName shouldBe "foo.Foo.enclosingMethod:void().LocalRecord"

          valueParam.name shouldBe "value"
          valueParam.typeFullName shouldBe "java.lang.String"
        }

        inside(constructor.body.astChildren.l) { case List(valueAssign: Call, printlnCall: Call) =>
          valueAssign.name shouldBe Operators.assignment
          valueAssign.code shouldBe "this.value = value"

          printlnCall.name shouldBe "println"
          printlnCall.code shouldBe "System.out.println(value)"
        }
      }
    }
  }
}
