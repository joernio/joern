package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{
  Block,
  Call,
  ControlStructure,
  FieldIdentifier,
  Identifier,
  Local,
  TypeRef
}
import io.shiftleft.semanticcpg.language.*

class PatternTests extends JavaSrcCode2CpgFixture {

  "patterns in binary expressions" when {
    "a variable is introduced to the RHS of an && expression" should {
      "be represented correctly" in {
        ???
      }
    }

    "a variable is introduced to the RHS of an || expression" should {
      "be represented correctly" in {
        ???
      }
    }

    "a variable is introduced to the RHS of an && expression, mutated and introduced to the body of an if" should {
      val cpg = code("""
          |class Test {
          |  static void test(Object o) {
          |    if (o instanceof String value && (value = "FoO").length() < 5) {
          |      System.out.println("value is " + value);
          |    }
          |  }
          |}
          |""".stripMargin)

      "be represented correctly" in {
        ???
      }
    }

    "a variable is introduced to the RHS of an || expression, mutated and introduced by an if" should {
      val cpg = code("""
          |class Test {
          |  static void test(Object o) {
          |    if (!(o instanceof String value) || (value = "MALICIOUS").length() < 5) {
          |      return;
          |    }
          |    System.out.println(value);
          |  }
          |}
          |""".stripMargin)

      "be represented correctly" in {
        ???
      }
    }
  }

  "patterns in ternary expressions" when {
    "a variable is introduced to the then expression" should {
      "be represented correctly" in {
        ???
      }
    }

    "a variable is introduced to the else expression" should {
      "be represented correctly" in {
        ???
      }
    }

  }

  "patterns in if statements" when {
    "a variable is introduced to the then block" should {
      val cpg = code("""
          |class Foo {
          |  Integer s;
          |  void foo(Object o) {
          |    if (o instanceof String s) {
          |      sink(s);
          |    }
          |  }
          |  static void sink(Object o) {}
          |}
          |""".stripMargin)

      "parse" in {
        cpg.identifier.name("s").nonEmpty shouldBe true
      }

      "create the s local in the then block" in {
        inside(cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).astChildren.l) {
          case List(_: Call, thenBlock: Block) =>
            thenBlock.ast.isLocal.name("s").typeFullName.l shouldBe List("java.lang.String")
        }
      }

      "create the s assignment in the then block" in {
        inside(cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).astChildren.l) {
          case List(_: Call, thenBlock: Block) =>
            thenBlock.ast.isCall
              .nameExact(Operators.assignment)
              .where(_.argument.isIdentifier.name("s"))
              .code
              .l shouldBe List("s = (String) o")
        }
      }

      "create an identifier referring to the s local as the argument for sink" in {
        inside(cpg.call.name("sink").argument.l) { case List(sIdentifier: Identifier) =>
          sIdentifier.name shouldBe "s"
          sIdentifier.typeFullName shouldBe "java.lang.String"
          sIdentifier.refsTo.l shouldBe cpg.local.name("s").l
        }
      }
    }

    "a variable is introduced to the else block" should {
      val cpg = code("""
          |class Foo {
          |  Integer s;
          |  void foo(Object o) {
          |    if (!(o instanceof String s)) { 
          |    } else {
          |      sink(s);
          |    }
          |  }
          |  static void sink(Object o) {}
          |}
          |""".stripMargin)

      "parse" in {
        cpg.identifier.name("s").nonEmpty shouldBe true
      }

      "create the s local in the else block" in {
        inside(cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).astChildren.l) {
          case List(_: Call, _: Block, elseBlock: Block) =>
            elseBlock.ast.isLocal.name("s").typeFullName.l shouldBe List("java.lang.String")
        }
      }

      "create the s assignment in the then block" in {
        inside(cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).astChildren.l) {
          case List(_: Call, _: Block, elseBlock: Block) =>
            elseBlock.ast.isCall
              .nameExact(Operators.assignment)
              .where(_.argument.isIdentifier.name("s"))
              .code
              .l shouldBe List("s = (String) o")
        }
      }

      "create an identifier referring to the s local as the argument for sink" in {
        inside(cpg.call.name("sink").argument.l) { case List(sIdentifier: Identifier) =>
          sIdentifier.name shouldBe "s"
          sIdentifier.typeFullName shouldBe "java.lang.String"
          sIdentifier.refsTo.l shouldBe cpg.local.name("s").l
        }
      }
    }

    "a variable is introduced to the surrounding scope" should {
      val cpg = code("""
          |class Foo {
          |  Integer s;
          |  void foo(Object o) {
          |    if (!(o instanceof String s)) { 
          |      return;
          |    }
          |    sink(s);
          |  }
          |  static void sink(Object o) {}
          |}
          |""".stripMargin)

      "parse" in {
        cpg.identifier.name("s").nonEmpty shouldBe true
      }

      "create the s local after the if statement" in {
        inside(cpg.method.name("foo").body.astChildren.l) {
          case List(_: ControlStructure, sLocal: Local, _: Call, _: Call) =>
            sLocal.name shouldBe "s"
            sLocal.typeFullName shouldBe "java.lang.String"
        }
      }

      "create the s assignment before the if statement" in {
        inside(cpg.method.name("foo").body.astChildren.l) {
          case List(_: ControlStructure, _: Local, assignment: Call, _: Call) =>
            assignment.name shouldBe Operators.assignment
            assignment.code shouldBe "s = (String) o"
        }
      }

      "have an s identifier as the sink argument with refs to the s local" in {
        inside(cpg.call.name("sink").argument.l) { case List(sIdentifier: Identifier) =>
          sIdentifier.name shouldBe "s"
          sIdentifier.typeFullName shouldBe "java.lang.String"
          sIdentifier.refsTo.l shouldBe cpg.local.name("s").l
        }
      }
    }

    "a variable is introduced to the else block and surrounding scope" should {
      val cpg = code("""
          |class Foo {
          |  Integer s;
          |  void foo(Object o) {
          |    if (!(o instanceof String s)) {
          |      sink1(s);
          |      return;
          |    } else {
          |      sink2(s);
          |    }
          |    sink3(s);
          |  }
          |  static void sink1(Object o) {}
          |  static void sink2(Object o) {}
          |  static void sink3(Object o) {}
          |}
          |""".stripMargin)

      "parse" in {
        cpg.identifier.name("s").nonEmpty shouldBe true
      }

      "create the s local before the if statement" in {
        inside(cpg.method.name("foo").body.astChildren.l) {
          case List(sLocal: Local, _: Call, _: ControlStructure, _: Call) =>
            sLocal.name shouldBe "s"
            sLocal.typeFullName shouldBe "java.lang.String"
        }
      }

      "create the s assignment before the if statement" in {
        inside(cpg.method.name("foo").body.astChildren.l) {
          case List(_: Local, assignment: Call, _: ControlStructure, _: Call) =>
            assignment.name shouldBe Operators.assignment
            assignment.code shouldBe "s = (String) o"
        }
      }

      "have an s field access as the sink1 argument" in {
        inside(cpg.call.name("sink1").argument.l) { case List(fieldAccess: Call) =>
          fieldAccess.name shouldBe Operators.fieldAccess
          fieldAccess.code shouldBe "this.s"
          fieldAccess.typeFullName shouldBe "java.lang.Integer"
        }
      }

      "have an s identifier as the sink2 argument with refs to the s local" in {
        inside(cpg.call.name("sink2").argument.l) { case List(sIdentifier: Identifier) =>
          sIdentifier.name shouldBe "s"
          sIdentifier.typeFullName shouldBe "java.lang.String"
          sIdentifier.refsTo.l shouldBe cpg.local.name("s").l
        }
      }

      "have an s identifier as the sink3 argument with refs to the s local" in {
        inside(cpg.call.name("sink3").argument.l) { case List(sIdentifier: Identifier) =>
          sIdentifier.name shouldBe "s"
          sIdentifier.typeFullName shouldBe "java.lang.String"
          sIdentifier.refsTo.l shouldBe cpg.local.name("s").l
        }
      }
    }
  }

  "patterns in while statements" when {
    "a variable is introduced to the body" should {
      val cpg = code("""
          |class Foo {
          |  Integer s;
          |  void foo(Object o) {
          |    while (o instanceof String s) {
          |      sink1(s);
          |    }
          |    sink2(s);
          |  }
          |  static void sink1(Object o) {}
          |  static void sink2(Object o) {}
          |}
          |""".stripMargin)

      "parse" in {
        cpg.identifier.name("s").nonEmpty shouldBe true
      }

      "create the s local in the while body" in {
        inside(cpg.controlStructure.controlStructureType(ControlStructureTypes.WHILE).astChildren.l) {
          case List(_: Call, body: Block) =>
            inside(body.astChildren.l) { case List(sLocal: Local, _: Call, _: Call) =>
              sLocal.name shouldBe "s"
              sLocal.typeFullName shouldBe "java.lang.String"
            }
        }
      }

      "create the s assignment in the body" in {
        inside(cpg.controlStructure.controlStructureType(ControlStructureTypes.WHILE).astChildren.l) {
          case List(_: Call, body: Block) =>
            inside(body.astChildren.l) { case List(_: Local, assignment: Call, _: Call) =>
              assignment.name shouldBe Operators.assignment
              assignment.code shouldBe "s = (String) o"
            }
        }
      }

      "have the argument of sink1 be an s identifier with refs to the s local" in {
        inside(cpg.call.name("sink1").argument.l) { case List(sIdentifier: Identifier) =>
          sIdentifier.name shouldBe "s"
          sIdentifier.typeFullName shouldBe "java.lang.String"
          sIdentifier.refsTo.l shouldBe cpg.local.name("s").l
        }
      }

      "have the argument of sink2 be a field access for s" in {
        inside(cpg.call.name("sink2").argument.l) { case List(fieldAccess: Call) =>
          fieldAccess.name shouldBe Operators.fieldAccess
          fieldAccess.code shouldBe "this.s"
          fieldAccess.typeFullName shouldBe "java.lang.Integer"
        }
      }
    }

    "a variable is introduced by the while" should {
      val cpg = code("""
          |class Foo {
          |  Integer s;
          |  void foo(Object o) {
          |    while (!(o instanceof String s)) {
          |      sink1(s);
          |    }
          |    sink2(s);
          |  }
          |  static void sink1(Object o) {}
          |  static void sink2(Object o) {}
          |}
          |""".stripMargin)

      "parse" in {
        cpg.identifier.name("s").nonEmpty shouldBe true
      }

      "create the s local after the while loop" in {
        inside(cpg.method.name("foo").body.astChildren.l) {
          case List(_: ControlStructure, sLocal: Local, _: Call, _: Call) =>
            sLocal.name shouldBe "s"
            sLocal.typeFullName shouldBe "java.lang.String"
        }
      }

      "create the s assignment after the while loop" in {
        inside(cpg.method.name("foo").body.astChildren.l) {
          case List(_: ControlStructure, _: Local, assignment: Call, _: Call) =>
            assignment.name shouldBe Operators.assignment
            assignment.code shouldBe "s = (String) o"
        }
      }

      "have the argument of sink1 be a field access for s" in {
        inside(cpg.call.name("sink1").argument.l) { case List(fieldAccess: Call) =>
          fieldAccess.name shouldBe Operators.fieldAccess
          fieldAccess.code shouldBe "this.s"
          fieldAccess.typeFullName shouldBe "java.lang.Integer"
        }
      }

      "have the argument of sink2 be an s identifier with refs to the s local" in {
        inside(cpg.call.name("sink2").argument.l) { case List(sIdentifier: Identifier) =>
          sIdentifier.name shouldBe "s"
          sIdentifier.typeFullName shouldBe "java.lang.String"
          sIdentifier.refsTo.l shouldBe cpg.local.name("s").l
        }
      }
    }
  }

  "patterns in do statements" when {
    "a variable is introduced by the do" should {
      val cpg = code("""
          |class Foo {
          |  Integer s;
          |  void foo(Object o) {
          |    do { sink1(s); } while (!(o instanceof String s));
          |    sink2(s);
          |  }
          |  static void sink1(Object o) {}
          |  static void sink2(Object o) {}
          |}
          |""".stripMargin)

      "parse" in {
        cpg.identifier.name("s").nonEmpty shouldBe true
      }

      "create the s local after the do loop" in {
        inside(cpg.method.name("foo").body.astChildren.l) {
          case List(_: ControlStructure, sLocal: Local, _: Call, _: Call) =>
            sLocal.name shouldBe "s"
            sLocal.typeFullName shouldBe "java.lang.String"
        }
      }

      "create the s assignment after the do loop" in {
        inside(cpg.method.name("foo").body.astChildren.l) {
          case List(_: ControlStructure, _: Local, assignment: Call, _: Call) =>
            assignment.name shouldBe Operators.assignment
            assignment.code shouldBe "s = (String) o"
        }
      }

      "have the argument of sink1 be a field access for s" in {
        inside(cpg.call.name("sink1").argument.l) { case List(fieldAccess: Call) =>
          fieldAccess.name shouldBe Operators.fieldAccess
          fieldAccess.code shouldBe "this.s"
          fieldAccess.typeFullName shouldBe "java.lang.Integer"
        }
      }

      "have the argument of sink2 be an s identifier with refs to the s local" in {
        inside(cpg.call.name("sink2").argument.l) { case List(sIdentifier: Identifier) =>
          sIdentifier.name shouldBe "s"
          sIdentifier.typeFullName shouldBe "java.lang.String"
          sIdentifier.refsTo.l shouldBe cpg.local.name("s").l
        }
      }

    }
  }

  "patterns in for statements" when {
    "a variable is introduced to the for update" should {
      "represent it correctly" in {
        ???
      }
    }

    "a variable is introduced to the for body" should {
      val cpg = code("""
          |class Foo {
          |  Integer s;
          |  void foo(Object o) {
          |    for (; o instanceof String s;) {
          |      sink1(s);
          |    }
          |    sink2(s);
          |  }
          |  static void sink1(Object o) {}
          |  static void sink2(Object o) {}
          |}
          |""".stripMargin)

      "parse" in {
        cpg.identifier.name("s").nonEmpty shouldBe true
      }

      "create the s local in the for body" in {
        inside(cpg.controlStructure.controlStructureType(ControlStructureTypes.FOR).astChildren.l) {
          case List(_: Call, body: Block) =>
            inside(body.astChildren.l) { case List(sLocal: Local, _: Call, _: Call) =>
              sLocal.name shouldBe "s"
              sLocal.typeFullName shouldBe "java.lang.String"
            }
        }
      }

      "create the s assignment in the for body" in {
        inside(cpg.controlStructure.controlStructureType(ControlStructureTypes.FOR).astChildren.l) {
          case List(_: Call, body: Block) =>
            inside(body.astChildren.l) { case List(_: Local, assignment: Call, _: Call) =>
              assignment.name shouldBe Operators.assignment
              assignment.code shouldBe "s = (String) o"
            }
        }
      }

      "have the argument of sink1 be an s identifier with refs to the s local" in {
        inside(cpg.call.name("sink1").argument.l) { case List(sIdentifier: Identifier) =>
          sIdentifier.name shouldBe "s"
          sIdentifier.typeFullName shouldBe "java.lang.String"
          sIdentifier.refsTo.l shouldBe cpg.local.name("s").l
        }
      }

      "have the argument of sink2 be a field access for s" in {
        inside(cpg.call.name("sink2").argument.l) { case List(fieldAccess: Call) =>
          fieldAccess.name shouldBe Operators.fieldAccess
          fieldAccess.code shouldBe "this.s"
          fieldAccess.typeFullName shouldBe "java.lang.Integer"
        }
      }
    }

    "a variable is introduced by the for" should {
      val cpg = code("""
                         |class Foo {
                         |  Integer s;
                         |  void foo(Object o) {
                         |    for (; !(s instanceof String s);) {
                         |      sink1(s);
                         |    }
                         |    sink2(s);
                         |  }
                         |  static void sink1(Object o) {}
                         |  static void sink2(Object o) {}
                         |}
                         |""".stripMargin)
      "parse" in {
        cpg.identifier.name("s").nonEmpty shouldBe true
      }

      "create the s local after the for loop" in {
        inside(cpg.method.name("foo").body.astChildren.l) {
          case List(_: ControlStructure, sLocal: Local, _: Call, _: Call) =>
            sLocal.name shouldBe "s"
            sLocal.typeFullName shouldBe "java.lang.String"
        }
      }

      "create the s assignment after the for loop" in {
        inside(cpg.method.name("foo").body.astChildren.l) {
          case List(_: ControlStructure, _: Local, assignment: Call, _: Call) =>
            assignment.name shouldBe Operators.assignment
            assignment.code shouldBe "s = (String) o"
        }
      }

      "have the argument of sink1 be a field access for s" in {
        inside(cpg.call.name("sink1").argument.l) { case List(fieldAccess: Call) =>
          fieldAccess.name shouldBe Operators.fieldAccess
          fieldAccess.code shouldBe "this.s"
          fieldAccess.typeFullName shouldBe "java.lang.Integer"
        }
      }

      "have the argument of sink2 be an s identifier with refs to the s local" in {
        inside(cpg.call.name("sink2").argument.l) { case List(sIdentifier: Identifier) =>
          sIdentifier.name shouldBe "s"
          sIdentifier.typeFullName shouldBe "java.lang.String"
          sIdentifier.refsTo.l shouldBe cpg.local.name("s").l
        }
      }
    }
  }

  "resolved patterns in instanceof expressions" when {
    "a type pattern is matched" should {
      val cpg = code("""
          |class Foo {
          |  void foo(Object o) {
          |    if (o instanceof String s) {
          |      sink(s);
          |    }
          |  }
          |}
          |""".stripMargin)

      "parse" in {
        cpg.call.name("sink").isEmpty shouldBe false
      }

      "have the correct lowering for the type check" in {
        inside(cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).condition.l) {
          case List(instanceOfCall: Call) =>
            instanceOfCall.name shouldBe Operators.instanceOf
            instanceOfCall.typeFullName shouldBe "boolean"
            instanceOfCall.code shouldBe "o instanceof String"

            inside(instanceOfCall.argument.l) { case List(oIdentifier: Identifier, stringType: TypeRef) =>
              oIdentifier.name shouldBe "o"
              oIdentifier.typeFullName shouldBe "java.lang.Object"
              oIdentifier.code shouldBe "o"

              stringType.typeFullName shouldBe "java.lang.String"
              stringType.code shouldBe "String"
            }
        }
      }

      "have the correct lowering for the variable assignment" in {
        inside(cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).astChildren.isBlock.astChildren.l) {
          case List(sLocal: Local, sAssign: Call, _: Call) =>
            sLocal.name shouldBe "s"
            sLocal.typeFullName shouldBe "java.lang.String"
            sLocal.code shouldBe "String s"

            sAssign.name shouldBe Operators.assignment
            sAssign.methodFullName shouldBe Operators.assignment
            sAssign.code shouldBe "String s = (String) o"

            inside(sAssign.argument.l) { case List(sIdentifier: Identifier, castExpr: Call) =>
              sIdentifier.name shouldBe "s"
              sIdentifier.typeFullName shouldBe "java.lang.String"
              sIdentifier.code shouldBe "s"
              sIdentifier.refsTo.l should contain theSameElementsAs List(sLocal)

              castExpr.name shouldBe Operators.cast
              castExpr.methodFullName shouldBe Operators.cast
              castExpr.typeFullName shouldBe "java.lang.String"
              castExpr.code shouldBe "(String) o"

              inside(castExpr.argument.l) { case List(stringType: TypeRef, oIdentifier: Identifier) =>
                stringType.typeFullName shouldBe "java.lang.String"
                stringType.code shouldBe "String"

                oIdentifier.name shouldBe "o"
                oIdentifier.typeFullName shouldBe "java.lang.Object"
                oIdentifier.code shouldBe "o"
                oIdentifier.refsTo.l should contain theSameElementsAs cpg.method.name("foo").parameter.name("o").l
              }
            }
        }
      }
    }

    "a non-generic, non-nested record pattern is matched" should {
      val cpg = code("""
          |package box;
          |
          |record Box(String value) {}
          |
          |class Foo {
          |  void foo(Object o) {
          |    if (o instanceof Box(String s)) {
          |      sink(s);
          |    }
          |  }
          |}
          |""".stripMargin)

      "parse" in {
        cpg.call.name("sink").isEmpty shouldBe false
      }

      "have the correct lowering for the type check" in {
        // Don't need to check `Box.value() instanceof String` since it must be from the declaration
        inside(cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).condition.l) {
          case List(instanceOfBox: Call) =>
            instanceOfBox.name shouldBe Operators.instanceOf
            instanceOfBox.methodFullName shouldBe Operators.instanceOf
            instanceOfBox.code shouldBe "o instanceof Box"
            instanceOfBox.typeFullName shouldBe "boolean"

            inside(instanceOfBox.argument.l) { case List(oIdentifier: Identifier, boxType: TypeRef) =>
              oIdentifier.name shouldBe "o"
              oIdentifier.typeFullName shouldBe "java.lang.Object"
              oIdentifier.code shouldBe "o"
              oIdentifier.refsTo.l shouldBe cpg.method.name("foo").parameter.name("o").l

              boxType.typeFullName shouldBe "box.Box"
              boxType.code shouldBe "Box"
            }
        }
      }

      "have the correct lowering for the variable assignment" in {
        inside(cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).astChildren.isBlock.astChildren.l) {
          case List(sLocal: Local, sAssignment: Call, _: Call) =>
            sLocal.name shouldBe "s"
            sLocal.typeFullName shouldBe "java.lang.String"
            sLocal.code shouldBe "String s"

            sAssignment.name shouldBe Operators.assignment
            sAssignment.methodFullName shouldBe Operators.assignment
            sAssignment.typeFullName shouldBe "java.lang.String"
            sAssignment.code shouldBe "s = ((Box) o).value()"

            inside(sAssignment.argument.l) { case List(sIdentifier: Identifier, valueCall: Call) =>
              sIdentifier.name shouldBe "s"
              sIdentifier.typeFullName shouldBe "java.lang.String"
              sIdentifier.code shouldBe "s"
              sIdentifier.refsTo.l shouldBe List(sLocal)

              valueCall.name shouldBe "value"
              valueCall.methodFullName shouldBe "box.Box.value:java.lang.String()"
              valueCall.typeFullName shouldBe "java.lang.String"
              valueCall.code shouldBe "((Box) o).value()"

              inside(valueCall.argument.l) { case List(castExpr: Call) =>
                castExpr.name shouldBe Operators.cast
                castExpr.methodFullName shouldBe Operators.cast
                castExpr.typeFullName shouldBe "box.Box"
                castExpr.code shouldBe "(Box) o"

                inside(castExpr.argument.l) { case List(boxType: TypeRef, oIdentifier: Identifier) =>
                  boxType.typeFullName shouldBe "box.Box"
                  boxType.code shouldBe "Box"

                  oIdentifier.name shouldBe "o"
                  oIdentifier.typeFullName shouldBe "java.lang.Object"
                  oIdentifier.code shouldBe "o"
                  oIdentifier.refsTo.l shouldBe cpg.method.name("foo").parameter.name("o").l
                }
              }
            }
        }
      }
    }

    "a generic, non-nested record pattern is matched" should {
      val cpg = code("""
          |package box;
          |
          |record Box<T>(T value) {}
          |
          |class Foo {
          |  void foo(Object o) {
          |    if (o instanceof Box(String s)) {
          |      sink(s);
          |    }
          |  }
          |}
          |""".stripMargin)

      "parse" in {
        cpg.call.name("sink").isEmpty shouldBe false
      }

      "have the correct lowering for the type check" in {
        inside(cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).condition.l) {
          case List(andCall: Call) =>
            andCall.name shouldBe Operators.logicalAnd
            andCall.methodFullName shouldBe Operators.logicalAnd
            andCall.code shouldBe "o instanceof Box && ((Box) o).value() instanceof String)"

            inside(andCall.argument.l) { case List(instanceOfBox: Call, instanceOfString: Call) =>
              instanceOfBox.name shouldBe Operators.instanceOf
              instanceOfBox.methodFullName shouldBe Operators.instanceOf
              instanceOfBox.code shouldBe "o instanceof Box"
              instanceOfBox.typeFullName shouldBe "boolean"

              inside(instanceOfBox.argument.l) { case List(oIdentifier: Identifier, boxType: TypeRef) =>
                oIdentifier.name shouldBe "o"
                oIdentifier.typeFullName shouldBe "java.lang.Object"
                oIdentifier.code shouldBe "o"
                oIdentifier.refsTo.l shouldBe cpg.method.name("foo").parameter.name("o").l

                boxType.typeFullName shouldBe "box.Box"
                boxType.code shouldBe "Box"
              }

              instanceOfString.name shouldBe Operators.instanceOf
              instanceOfString.methodFullName shouldBe Operators.instanceOf
              instanceOfString.code shouldBe "((Box) o).value() instanceof String"
              instanceOfString.typeFullName shouldBe "boolean"

              inside(instanceOfString.argument.l) { case List(valueCall: Call, stringType: TypeRef) =>
                valueCall.name shouldBe "value"
                valueCall.methodFullName shouldBe "box.Box.value:java.lang.String()"
                valueCall.code shouldBe "((Box) o).value()"
                valueCall.typeFullName shouldBe "java.lang.String"

                inside(valueCall.argument.l) { case List(castExpr: Call) =>
                  castExpr.name shouldBe Operators.cast
                  castExpr.methodFullName shouldBe Operators.cast
                  castExpr.typeFullName shouldBe "box.Box"
                  inside(castExpr.argument.l) { case List(castBoxType: TypeRef, castOIdentifier: Identifier) =>
                    castBoxType.typeFullName shouldBe "box.Box"
                    castBoxType.code shouldBe "Box"

                    castOIdentifier.name shouldBe "o"
                    castOIdentifier.typeFullName shouldBe "java.lang.Object"
                    castOIdentifier.code shouldBe "o"
                    castOIdentifier.refsTo.l shouldBe cpg.method.name("foo").parameter.name("o").l
                  }
                }

                stringType.typeFullName shouldBe "java.lang.String"
                stringType.code shouldBe "String"
              }
            }
        }
      }

      "have the correct lowering for the variable assignment" in {
        inside(cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).astChildren.isBlock.astChildren.l) {
          case List(sLocal: Local, sAssignment: Call, _: Call) =>
            sLocal.name shouldBe "s"
            sLocal.typeFullName shouldBe "java.lang.String"
            sLocal.code shouldBe "String s"

            sAssignment.name shouldBe Operators.assignment
            sAssignment.methodFullName shouldBe Operators.assignment
            sAssignment.typeFullName shouldBe "java.lang.String"
            sAssignment.code shouldBe "s = (String) ((Box) o).value()"

            inside(sAssignment.argument.l) { case List(stringCast: Call) =>
              stringCast.name shouldBe Operators.cast
              stringCast.methodFullName shouldBe Operators.cast
              stringCast.typeFullName shouldBe "java.lang.String"
              stringCast.code shouldBe "(String) ((Box) o).value()"

              inside(stringCast.argument.l) { case List(sIdentifier: Identifier, valueCall: Call) =>
                sIdentifier.name shouldBe "s"
                sIdentifier.typeFullName shouldBe "java.lang.String"
                sIdentifier.code shouldBe "s"
                sIdentifier.refsTo.l shouldBe List(sLocal)

                valueCall.name shouldBe "value"
                valueCall.methodFullName shouldBe "box.Box.value:java.lang.Object()"
                valueCall.typeFullName shouldBe "java.lang.Object"
                valueCall.code shouldBe "((Box) o).value()"

                inside(valueCall.argument.l) { case List(castExpr: Call) =>
                  castExpr.name shouldBe Operators.cast
                  castExpr.methodFullName shouldBe Operators.cast
                  castExpr.typeFullName shouldBe "box.Box"
                  castExpr.code shouldBe "(Box) o"

                  inside(castExpr.argument.l) { case List(boxType: TypeRef, oIdentifier: Identifier) =>
                    boxType.typeFullName shouldBe "box.Box"
                    boxType.code shouldBe "Box"

                    oIdentifier.name shouldBe "o"
                    oIdentifier.typeFullName shouldBe "java.lang.Object"
                    oIdentifier.code shouldBe "o"
                    oIdentifier.refsTo.l shouldBe cpg.method.name("foo").parameter.name("o").l
                  }
                }
              }
            }
        }
      }
    }

    "a non-generic, nested record pattern is matched" should {

      val cpg = code("""
          |package box;
          |
          |record PairBox(Pair value) {}
          |record Pair(String first, Integer second) {}
          |
          |class Foo {
          |  void foo(Object o) {
          |    if (o instanceof PairBox(Pair (String s, Integer i))) {
          |      sink(s);
          |      sink(i);
          |    }
          |  }
          |}
          |""".stripMargin)

      "parse" in {
        cpg.call.name("sink").isEmpty shouldBe false
      }

      "have the correct lowering for the type check" in {
        // In this case it is only necessary to check o instanceof PairBox since it is assumed the code compiles
        inside(cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).condition.l) {
          case List(instanceOfCall: Call) =>
            instanceOfCall.name shouldBe Operators.instanceOf
            instanceOfCall.typeFullName shouldBe "boolean"
            instanceOfCall.code shouldBe "o instanceof PairBox"

            inside(instanceOfCall.argument.l) { case List(oIdentifier: Identifier, pairBoxType: TypeRef) =>
              oIdentifier.name shouldBe "o"
              oIdentifier.typeFullName shouldBe "java.lang.Object"
              oIdentifier.code shouldBe "o"

              pairBoxType.typeFullName shouldBe "java.lang.String"
              pairBoxType.code shouldBe "String"
            }
        }
      }

      "have the correct lowering for the variable assignment" in {
        val oParameter = cpg.method.name("foo").parameter.name("o").l
        inside(cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).astChildren.isBlock.astChildren.l) {
          case List(sLocal: Local, iLocal: Local, sAssign: Call, iAssign: Call, sSink: Call, iSink: Call) =>
            sLocal.name shouldBe "s"
            sLocal.code shouldBe "s"
            sLocal.typeFullName shouldBe "java.lang.String"

            iLocal.name shouldBe "i"
            iLocal.code shouldBe "i"
            iLocal.typeFullName shouldBe "java.lang.Integer"

            sAssign.name shouldBe Operators.assignment
            sAssign.methodFullName shouldBe Operators.assignment
            sAssign.typeFullName shouldBe "java.lang.String"
            sAssign.code shouldBe "s = ((PairBox) o).value().first()"

            inside(sAssign.argument.l) { case List(sIdentifier: Identifier, firstCall: Call) =>
              sIdentifier.name shouldBe "s"
              sIdentifier.code shouldBe "s"
              sIdentifier.typeFullName shouldBe "java.lang.String"
              sIdentifier.refsTo.l shouldBe List(sLocal)

              firstCall.name shouldBe "first"
              firstCall.methodFullName shouldBe "box.Pair.first:java.lang.String()"
              firstCall.signature shouldBe "java.lang.String()"
              firstCall.typeFullName shouldBe "java.lang.String"
              firstCall.code shouldBe "((PairBox) o).value().first()"

              inside(firstCall.argument.l) { case List(valueCall: Call) =>
                valueCall.name shouldBe "value"
                valueCall.methodFullName shouldBe "box.PairBox.value:box.Pair()"
                valueCall.signature shouldBe "box.Pair()"
                valueCall.typeFullName shouldBe "box.Pair"
                valueCall.code shouldBe "((PairBox) o).value()"

                inside(valueCall.argument.l) { case List(castExpr: Call) =>
                  castExpr.name shouldBe Operators.cast
                  castExpr.methodFullName shouldBe Operators.cast
                  castExpr.typeFullName shouldBe "box.PairBox"
                  castExpr.code shouldBe "(PairBox) o"

                  inside(castExpr.argument.l) { case List(pairBoxType: TypeRef, oIdentifier: Identifier) =>
                    pairBoxType.typeFullName shouldBe "box.PairBox"
                    pairBoxType.code shouldBe "PairBox"

                    oIdentifier.name shouldBe "o"
                    oIdentifier.code shouldBe "o"
                    oIdentifier.typeFullName shouldBe "java.lang.Object"
                    oIdentifier.refsTo.l shouldBe oParameter
                  }
                }
              }
            }

            iAssign.name shouldBe Operators.assignment
            iAssign.methodFullName shouldBe Operators.assignment
            iAssign.typeFullName shouldBe "java.lang.Integer"
            iAssign.code shouldBe "i = ((PairBox) o).value().second()"

            inside(iAssign.argument.l) { case List(iIdentifier: Identifier, secondCall: Call) =>
              iIdentifier.name shouldBe "i"
              iIdentifier.code shouldBe "i"
              iIdentifier.typeFullName shouldBe "java.lang.Integer"
              iIdentifier.refsTo.l shouldBe List(iLocal)

              secondCall.name shouldBe "second"
              secondCall.methodFullName shouldBe "box.Pair.second:java.lang.Integer()"
              secondCall.signature shouldBe "java.lang.Integer()"
              secondCall.typeFullName shouldBe "java.lang.Integer"
              secondCall.code shouldBe "((PairBox) o).value().second()"

              inside(secondCall.argument.l) { case List(valueCall: Call) =>
                valueCall.name shouldBe "value"
                valueCall.methodFullName shouldBe "box.PairBox.value:box.Pair()"
                valueCall.signature shouldBe "box.Pair()"
                valueCall.typeFullName shouldBe "box.Pair()"
                valueCall.code shouldBe "((PairBox) o).value()"

                inside(valueCall.argument.l) { case List(castExpr: Call) =>
                  castExpr.name shouldBe Operators.cast
                  castExpr.methodFullName shouldBe Operators.cast
                  castExpr.typeFullName shouldBe "box.PairBox"
                  castExpr.code shouldBe "(PairBox) o"

                  inside(castExpr.argument.l) { case List(pairBoxType: TypeRef, oIdentifier: Identifier) =>
                    pairBoxType.typeFullName shouldBe "box.PairBox"
                    pairBoxType.code shouldBe "PairBox"

                    oIdentifier.name shouldBe "o"
                    oIdentifier.code shouldBe "o"
                    oIdentifier.typeFullName shouldBe "java.lang.Object"
                    oIdentifier.refsTo.l shouldBe oParameter
                  }
                }
              }
            }

            inside(sSink.argument.isIdentifier.name("s").l) { case List(sIdentifier: Identifier) =>
              sIdentifier.name shouldBe "s"
              sIdentifier.code shouldBe "s"
              sIdentifier.typeFullName shouldBe "java.lang.String"
              sIdentifier.refsTo.l shouldBe List(sLocal)
            }

            inside(iSink.argument.isIdentifier.name("i").l) { case List(iIdentifier: Identifier) =>
              iIdentifier.name shouldBe "i"
              iIdentifier.code shouldBe "i"
              iIdentifier.typeFullName shouldBe "java.lang.Integer"
              iIdentifier.refsTo.l shouldBe List(iLocal)
            }
        }
      }
    }

    "a generic, nested record pattern is matched" should {

      val cpg = code("""
          |package box;
          |
          |record Box<T>(T value) {}
          |record Pair<U, V>(U first, V second) {}
          |
          |class Foo {
          |  void foo(Object o) {
          |    if (o instanceof Box(Pair(String s, Integer i))) {
          |      sink(s);
          |      sink(i);
          |    }
          |  }
          |}
          |""".stripMargin)

      "parse" in {
        cpg.call.name("sink").isEmpty shouldBe false
      }

      "have the correct lowering for the type check" in {
        val oParameter = cpg.method.name("foo").parameter.name("o").l
        inside(cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).condition.l) {
          case List(firstAnd: Call) =>
            firstAnd.name shouldBe Operators.logicalAnd
            firstAnd.methodFullName shouldBe Operators.logicalAnd
            firstAnd.typeFullName shouldBe "boolean"
            firstAnd.code shouldBe "o instanceof Box(Pair(String s, Integer i)) && (((Box) o).value() instanceof Pair && (((Pair) ((Box) o).value()).first() instanceof String && ((Pair) ((Box) o).value()).second() instanceof Integer))"

            inside(firstAnd.argument.l) { case List(oInstanceOfBox: Call, secondAnd: Call) =>
              oInstanceOfBox.name shouldBe Operators.instanceOf
              oInstanceOfBox.methodFullName shouldBe Operators.instanceOf
              oInstanceOfBox.typeFullName shouldBe "boolean"
              oInstanceOfBox.code shouldBe "o instanceof Box(Pair(String s, Integer i))"

              inside(oInstanceOfBox.argument.l) { case List(oIdentifier: Identifier, boxType: TypeRef) =>
                oIdentifier.name shouldBe "o"
                oIdentifier.typeFullName shouldBe "java.lang.Object"
                oIdentifier.code shouldBe "o"
                oIdentifier.refsTo.l shouldBe oParameter

                boxType.typeFullName shouldBe "box.Box"
                boxType.code shouldBe "Box"
              }

              secondAnd.name shouldBe Operators.logicalAnd
              secondAnd.methodFullName shouldBe Operators.logicalAnd
              secondAnd.typeFullName shouldBe "boolean"
              secondAnd.code shouldBe "((Box) o).value() instanceof Pair && (((Pair) ((Box) o).value()).first() instanceof String && ((Pair) ((Box) o).value()).second() instanceof Integer)"

              inside(secondAnd.argument.l) { case List(oValueInstanceOfPair: Call, thirdAnd: Call) =>
                oValueInstanceOfPair.name shouldBe Operators.instanceOf
                oValueInstanceOfPair.methodFullName shouldBe Operators.instanceOf
                oValueInstanceOfPair.typeFullName shouldBe "boolean"
                oValueInstanceOfPair.code shouldBe "((Box) o).value() instanceof Pair"

                inside(oValueInstanceOfPair.argument.l) { case List(valueCall: Call, pairType: TypeRef) =>
                  valueCall.name shouldBe "value"
                  valueCall.methodFullName shouldBe "box.Box.value:box.Pair()"
                  valueCall.signature shouldBe "box.Pair()"
                  valueCall.code shouldBe "((Box) o).value()"

                  inside(valueCall.argument.l) { case List(castExpr: Call) =>
                    castExpr.name shouldBe Operators.cast
                    castExpr.methodFullName shouldBe Operators.cast
                    castExpr.typeFullName shouldBe "box.Box"
                    castExpr.code shouldBe "(Box) o"

                    inside(castExpr.argument.l) { case List(boxType: TypeRef, oIdentifier: Identifier) =>
                      boxType.typeFullName shouldBe "box.Box"
                      boxType.code shouldBe "Box"

                      oIdentifier.name shouldBe "o"
                      oIdentifier.code shouldBe "o"
                      oIdentifier.typeFullName shouldBe "java.lang.Object"
                      oIdentifier.refsTo.l shouldBe oParameter
                    }
                  }

                  pairType.typeFullName shouldBe "box.Pair"
                  pairType.code shouldBe "Pair"
                }

                thirdAnd.name shouldBe Operators.logicalAnd
                thirdAnd.methodFullName shouldBe Operators.logicalAnd
                thirdAnd.typeFullName shouldBe "boolean"
                thirdAnd.code shouldBe "((Pair) ((Box) o).value()).first() instanceof String && ((Pair) ((Box) o).value()).second() instanceof Integer"

                inside(thirdAnd.argument.l) { case List(firstInstanceOfString: Call, secondInstanceOfInteger: Call) =>
                  firstInstanceOfString.name shouldBe Operators.instanceOf
                  firstInstanceOfString.methodFullName shouldBe Operators.instanceOf
                  firstInstanceOfString.typeFullName shouldBe "boolean"
                  firstInstanceOfString.code shouldBe "((Pair) ((Box) o).value()).first() instanceof String"

                  inside(firstInstanceOfString.argument.l) { case List(firstCall: Call, stringType: TypeRef) =>
                    firstCall.name shouldBe "first"
                    firstCall.methodFullName shouldBe "box.Pair.first:java.lang.String()"
                    firstCall.typeFullName shouldBe "java.lang.String"
                    firstCall.code shouldBe "((Pair) ((Box) o).value()).first()"

                    inside(firstCall.argument.l) { case List(pairCast: Call) =>
                      pairCast.name shouldBe Operators.cast
                      pairCast.methodFullName shouldBe Operators.cast
                      pairCast.typeFullName shouldBe "box.Pair"
                      pairCast.code shouldBe "(Pair) ((Box) o).value()"

                      inside(pairCast.argument.l) { case List(pairType: TypeRef, valueCall: Call) =>
                        pairType.typeFullName shouldBe "box.Pair"
                        pairType.code shouldBe "Pair"

                        valueCall.name shouldBe "value"
                        valueCall.methodFullName shouldBe "box.Box.value:box.Pair()"
                        valueCall.typeFullName shouldBe "box.Pair"
                        valueCall.code shouldBe "((Box) o).value()"

                        inside(valueCall.argument.l) { case List(boxCast: Call) =>
                          boxCast.name shouldBe Operators.cast
                          boxCast.methodFullName shouldBe Operators.cast
                          boxCast.typeFullName shouldBe "box.Box"
                          boxCast.code shouldBe "(Box) o"

                          inside(boxCast.argument.l) { case List(boxType: TypeRef, oIdentifier: Identifier) =>
                            boxType.typeFullName shouldBe "box.Box"
                            boxType.code shouldBe "Box"

                            oIdentifier.name shouldBe "o"
                            oIdentifier.code shouldBe "o"
                            oIdentifier.typeFullName shouldBe "java.lang.Object"
                            oIdentifier.refsTo.l shouldBe oParameter
                          }
                        }
                      }
                    }

                    stringType.typeFullName shouldBe "java.lang.String"
                    stringType.code shouldBe "String"
                  }

                  secondInstanceOfInteger.name shouldBe Operators.instanceOf
                  secondInstanceOfInteger.methodFullName shouldBe Operators.instanceOf
                  secondInstanceOfInteger.typeFullName shouldBe "boolean"
                  secondInstanceOfInteger.code shouldBe "((Pair) ((Box) o).value()).second() instanceof Integer"

                  inside(secondInstanceOfInteger.argument.l) { case List(secondCall: Call, integerType: TypeRef) =>
                    secondCall.name shouldBe "second"
                    secondCall.methodFullName shouldBe "box.Pair.second:java.lang.String()"
                    secondCall.typeFullName shouldBe "java.lang.Integer"
                    secondCall.code shouldBe "((Pair) ((Box) o).value()).second()"

                    inside(secondCall.argument.l) { case List(pairCast: Call) =>
                      pairCast.name shouldBe Operators.cast
                      pairCast.methodFullName shouldBe Operators.cast
                      pairCast.typeFullName shouldBe "box.Pair"
                      pairCast.code shouldBe "(Pair) ((Box) o).value()"

                      inside(pairCast.argument.l) { case List(pairType: TypeRef, valueCall: Call) =>
                        pairType.typeFullName shouldBe "box.Pair"
                        pairType.code shouldBe "Pair"

                        valueCall.name shouldBe "value"
                        valueCall.methodFullName shouldBe "box.Box.value:box.Pair()"
                        valueCall.typeFullName shouldBe "box.Pair"
                        valueCall.code shouldBe "((Box) o).value()"

                        inside(valueCall.argument.l) { case List(boxCast: Call) =>
                          boxCast.name shouldBe Operators.cast
                          boxCast.methodFullName shouldBe Operators.cast
                          boxCast.typeFullName shouldBe "box.Box"
                          boxCast.code shouldBe "(Box) o"

                          inside(boxCast.argument.l) { case List(boxType: TypeRef, oIdentifier: Identifier) =>
                            boxType.typeFullName shouldBe "box.Box"
                            boxType.code shouldBe "Box"

                            oIdentifier.name shouldBe "o"
                            oIdentifier.code shouldBe "o"
                            oIdentifier.typeFullName shouldBe "java.lang.Object"
                            oIdentifier.refsTo.l shouldBe oParameter
                          }
                        }
                      }
                    }
                    integerType.typeFullName shouldBe "java.lang.Integer"
                    integerType.code shouldBe "Integer"
                  }
                }
              }
            }
        }
      }

      "have the correct lowering for the variable assignment" in {
        val oParameter = cpg.method.name("foo").parameter.name("o").l
        inside(cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).astChildren.isBlock.astChildren.l) {
          case List(sLocal: Local, iLocal: Local, sAssign: Call, iAssign: Call, sSink: Call, iSink: Call) =>
            sLocal.name shouldBe "s"
            sLocal.code shouldBe "s"
            sLocal.typeFullName shouldBe "java.lang.String"

            iLocal.name shouldBe "i"
            iLocal.code shouldBe "i"
            iLocal.typeFullName shouldBe "java.lang.Integer"

            sAssign.name shouldBe Operators.assignment
            sAssign.methodFullName shouldBe Operators.assignment
            sAssign.typeFullName shouldBe "java.lang.String"
            sAssign.code shouldBe "s = (String) ((Pair) ((Box) o).value()).first()"

            inside(sAssign.argument.l) { case List(sIdentifier: Identifier, stringCast: Call) =>
              sIdentifier.name shouldBe "s"
              sIdentifier.code shouldBe "s"
              sIdentifier.typeFullName shouldBe "java.lang.String"
              sIdentifier.refsTo.l shouldBe List(sLocal)

              stringCast.name shouldBe Operators.cast
              stringCast.methodFullName shouldBe Operators.cast
              stringCast.typeFullName shouldBe "java.lang.String"
              stringCast.code shouldBe "(String) ((Pair) ((Box) o).value()).first()"

              inside(stringCast.argument.l) { case List(stringType: TypeRef, firstCall: Call) =>
                stringType.typeFullName shouldBe "java.lang.String"
                stringType.code shouldBe "String"

                firstCall.name shouldBe "first"
                firstCall.methodFullName shouldBe "box.Pair.first:java.lang.Object()"
                firstCall.signature shouldBe "java.lang.Object()"
                // TODO: Should this be the erased type?
                firstCall.typeFullName shouldBe "java.lang.Object"
                firstCall.code shouldBe "((Pair) ((Box) o).value()).first()"

                inside(firstCall.argument.l) { case List(pairCast: Call) =>
                  pairCast.name shouldBe Operators.cast
                  pairCast.methodFullName shouldBe Operators.cast
                  pairCast.typeFullName shouldBe "box.Pair"
                  pairCast.code shouldBe "(Pair) ((Box) o).value()"

                  inside(pairCast.argument.l) { case List(pairType: TypeRef, valueCall: Call) =>
                    pairType.typeFullName shouldBe "box.Pair"
                    pairType.code shouldBe "Pair"

                    valueCall.name shouldBe "value"
                    valueCall.methodFullName shouldBe "box.Box.value:java.lang.Object()"
                    valueCall.signature shouldBe "java.lang.Object()"
                    valueCall.typeFullName shouldBe "java.lang.Object"
                    valueCall.code shouldBe "((Box) o).value()"

                    inside(valueCall.argument.l) { case List(boxCast: Call) =>
                      boxCast.name shouldBe Operators.cast
                      boxCast.methodFullName shouldBe Operators.cast
                      boxCast.typeFullName shouldBe "box.Box"
                      boxCast.code shouldBe "(Box) o"

                      inside(boxCast.argument.l) { case List(boxType: TypeRef, oIdentifier: Identifier) =>
                        boxType.typeFullName shouldBe "box.Box"
                        boxType.code shouldBe "Box"

                        oIdentifier.name shouldBe "o"
                        oIdentifier.code shouldBe "o"
                        oIdentifier.typeFullName shouldBe "java.lang.Object"
                        oIdentifier.refsTo.l shouldBe oParameter
                      }
                    }
                  }
                }
              }
            }

            iAssign.name shouldBe Operators.assignment
            iAssign.methodFullName shouldBe Operators.assignment
            iAssign.typeFullName shouldBe "java.lang.Integer"
            iAssign.code shouldBe "i = (Integer) ((Pair) ((Box) o).value()).second()"

            inside(iAssign.argument.l) { case List(iIdentifier: Identifier, integerCast: Call) =>
              iIdentifier.name shouldBe "i"
              iIdentifier.code shouldBe "i"
              iIdentifier.typeFullName shouldBe "java.lang.Integer"
              iIdentifier.refsTo.l shouldBe List(iLocal)

              integerCast.name shouldBe Operators.cast
              integerCast.methodFullName shouldBe Operators.cast
              integerCast.typeFullName shouldBe "java.lang.Integer"
              integerCast.code shouldBe "(Integer) ((Pair) ((Box) o).value()).second()"

              inside(integerCast.argument.l) { case List(integerType: TypeRef, secondCall: Call) =>
                integerType.typeFullName shouldBe "java.lang.Integer"
                integerType.code shouldBe "Integer"

                secondCall.name shouldBe "second"
                secondCall.methodFullName shouldBe "box.Pair.second:java.lang.Object()"
                secondCall.signature shouldBe "java.lang.Object()"
                // TODO: Should this be the erased type?
                secondCall.typeFullName shouldBe "java.lang.Object"
                secondCall.code shouldBe "((Pair) ((Box) o).value()).second()"

                inside(secondCall.argument.l) { case List(pairCast: Call) =>
                  pairCast.name shouldBe Operators.cast
                  pairCast.methodFullName shouldBe Operators.cast
                  pairCast.typeFullName shouldBe "box.Pair"
                  pairCast.code shouldBe "(Pair) ((Box) o).value()"

                  inside(pairCast.argument.l) { case List(pairType: TypeRef, valueCall: Call) =>
                    pairType.typeFullName shouldBe "box.Pair"
                    pairType.code shouldBe "Pair"

                    valueCall.name shouldBe "value"
                    valueCall.methodFullName shouldBe "box.Box.value:java.lang.Object()"
                    valueCall.signature shouldBe "java.lang.Object()"
                    valueCall.typeFullName shouldBe "java.lang.Object"
                    valueCall.code shouldBe "((Box) o).value()"

                    inside(valueCall.argument.l) { case List(boxCast: Call) =>
                      boxCast.name shouldBe Operators.cast
                      boxCast.methodFullName shouldBe Operators.cast
                      boxCast.typeFullName shouldBe "box.Box"
                      boxCast.code shouldBe "(Box) o"

                      inside(boxCast.argument.l) { case List(boxType: TypeRef, oIdentifier: Identifier) =>
                        boxType.typeFullName shouldBe "box.Box"
                        boxType.code shouldBe "Box"

                        oIdentifier.name shouldBe "o"
                        oIdentifier.code shouldBe "o"
                        oIdentifier.typeFullName shouldBe "java.lang.Object"
                        oIdentifier.refsTo.l shouldBe oParameter
                      }
                    }
                  }
                }
              }
            }
        }
      }
    }
  }

  "resolved patterns in switch expressions" when {
    "a type pattern is matched" should {
      val cpg = code("""
          |package box;
          |
          |class Foo {
          |  void foo(Object o) {
          |    switch (o) {
          |      case String s -> sink(s);
          |      default -> {}
          |    }
          |  }
          |}
          |""".stripMargin)

      "parse" in {
        cpg.call.name("sink").isEmpty shouldBe false
      }

      "have the correct lowering for the variable assignment" in {
        // TODO Should this be MATCH?
        inside(
          cpg.controlStructure.controlStructureType(ControlStructureTypes.SWITCH).astChildren.isBlock.astChildren.l
        ) { case List(sLocal: Local, sAssign: Call, _: Call) =>
          sLocal.name shouldBe "s"
          sLocal.typeFullName shouldBe "java.lang.String"
          sLocal.code shouldBe "String s"

          sAssign.name shouldBe Operators.assignment
          sAssign.methodFullName shouldBe Operators.assignment
          sAssign.code shouldBe "String s = (String) o"

          inside(sAssign.argument.l) { case List(sIdentifier: Identifier, castExpr: Call) =>
            sIdentifier.name shouldBe "s"
            sIdentifier.typeFullName shouldBe "java.lang.String"
            sIdentifier.code shouldBe "s"
            sIdentifier.refsTo.l should contain theSameElementsAs List(sLocal)

            castExpr.name shouldBe Operators.cast
            castExpr.methodFullName shouldBe Operators.cast
            castExpr.typeFullName shouldBe "java.lang.String"
            castExpr.code shouldBe "(String) o"

            inside(castExpr.argument.l) { case List(stringType: TypeRef, oIdentifier: Identifier) =>
              stringType.typeFullName shouldBe "java.lang.String"
              stringType.code shouldBe "String"

              oIdentifier.name shouldBe "o"
              oIdentifier.typeFullName shouldBe "java.lang.Object"
              oIdentifier.code shouldBe "o"
              oIdentifier.refsTo.l should contain theSameElementsAs cpg.method.name("foo").parameter.name("o").l
            }
          }
        }
      }
    }

    "a non-generic, non-nested record pattern is matched" should {
      val cpg = code("""
          |package box;
          |
          |record Box(String value) {}
          |
          |class Foo {
          |  void foo(Object o) {
          |    switch (o) {
          |      case Box(String s) -> sink(s);
          |      default -> {}
          |    }
          |  }
          |}
          |""".stripMargin)

      "parse" in {
        cpg.call.name("sink").isEmpty shouldBe false
      }

      "have the correct lowering for the variable assignment" in {
        inside(
          cpg.controlStructure.controlStructureType(ControlStructureTypes.SWITCH).astChildren.isBlock.astChildren.l
        ) { case List(sLocal: Local, sAssignment: Call, _: Call) =>
          sLocal.name shouldBe "s"
          sLocal.typeFullName shouldBe "java.lang.String"
          sLocal.code shouldBe "String s"

          sAssignment.name shouldBe Operators.assignment
          sAssignment.methodFullName shouldBe Operators.assignment
          sAssignment.typeFullName shouldBe "java.lang.String"
          sAssignment.code shouldBe "s = ((Box) o).value()"

          inside(sAssignment.argument.l) { case List(sIdentifier: Identifier, valueCall: Call) =>
            sIdentifier.name shouldBe "s"
            sIdentifier.typeFullName shouldBe "java.lang.String"
            sIdentifier.code shouldBe "s"
            sIdentifier.refsTo.l shouldBe List(sLocal)

            valueCall.name shouldBe "value"
            valueCall.methodFullName shouldBe "box.Box.value:java.lang.String()"
            valueCall.typeFullName shouldBe "java.lang.String"
            valueCall.code shouldBe "((Box) o).value()"

            inside(valueCall.argument.l) { case List(castExpr: Call) =>
              castExpr.name shouldBe Operators.cast
              castExpr.methodFullName shouldBe Operators.cast
              castExpr.typeFullName shouldBe "box.Box"
              castExpr.code shouldBe "(Box) o"

              inside(castExpr.argument.l) { case List(boxType: TypeRef, oIdentifier: Identifier) =>
                boxType.typeFullName shouldBe "box.Box"
                boxType.code shouldBe "Box"

                oIdentifier.name shouldBe "o"
                oIdentifier.typeFullName shouldBe "java.lang.Object"
                oIdentifier.code shouldBe "o"
                oIdentifier.refsTo.l shouldBe cpg.method.name("foo").parameter.name("o").l
              }
            }
          }
        }
      }
    }

    "a generic, non-nested record pattern is matched" should {
      val cpg = code("""
          |package box;
          |
          |record Box<T>(T value) {}
          |
          |class Foo {
          |  void foo(Object o) {
          |    switch (o) {
          |      case Box(String s) -> sink(s);
          |      default -> {}
          |    }
          |  }
          |}
          |""".stripMargin)

      "parse" in {
        cpg.call.name("sink").isEmpty shouldBe false
      }

      "have the correct lowering for the variable assignment" in {
        inside(
          cpg.controlStructure.controlStructureType(ControlStructureTypes.SWITCH).astChildren.isBlock.astChildren.l
        ) { case List(sLocal: Local, sAssignment: Call, _: Call) =>
          sLocal.name shouldBe "s"
          sLocal.typeFullName shouldBe "java.lang.String"
          sLocal.code shouldBe "String s"

          sAssignment.name shouldBe Operators.assignment
          sAssignment.methodFullName shouldBe Operators.assignment
          sAssignment.typeFullName shouldBe "java.lang.String"
          sAssignment.code shouldBe "s = (String) ((Box) o).value()"

          inside(sAssignment.argument.l) { case List(stringCast: Call) =>
            stringCast.name shouldBe Operators.cast
            stringCast.methodFullName shouldBe Operators.cast
            stringCast.typeFullName shouldBe "java.lang.String"
            stringCast.code shouldBe "(String) ((Box) o).value()"

            inside(stringCast.argument.l) { case List(sIdentifier: Identifier, valueCall: Call) =>
              sIdentifier.name shouldBe "s"
              sIdentifier.typeFullName shouldBe "java.lang.String"
              sIdentifier.code shouldBe "s"
              sIdentifier.refsTo.l shouldBe List(sLocal)

              valueCall.name shouldBe "value"
              valueCall.methodFullName shouldBe "box.Box.value:java.lang.Object()"
              valueCall.typeFullName shouldBe "java.lang.Object"
              valueCall.code shouldBe "((Box) o).value()"

              inside(valueCall.argument.l) { case List(castExpr: Call) =>
                castExpr.name shouldBe Operators.cast
                castExpr.methodFullName shouldBe Operators.cast
                castExpr.typeFullName shouldBe "box.Box"
                castExpr.code shouldBe "(Box) o"

                inside(castExpr.argument.l) { case List(boxType: TypeRef, oIdentifier: Identifier) =>
                  boxType.typeFullName shouldBe "box.Box"
                  boxType.code shouldBe "Box"

                  oIdentifier.name shouldBe "o"
                  oIdentifier.typeFullName shouldBe "java.lang.Object"
                  oIdentifier.code shouldBe "o"
                  oIdentifier.refsTo.l shouldBe cpg.method.name("foo").parameter.name("o").l
                }
              }
            }
          }
        }
      }
    }

    "a non-generic, nested record pattern is matched" should {

      val cpg = code("""
          |package box;
          |
          |record PairBox(Pair value) {}
          |record Pair(String first, Integer second) {}
          |
          |class Foo {
          |  void foo(Object o) {
          |    switch (o) {
          |      case PairBox(Pair(String s, Integer i)) -> { sink(s); sink(i); }
          |      default -> {}
          |    }
          |  }
          |}
          |""".stripMargin)

      "parse" in {
        cpg.call.name("sink").isEmpty shouldBe false
      }

      "have the correct lowering for the variable assignment" in {
        val oParameter = cpg.method.name("foo").parameter.name("o").l
        inside(
          cpg.controlStructure.controlStructureType(ControlStructureTypes.SWITCH).astChildren.isBlock.astChildren.l
        ) { case List(sLocal: Local, iLocal: Local, sAssign: Call, iAssign: Call, sinkS: Call, sinkI: Call) =>
          sLocal.name shouldBe "s"
          sLocal.code shouldBe "s"
          sLocal.typeFullName shouldBe "java.lang.String"

          iLocal.name shouldBe "i"
          iLocal.code shouldBe "i"
          iLocal.typeFullName shouldBe "java.lang.Integer"

          sAssign.name shouldBe Operators.assignment
          sAssign.methodFullName shouldBe Operators.assignment
          sAssign.typeFullName shouldBe "java.lang.String"
          sAssign.code shouldBe "s = ((PairBox) o).value().first()"

          inside(sAssign.argument.l) { case List(sIdentifier: Identifier, firstCall: Call) =>
            sIdentifier.name shouldBe "s"
            sIdentifier.code shouldBe "s"
            sIdentifier.typeFullName shouldBe "java.lang.String"
            sIdentifier.refsTo.l shouldBe List(sLocal)

            firstCall.name shouldBe "first"
            firstCall.methodFullName shouldBe "box.Pair.first:java.lang.String()"
            firstCall.signature shouldBe "java.lang.String()"
            firstCall.typeFullName shouldBe "java.lang.String"
            firstCall.code shouldBe "((PairBox) o).value().first()"

            inside(firstCall.argument.l) { case List(valueCall: Call) =>
              valueCall.name shouldBe "value"
              valueCall.methodFullName shouldBe "box.PairBox.value:box.Pair()"
              valueCall.signature shouldBe "box.Pair()"
              valueCall.typeFullName shouldBe "box.Pair()"
              valueCall.code shouldBe "((PairBox) o).value()"

              inside(valueCall.argument.l) { case List(castExpr: Call) =>
                castExpr.name shouldBe Operators.cast
                castExpr.methodFullName shouldBe Operators.cast
                castExpr.typeFullName shouldBe "box.PairBox"
                castExpr.code shouldBe "(PairBox) o"

                inside(castExpr.argument.l) { case List(pairBoxType: TypeRef, oIdentifier: Identifier) =>
                  pairBoxType.typeFullName shouldBe "box.PairBox"
                  pairBoxType.code shouldBe "PairBox"

                  oIdentifier.name shouldBe "o"
                  oIdentifier.code shouldBe "o"
                  oIdentifier.typeFullName shouldBe "java.lang.Object"
                  oIdentifier.refsTo.l shouldBe oParameter
                }
              }
            }
          }

          iAssign.name shouldBe Operators.assignment
          iAssign.methodFullName shouldBe Operators.assignment
          iAssign.typeFullName shouldBe "java.lang.Integer"
          iAssign.code shouldBe "i = ((PairBox) o).value().second()"

          inside(iAssign.argument.l) { case List(iIdentifier: Identifier, secondCall: Call) =>
            iIdentifier.name shouldBe "i"
            iIdentifier.code shouldBe "i"
            iIdentifier.typeFullName shouldBe "java.lang.Integer"
            iIdentifier.refsTo.l shouldBe List(iLocal)

            secondCall.name shouldBe "second"
            secondCall.methodFullName shouldBe "box.Pair.second:java.lang.Integer()"
            secondCall.signature shouldBe "java.lang.Integer()"
            secondCall.typeFullName shouldBe "java.lang.Integer"
            secondCall.code shouldBe "((PairBox) o).value().second()"

            inside(secondCall.argument.l) { case List(valueCall: Call) =>
              valueCall.name shouldBe "value"
              valueCall.methodFullName shouldBe "box.PairBox.value:box.Pair()"
              valueCall.signature shouldBe "box.Pair()"
              valueCall.typeFullName shouldBe "box.Pair()"
              valueCall.code shouldBe "((PairBox) o).value()"

              inside(valueCall.argument.l) { case List(castExpr: Call) =>
                castExpr.name shouldBe Operators.cast
                castExpr.methodFullName shouldBe Operators.cast
                castExpr.typeFullName shouldBe "box.PairBox"
                castExpr.code shouldBe "(PairBox) o"

                inside(castExpr.argument.l) { case List(pairBoxType: TypeRef, oIdentifier: Identifier) =>
                  pairBoxType.typeFullName shouldBe "box.PairBox"
                  pairBoxType.code shouldBe "PairBox"

                  oIdentifier.name shouldBe "o"
                  oIdentifier.code shouldBe "o"
                  oIdentifier.typeFullName shouldBe "java.lang.Object"
                  oIdentifier.refsTo.l shouldBe oParameter
                }
              }
            }
          }

          inside(sinkS.argument.isIdentifier.name("s").l) { case List(sIdentifier: Identifier) =>
            sIdentifier.name shouldBe "s"
            sIdentifier.code shouldBe "s"
            sIdentifier.typeFullName shouldBe "java.lang.String"
            sIdentifier.refsTo.l shouldBe List(sLocal)
          }

          inside(sinkI.argument.isIdentifier.name("i").l) { case List(iIdentifier: Identifier) =>
            iIdentifier.name shouldBe "i"
            iIdentifier.code shouldBe "i"
            iIdentifier.typeFullName shouldBe "java.lang.Integer"
            iIdentifier.refsTo.l shouldBe List(iLocal)
          }
        }
      }
    }

    "a generic, nested record pattern is matched" should {

      val cpg = code("""
          |package box;
          |
          |record Box<T>(Pair value) {}
          |record Pair<U, V>(U first, V second) {}
          |
          |class Foo {
          |  void foo(Object o) {
          |    switch (o) {
          |      case Box(Pair(String s, Integer i)) -> { sink(s); sink(i); }
          |      default -> {}
          |    }
          |  }
          |}
          |""".stripMargin)

      "parse" in {
        cpg.call.name("sink").isEmpty shouldBe false
      }

      "have the correct lowering for the variable assignment" in {
        val oParameter = cpg.method.name("foo").parameter.name("o").l
        inside(
          cpg.controlStructure.controlStructureType(ControlStructureTypes.SWITCH).astChildren.isBlock.astChildren.l
        ) { case List(sLocal: Local, iLocal: Local, sAssign: Call, iAssign: Call, sSink: Call, iSink: Call) =>
          sLocal.name shouldBe "s"
          sLocal.code shouldBe "s"
          sLocal.typeFullName shouldBe "java.lang.String"

          iLocal.name shouldBe "i"
          iLocal.code shouldBe "i"
          iLocal.typeFullName shouldBe "java.lang.Integer"

          sAssign.name shouldBe Operators.assignment
          sAssign.methodFullName shouldBe Operators.assignment
          sAssign.typeFullName shouldBe "java.lang.String"
          sAssign.code shouldBe "s = (String) ((Pair) ((Box) o).value()).first()"

          inside(sAssign.argument.l) { case List(sIdentifier: Identifier, stringCast: Call) =>
            sIdentifier.name shouldBe "s"
            sIdentifier.code shouldBe "s"
            sIdentifier.typeFullName shouldBe "java.lang.String"
            sIdentifier.refsTo.l shouldBe List(sLocal)

            stringCast.name shouldBe Operators.cast
            stringCast.methodFullName shouldBe Operators.cast
            stringCast.typeFullName shouldBe "java.lang.String"
            stringCast.code shouldBe "(String) ((Pair) ((Box) o).value()).first()"

            inside(stringCast.argument.l) { case List(stringType: TypeRef, firstCall: Call) =>
              stringType.typeFullName shouldBe "java.lang.String"
              stringType.code shouldBe "String"

              firstCall.name shouldBe "first"
              firstCall.methodFullName shouldBe "box.Pair.first:java.lang.Object()"
              firstCall.signature shouldBe "java.lang.Object()"
              // TODO: Should this be the erased type?
              firstCall.typeFullName shouldBe "java.lang.Object"
              firstCall.code shouldBe "((Pair) ((Box) o).value()).first()"

              inside(firstCall.argument.l) { case List(pairCast: Call) =>
                pairCast.name shouldBe Operators.cast
                pairCast.methodFullName shouldBe Operators.cast
                pairCast.typeFullName shouldBe "box.Pair"
                pairCast.code shouldBe "(Pair) ((Box) o).value()"

                inside(pairCast.argument.l) { case List(pairType: TypeRef, valueCall: Call) =>
                  pairType.typeFullName shouldBe "box.Pair"
                  pairType.code shouldBe "Pair"

                  valueCall.name shouldBe "value"
                  valueCall.methodFullName shouldBe "box.Box.value:java.lang.Object()"
                  valueCall.signature shouldBe "java.lang.Object()"
                  valueCall.typeFullName shouldBe "java.lang.Object"
                  valueCall.code shouldBe "((Box) o).value()"

                  inside(valueCall.argument.l) { case List(boxCast: Call) =>
                    boxCast.name shouldBe Operators.cast
                    boxCast.methodFullName shouldBe Operators.cast
                    boxCast.typeFullName shouldBe "box.Box"
                    boxCast.code shouldBe "(Box) o"

                    inside(boxCast.argument.l) { case List(boxType: TypeRef, oIdentifier: Identifier) =>
                      boxType.typeFullName shouldBe "box.Box"
                      boxType.code shouldBe "Box"

                      oIdentifier.name shouldBe "o"
                      oIdentifier.code shouldBe "o"
                      oIdentifier.typeFullName shouldBe "java.lang.Object"
                      oIdentifier.refsTo.l shouldBe oParameter
                    }
                  }
                }
              }
            }
          }

          iAssign.name shouldBe Operators.assignment
          iAssign.methodFullName shouldBe Operators.assignment
          iAssign.typeFullName shouldBe "java.lang.Integer"
          iAssign.code shouldBe "i = (Integer) ((Pair) ((Box) o).value()).second()"

          inside(iAssign.argument.l) { case List(iIdentifier: Identifier, integerCast: Call) =>
            iIdentifier.name shouldBe "i"
            iIdentifier.code shouldBe "i"
            iIdentifier.typeFullName shouldBe "java.lang.Integer"
            iIdentifier.refsTo.l shouldBe List(iLocal)

            integerCast.name shouldBe Operators.cast
            integerCast.methodFullName shouldBe Operators.cast
            integerCast.typeFullName shouldBe "java.lang.Integer"
            integerCast.code shouldBe "(Integer) ((Pair) ((Box) o).value()).second()"

            inside(integerCast.argument.l) { case List(integerType: TypeRef, secondCall: Call) =>
              integerType.typeFullName shouldBe "java.lang.Integer"
              integerType.code shouldBe "Integer"

              secondCall.name shouldBe "second"
              secondCall.methodFullName shouldBe "box.Pair.second:java.lang.Object()"
              secondCall.signature shouldBe "java.lang.Object()"
              // TODO: Should this be the erased type?
              secondCall.typeFullName shouldBe "java.lang.Object"
              secondCall.code shouldBe "((Pair) ((Box) o).value()).second()"

              inside(secondCall.argument.l) { case List(pairCast: Call) =>
                pairCast.name shouldBe Operators.cast
                pairCast.methodFullName shouldBe Operators.cast
                pairCast.typeFullName shouldBe "box.Pair"
                pairCast.code shouldBe "(Pair) ((Box) o).value()"

                inside(pairCast.argument.l) { case List(pairType: TypeRef, valueCall: Call) =>
                  pairType.typeFullName shouldBe "box.Pair"
                  pairType.code shouldBe "Pair"

                  valueCall.name shouldBe "value"
                  valueCall.methodFullName shouldBe "box.Box.value:java.lang.Object()"
                  valueCall.signature shouldBe "java.lang.Object()"
                  valueCall.typeFullName shouldBe "java.lang.Object"
                  valueCall.code shouldBe "((Box) o).value()"

                  inside(valueCall.argument.l) { case List(boxCast: Call) =>
                    boxCast.name shouldBe Operators.cast
                    boxCast.methodFullName shouldBe Operators.cast
                    boxCast.typeFullName shouldBe "box.Box"
                    boxCast.code shouldBe "(Box) o"

                    inside(boxCast.argument.l) { case List(boxType: TypeRef, oIdentifier: Identifier) =>
                      boxType.typeFullName shouldBe "box.Box"
                      boxType.code shouldBe "Box"

                      oIdentifier.name shouldBe "o"
                      oIdentifier.code shouldBe "o"
                      oIdentifier.typeFullName shouldBe "java.lang.Object"
                      oIdentifier.refsTo.l shouldBe oParameter
                    }
                  }
                }
              }
            }
          }

          inside(sSink.argument.isIdentifier.name("s").l) { case List(sIdentifier: Identifier) =>
            sIdentifier.name shouldBe "s"
            sIdentifier.code shouldBe "s"
            sIdentifier.typeFullName shouldBe "java.lang.String"
            sIdentifier.refsTo.l shouldBe List(sLocal)
          }

          inside(iSink.argument.isIdentifier.name("i").l) { case List(iIdentifier: Identifier) =>
            iIdentifier.name shouldBe "i"
            iIdentifier.code shouldBe "i"
            iIdentifier.typeFullName shouldBe "java.lang.Integer"
            iIdentifier.refsTo.l shouldBe List(iLocal)
          }
        }
      }
    }
  }

  "unresolved patterns in instanceof expressions" when {

    "the pattern is a type pattern without an import fallback" should {
      val cpg = code("""
          |class Foo {
          |  void foo(Object o) {
          |    if (o instanceof Bar b) {
          |      sink(b);
          |    }
          |  }
          |}
          |""".stripMargin)

      "have the correct lowering for the type check" in {
        inside(cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).condition.l) {
          case List(oInstanceOfBar: Call) =>
            oInstanceOfBar.name shouldBe Operators.instanceOf
            oInstanceOfBar.methodFullName shouldBe Operators.instanceOf
            oInstanceOfBar.typeFullName shouldBe "boolean"
            oInstanceOfBar.code shouldBe "o instanceof Bar"

            inside(oInstanceOfBar.argument.l) { case List(oIdentifier: Identifier, barType: TypeRef) =>
              oIdentifier.name shouldBe "o"
              oIdentifier.code shouldBe "o"
              oIdentifier.typeFullName shouldBe "java.lang.Object"
              oIdentifier.refsTo.l shouldBe cpg.method.name("foo").parameter.name("o").l

              barType.typeFullName shouldBe "ANY"
              barType.code shouldBe "Bar"
            }
        }
      }

      "have the correct lowering for the variable assignment" in {
        inside(cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).astChildren.isBlock.astChildren.l) {
          case List(bLocal: Local, bAssign: Call, bSink: Call) =>
            bLocal.name shouldBe "b"
            bLocal.code shouldBe "Bar b"
            bLocal.typeFullName shouldBe "ANY"

            bAssign.name shouldBe Operators.assignment
            bAssign.methodFullName shouldBe Operators.assignment
            bAssign.typeFullName shouldBe "ANY"
            bAssign.code shouldBe "b = Bar.<unresolvedField>"

            inside(bAssign.argument.l) { case List(bIdentifier: Identifier, fieldAccess: Call) =>
              bIdentifier.name shouldBe "b"
              bIdentifier.code shouldBe "b"
              bIdentifier.typeFullName shouldBe "ANY"
              bIdentifier.refsTo.l shouldBe List(bLocal)

              fieldAccess.name shouldBe Operators.fieldAccess
              fieldAccess.methodFullName shouldBe Operators.fieldAccess
              fieldAccess.typeFullName shouldBe "ANY"
              fieldAccess.code shouldBe "Bar.<unresolvedField>"

              inside(fieldAccess.argument.l) {
                case List(barIdentifier: Identifier, unresolvedFieldIdentifier: FieldIdentifier) =>
                  barIdentifier.name shouldBe "Bar"
                  barIdentifier.code shouldBe "Bar"
                  barIdentifier.typeFullName shouldBe "ANY"

                  unresolvedFieldIdentifier.canonicalName shouldBe "<unresolvedField>"
              }
            }

            bSink.argument.isIdentifier.name("b").refsTo.l shouldBe List(bLocal)
        }
      }
    }

    "the pattern is a type pattern with an import fallback" should {
      val cpg = code("""
          |import bar.Bar;
          |
          |class Foo {
          |  void foo(Object o) {
          |    if (o instanceof Bar b) {
          |      sink(b);
          |    }
          |  }
          |}
          |""".stripMargin)

      "have the correct lowering for the type check" in {
        inside(cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).condition.l) {
          case List(oInstanceOfBar: Call) =>
            oInstanceOfBar.name shouldBe Operators.instanceOf
            oInstanceOfBar.methodFullName shouldBe Operators.instanceOf
            oInstanceOfBar.typeFullName shouldBe "boolean"
            oInstanceOfBar.code shouldBe "o instanceof Bar"

            inside(oInstanceOfBar.argument.l) { case List(oIdentifier: Identifier, barType: TypeRef) =>
              oIdentifier.name shouldBe "o"
              oIdentifier.code shouldBe "o"
              oIdentifier.typeFullName shouldBe "java.lang.Object"
              oIdentifier.refsTo.l shouldBe cpg.method.name("foo").parameter.name("o").l

              barType.typeFullName shouldBe "bar.Bar"
              barType.code shouldBe "Bar"
            }
        }
      }

      "have the correct lowering for the variable assignment" in {
        inside(cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).astChildren.isBlock.astChildren.l) {
          case List(bLocal: Local, bAssign: Call, bSink: Call) =>
            bLocal.name shouldBe "b"
            bLocal.code shouldBe "Bar b"
            bLocal.typeFullName shouldBe "bar.Bar"

            bAssign.name shouldBe Operators.assignment
            bAssign.methodFullName shouldBe Operators.assignment
            bAssign.typeFullName shouldBe "ANY"
            bAssign.code shouldBe "b = Bar.<unresolvedField>"

            inside(bAssign.argument.l) { case List(bIdentifier: Identifier, fieldAccess: Call) =>
              bIdentifier.name shouldBe "b"
              bIdentifier.code shouldBe "b"
              bIdentifier.typeFullName shouldBe "bar.Bar"
              bIdentifier.refsTo.l shouldBe List(bLocal)

              fieldAccess.name shouldBe Operators.fieldAccess
              fieldAccess.methodFullName shouldBe Operators.fieldAccess
              fieldAccess.typeFullName shouldBe "ANY"
              fieldAccess.code shouldBe "Bar.<unresolvedField>"

              inside(fieldAccess.argument.l) {
                case List(barIdentifier: Identifier, unresolvedFieldIdentifier: FieldIdentifier) =>
                  barIdentifier.name shouldBe "Bar"
                  barIdentifier.code shouldBe "Bar"
                  barIdentifier.typeFullName shouldBe "bar.Bar"

                  unresolvedFieldIdentifier.canonicalName shouldBe "<unresolvedField>"
              }
            }

            bSink.argument.isIdentifier.name("b").refsTo.l shouldBe List(bLocal)
        }
      }
    }

    "the pattern is a nested record pattern" should {
      val cpg = code("""
          |class Foo {
          |  void foo(Object o) {
          |    if (o instanceof Bar(Baz(Qux q))) {
          |      sink(q);
          |    }
          |  }
          |}
          |""".stripMargin)

      "have the correct lowering for the type check" in {
        inside(cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).condition.l) {
          case List(oInstanceOfBar: Call) =>
            oInstanceOfBar.name shouldBe Operators.instanceOf
            oInstanceOfBar.methodFullName shouldBe Operators.instanceOf
            oInstanceOfBar.typeFullName shouldBe "boolean"
            oInstanceOfBar.code shouldBe "o instanceof Bar(Baz(Qux q))"

            inside(oInstanceOfBar.argument.l) { case List(oIdentifier: Identifier, barType: TypeRef) =>
              oIdentifier.name shouldBe "o"
              oIdentifier.code shouldBe "o"
              oIdentifier.typeFullName shouldBe "java.lang.Object"
              oIdentifier.refsTo.l shouldBe cpg.method.name("foo").parameter.name("o").l

              barType.typeFullName shouldBe "ANY"
              barType.code shouldBe "Bar"
            }
        }
      }

      "have the correct lowering for the variable assignment" in {
        inside(cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).astChildren.isBlock.astChildren.l) {
          case List(qLocal: Local, qAssign: Call, qSink: Call) =>
            qLocal.name shouldBe "q"
            qLocal.code shouldBe "q"
            qLocal.typeFullName shouldBe "ANY"

            qAssign.name shouldBe Operators.assignment
            qAssign.methodFullName shouldBe Operators.assignment
            qAssign.typeFullName shouldBe "ANY"
            qAssign.code shouldBe "q = ((Bar) o).<unresolvedField>.<unresolvedField>"

            inside(qAssign.argument.l) { case List(qIdentifier: Identifier, firstFieldAccess: Call) =>
              qIdentifier.name shouldBe "q"
              qIdentifier.code shouldBe "q"
              qIdentifier.typeFullName shouldBe "ANY"
              qIdentifier.refsTo.l shouldBe List(qLocal)

              firstFieldAccess.name shouldBe Operators.fieldAccess
              firstFieldAccess.methodFullName shouldBe Operators.fieldAccess
              firstFieldAccess.typeFullName shouldBe "ANY"
              firstFieldAccess.code shouldBe "((Bar) o).<unresolvedField>.<unresolvedField>"

              inside(firstFieldAccess.argument.l) {
                case List(secondFieldAccess: Call, unresolvedFieldIdentifier: FieldIdentifier) =>
                  secondFieldAccess.name shouldBe Operators.fieldAccess
                  secondFieldAccess.methodFullName shouldBe Operators.fieldAccess
                  secondFieldAccess.typeFullName shouldBe "ANY"
                  secondFieldAccess.code shouldBe "((Bar) o).<unresolvedField>"

                  inside(secondFieldAccess.argument.l) {
                    case List(castExpr: Call, unresolvedFieldIdentifier: FieldIdentifier) =>
                      castExpr.name shouldBe Operators.cast
                      castExpr.methodFullName shouldBe Operators.cast
                      castExpr.typeFullName shouldBe "ANY"
                      castExpr.code shouldBe "(Bar) o"

                      inside(castExpr.argument.l) { case List(barType: TypeRef, oIdentifier: Identifier) =>
                        barType.typeFullName shouldBe "ANY"
                        barType.code shouldBe "Bar"

                        oIdentifier.name shouldBe "o"
                        oIdentifier.code shouldBe "o"
                        oIdentifier.typeFullName shouldBe "java.lang.Object"
                        oIdentifier.refsTo.l shouldBe cpg.method.name("foo").parameter.name("o").l
                      }

                      unresolvedFieldIdentifier.canonicalName shouldBe "<unresolvedField>"
                      unresolvedFieldIdentifier.code shouldBe "<unresolvedField>"
                  }

                  unresolvedFieldIdentifier.canonicalName shouldBe "<unresolvedField>"
                  unresolvedFieldIdentifier.code shouldBe "<unresolvedField>"
              }
            }
        }
      }
    }
  }

}
