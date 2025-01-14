package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCode2CpgFixture
import io.joern.javasrc2cpg.typesolvers.TypeInfoCalculator.TypeConstants
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{
  Block,
  Call,
  ControlStructure,
  FieldIdentifier,
  Identifier,
  JumpTarget,
  Literal,
  Local,
  TypeRef
}
import io.shiftleft.semanticcpg.language.*

class PatternExprTests extends JavaSrcCode2CpgFixture {

  "a pattern initializer in a lambda method" should {
    val cpg = code("""
        |import java.util.function.Function;
        |
        |class Foo {
        |  Function test() {
        |    return o -> foo() instanceof String s ? s : null;
        |  }
        |}
        |""".stripMargin)

    "not create any orphan locals" in {
      cpg.local.exists(_._astIn.isEmpty) shouldBe false
    }
  }

  "a type pattern in an expression in an explicit constructor" should {
    val cpg = code("""
        |class Test {
        |  Object foo() {
        |    return "abc";
        |  }
        |
        |  public Test() {
        |    boolean b = foo() instanceof String s;
        |  }
        |}
        |""".stripMargin)

    "parse" in {
      cpg.call.name("foo").nonEmpty shouldBe true
    }

    "be represented correctly" in {
      inside(cpg.method.name(".*init.*").body.astChildren.l) {
        case List(tmpLocal: Local, sLocal: Local, sAssign: Call, bLocal: Local, bAssign: Call) =>
          tmpLocal.name shouldBe "$obj0"

          bLocal.name shouldBe "b"

          // TODO should s assignment be added if it is never used
          // TODO sAssign.code shouldBe "s = (String) $obj0"

          sLocal.name shouldBe "s"

          // TODO bAssign code
          bAssign.methodFullName shouldBe Operators.assignment
          inside(bAssign.argument.l) { case List(bIdentifier: Identifier, instanceOfCall: Call) =>
            bIdentifier.name shouldBe "b"
            bIdentifier.typeFullName shouldBe "boolean"
            bIdentifier.refsTo.l shouldBe List(bLocal)

            instanceOfCall.methodFullName shouldBe Operators.instanceOf

            inside(instanceOfCall.argument.l) { case List(tmpAssign: Call, stringType: TypeRef) =>
              tmpAssign.methodFullName shouldBe Operators.assignment
              // TODO tmpAssign code

              inside(tmpAssign.argument.l) { case List(tmpIdentifier: Identifier, fooCall: Call) =>
                tmpIdentifier.name shouldBe "$obj0"
                tmpIdentifier.typeFullName shouldBe "java.lang.Object"
                tmpIdentifier.refsTo.l shouldBe List(tmpLocal)

                fooCall.name shouldBe "foo"
                fooCall.methodFullName shouldBe "Test.foo:java.lang.Object()"
              }
            }
          }
      }
    }
  }

  "a pattern matching instanceof in a field initializer" should {
    val cpg = code("""
                       |import foo.Foo;
                       |
                       |class Test {
                       |    public int x = Foo.FOO instanceof String s ? s.length() : -1;
                       |}
                       |""".stripMargin)
      .moreCode("""
          |package foo;
          |
          |public class Foo {
          |  public Object FOO = "abc";
          |}
          |""".stripMargin)
    "parse" in {
      cpg.call.name("length").nonEmpty shouldBe true
    }

    "add the local and initialiser for the pattern variable to the <init> method" in {
      inside(cpg.typeDecl.name("Test").method.nameExact("<init>").body.astChildren.l) {
        case List(sLocal: Local, xAssign: Call) =>
          sLocal.name shouldBe "s"
          sLocal.typeFullName shouldBe "java.lang.String"

          xAssign.methodFullName shouldBe Operators.assignment

          inside(xAssign.argument.l) { case List(xFieldAccess: Call, conditionalExpr: Call) =>
            xFieldAccess.methodFullName shouldBe Operators.fieldAccess
            // TODO xFieldAccess test

            conditionalExpr.methodFullName shouldBe Operators.conditional
            conditionalExpr.typeFullName shouldBe "int"

            inside(conditionalExpr.argument.l) { case List(instanceOfCall: Call, lengthCall: Call, minusCall: Call) =>
              instanceOfCall.methodFullName shouldBe Operators.instanceOf
              instanceOfCall.code shouldBe "Foo.FOO instanceof String"

              inside(instanceOfCall.argument.l) { case List(fooFieldAccess: Call, stringType: TypeRef) =>
                fooFieldAccess.code shouldBe "Foo.FOO"

                // TODO: Fix static field access arguments
                // inside(fooFieldAccess.argument.l) {
                //   case List(fooType: TypeRef, fooFieldName: FieldIdentifier) =>
                //     fooType.typeFullName shouldBe "foo.Foo"

                //     fooFieldName.canonicalName shouldBe "FOO"
                // }

                stringType.typeFullName shouldBe "java.lang.String"
              }

              lengthCall.methodFullName shouldBe "java.lang.String.length:int()"
              inside(lengthCall.argument.l) { case List(sAssign: Call) =>
                sAssign.name shouldBe Operators.assignment

                inside(sAssign.argument.l) { case List(sIdentifier: Identifier, oCast: Call) =>
                  sIdentifier.name shouldBe "s"
                  sIdentifier.refsTo.l shouldBe List(sLocal)

                // TODO oCast.code shouldBe "(String) $obj0"
                }
              }

              minusCall.methodFullName shouldBe Operators.minus
            }
          }
      }
    }
  }

  "a pattern matching instanceof in a static field initializer" should {
    val cpg = code("""
                       |import foo.Foo;
                       |
                       |class Test {
                       |    public static int x = Foo.FOO instanceof String s ? s.length() : -1;
                       |}
                       |""".stripMargin)
      .moreCode("""
                    |package foo;
                    |
                    |public class Foo {
                    |  public Object FOO = "abc";
                    |}
                    |""".stripMargin)
    "parse" in {
      cpg.call.name("length").nonEmpty shouldBe true
    }

    "add the local and initialiser for the pattern variable to the <clinit> method" in {
      inside(cpg.typeDecl.name("Test").method.nameExact("<clinit>").body.astChildren.l) {
        case List(sLocal: Local, xAssign: Call) =>
          sLocal.name shouldBe "s"
          sLocal.typeFullName shouldBe "java.lang.String"

          xAssign.methodFullName shouldBe Operators.assignment
          // TODO xAssign code

          inside(xAssign.argument.l) { case List(xFieldAccess: Call, conditionalExpr: Call) =>
            xFieldAccess.methodFullName shouldBe Operators.fieldAccess
            // TODO xFieldAccess test

            conditionalExpr.methodFullName shouldBe Operators.conditional
            conditionalExpr.typeFullName shouldBe "int"

            inside(conditionalExpr.argument.l) { case List(instanceOfCall: Call, lengthCall: Call, minusCall: Call) =>
              instanceOfCall.methodFullName shouldBe Operators.instanceOf
              inside(instanceOfCall.argument.l) { case List(fooFieldAccess: Call, stringType: TypeRef) =>
                fooFieldAccess.code shouldBe "Foo.FOO"

                stringType.typeFullName shouldBe "java.lang.String"
              }

              lengthCall.methodFullName shouldBe "java.lang.String.length:int()"
              inside(lengthCall.argument.l) { case List(sAssign: Call) =>
                sAssign.name shouldBe Operators.assignment

                inside(sAssign.argument.l) { case List(sIdentifier: Identifier, oCast: Call) =>
                  sIdentifier.name shouldBe "s"
                  sIdentifier.refsTo.l shouldBe List(sLocal)

                // TODO oCast.code shouldBe "(String) $obj0"
                }
              }

              minusCall.methodFullName shouldBe Operators.minus
            }
          }
      }
    }
  }

  "a pattern matching instanceof with a call lhs" should {
    val cpg = code("""
                       |class Test {
                       |  static String foo() {
                       |    return "Hello, world!";
                       |  }
                       |
                       |  static void sink(String s) { /* Do nothing */ }
                       |
                       |  void test(Object o) {
                       |    if (foo() instanceof String s && s.isEmpty()) {
                       |      sink(s);
                       |    }
                       |  }
                       |}
                       |""".stripMargin)

    "parse" in {
      cpg.call.name("sink").size shouldBe 1
    }

    "add a tmp local for the foo call to the start of the method" in {
      inside(cpg.method.name("test").body.astChildren.l) { case (tmpLocal: Local) :: _ =>
        tmpLocal.name shouldBe "$obj0"
        tmpLocal.code shouldBe "$obj0"
        tmpLocal.typeFullName shouldBe "java.lang.String"
      }
    }

    "create an assignment for the temporary local as the first instanceof argument" in {
      inside(cpg.call.nameExact(Operators.instanceOf).argument.head) { case assignment: Call =>
        assignment.name shouldBe Operators.assignment
        assignment.typeFullName shouldBe "java.lang.String"
        assignment.code shouldBe "$obj0 = foo()"

        inside(assignment.argument.l) { case List(tmpIdentifier: Identifier, fooCall: Call) =>
          tmpIdentifier.name shouldBe "$obj0"
          tmpIdentifier.code shouldBe "$obj0"
          tmpIdentifier.typeFullName shouldBe "java.lang.String"
          tmpIdentifier.refsTo.l shouldBe cpg.local.nameExact("$obj0").l

          fooCall.name shouldBe "foo"
          fooCall.methodFullName shouldBe "Test.foo:java.lang.String()"
          fooCall.typeFullName shouldBe "java.lang.String"
          fooCall.code shouldBe "foo()"
        }
      }
    }
  }

  "patterns in binary expressions" when {
    "a variable is introduced to the RHS of an && expression" should {

      val cpg = code("""
                         |class Test {
                         |  void test(Object o) {
                         |    if (o instanceof String s && s.isEmpty()) {
                         |      System.out.println(s);
                         |    }
                         |  }
                         |}
                         |""".stripMargin)

      "parse" in {
        cpg.call.name("isEmpty").nonEmpty shouldBe true
      }

      "not have any nodes with multiple AST parents" in {
        cpg.astNode.filter(_._astIn.size > 1).l shouldBe Nil
      }

      "be represented correctly" in {
        inside(cpg.method.name("test").body.astChildren.l) { case List(sLocal: Local, ifStmt: ControlStructure) =>
          sLocal.name shouldBe "s"
          sLocal.typeFullName shouldBe "java.lang.String"
          sLocal.code shouldBe "String s"

          inside(ifStmt.condition.l) { case List(andCall: Call) =>
            andCall.name shouldBe Operators.logicalAnd
            andCall.typeFullName shouldBe TypeConstants.Boolean
            // TODO fix code
            // andCall.code shouldBe "o instanceof String s && (s = (String) o).isEmpty()"

            inside(andCall.argument.l) { case List(instanceOfCall: Call, isEmptyCall: Call) =>
              instanceOfCall.name shouldBe Operators.instanceOf
              instanceOfCall.code shouldBe "o instanceof String"

              inside(instanceOfCall.argument.l) { case List(oIdentifier: Identifier, stringType: TypeRef) =>
                oIdentifier.name shouldBe "o"
                oIdentifier.typeFullName shouldBe "java.lang.Object"

                stringType.typeFullName shouldBe "java.lang.String"
              }

              isEmptyCall.name shouldBe "isEmpty"
              isEmptyCall.methodFullName shouldBe "java.lang.String.isEmpty:boolean()"
              // TODO Fix code
              // isEmptyCall.code shouldBe "(s = (String) o).isEmpty()"

              inside(isEmptyCall.argument.l) { case List(sAssignment: Call) =>
                sAssignment.name shouldBe Operators.assignment
                // TODO Fix code
                // sAssignment.code shouldBe "s = (String) o"
                sAssignment.typeFullName shouldBe "java.lang.String"

                inside(sAssignment.argument.l) { case List(sIdentifier: Identifier, castCall: Call) =>
                  sIdentifier.name shouldBe "s"
                  sIdentifier.typeFullName shouldBe "java.lang.String"
                  sIdentifier.refsTo.l shouldBe cpg.method.name("test").local.name("s").l

                  castCall.name shouldBe Operators.cast
                  castCall.methodFullName shouldBe Operators.cast
                  castCall.code shouldBe "(String) o"

                  inside(castCall.argument.l) { case List(innerStringType: TypeRef, innerOIdentifier: Identifier) =>
                    innerStringType.typeFullName shouldBe "java.lang.String"

                    innerOIdentifier.name shouldBe "o"
                    innerOIdentifier.typeFullName shouldBe "java.lang.Object"
                    innerOIdentifier.refsTo.l shouldBe cpg.method.name("test").parameter.name("o").l
                  }
                }
              }
            }
          }
        }
      }
    }

    "a variable is introduced to the RHS of an || expression" should {
      val cpg = code("""
                         |class Test {
                         |  void test(Object o) {
                         |    if (!(o instanceof String s) || s.isEmpty()) {
                         |      System.out.println("no input found");
                         |    }
                         |  }
                         |}
                         |""".stripMargin)

      "parse" in {
        cpg.call.name("isEmpty").nonEmpty shouldBe true
      }

      "be represented correctly" in {
        inside(cpg.method.name("test").body.astChildren.l) { case List(sLocal: Local, ifStmt: ControlStructure) =>
          sLocal.name shouldBe "s"
          sLocal.typeFullName shouldBe "java.lang.String"
          sLocal.code shouldBe "String s"

          inside(ifStmt.condition.l) { case List(orCall: Call) =>
            orCall.name shouldBe Operators.logicalOr
            orCall.typeFullName shouldBe TypeConstants.Boolean

            // TODO fix code
            // orCall.code shouldBe "!(o instanceof String s) || (s = (String) o).isEmpty()"

            inside(orCall.argument.l) { case List(notCall: Call, isEmptyCall: Call) =>
              notCall.code shouldBe "!(o instanceof String s)"
              inside(notCall.argument.l) { case List(instanceOfCall: Call) =>
                instanceOfCall.name shouldBe Operators.instanceOf
                instanceOfCall.code shouldBe "o instanceof String"

                inside(instanceOfCall.argument.l) { case List(oIdentifier: Identifier, stringType: TypeRef) =>
                  oIdentifier.name shouldBe "o"
                  oIdentifier.typeFullName shouldBe "java.lang.Object"

                  stringType.typeFullName shouldBe "java.lang.String"
                }
              }

              isEmptyCall.name shouldBe "isEmpty"
              isEmptyCall.methodFullName shouldBe "java.lang.String.isEmpty:boolean()"
              // TODO fix code: is currently s = (String) o.isEmpty()
              // isEmptyCall.code shouldBe "(s = (String) o).isEmpty()"

              inside(isEmptyCall.argument.l) { case List(sAssignment: Call) =>
                sAssignment.name shouldBe Operators.assignment
                sAssignment.code shouldBe "s = (String) o"
                sAssignment.typeFullName shouldBe "java.lang.String"

                inside(sAssignment.argument.l) { case List(sIdentifier: Identifier, castCall: Call) =>
                  sIdentifier.name shouldBe "s"
                  sIdentifier.typeFullName shouldBe "java.lang.String"
                  sIdentifier.refsTo.l shouldBe cpg.method.name("test").local.name("s").l

                  castCall.name shouldBe Operators.cast
                  castCall.methodFullName shouldBe Operators.cast
                  castCall.code shouldBe "(String) o"

                  inside(castCall.argument.l) { case List(innerStringType: TypeRef, innerOIdentifier: Identifier) =>
                    innerStringType.typeFullName shouldBe "java.lang.String"

                    innerOIdentifier.name shouldBe "o"
                    innerOIdentifier.typeFullName shouldBe "java.lang.Object"
                    innerOIdentifier.refsTo.l shouldBe cpg.method.name("test").parameter.name("o").l
                  }
                }
              }
            }
          }
        }
      }
    }

    "a variable is introduced to the RHS of an && expression, mutated and introduced to the body of an if" should {
      val cpg = code("""
                         |class Test {
                         |  static void test(Object o) {
                         |    if (o instanceof String value && (value = "Foo").isEmpty()) {
                         |      System.out.println(value);
                         |    }
                         |  }
                         |}
                         |""".stripMargin)

      "parse" in {
        cpg.call.name("isEmpty").nonEmpty shouldBe true
      }

      "be represented correctly" in {
        inside(cpg.method.name("test").body.astChildren.l) { case List(valueLocal: Local, ifStmt: ControlStructure) =>
          valueLocal.name shouldBe "value"
          valueLocal.code shouldBe "String value"
          valueLocal.typeFullName shouldBe "java.lang.String"

          // TODO Fix code
          // ifStmt.code shouldBe "if (o instanceof String value && ((value = (String) o) = \"Foo\").isEmpty())"

          inside(ifStmt.condition.l) { case List(andCall: Call) =>
            andCall.name shouldBe Operators.logicalAnd
            andCall.methodFullName shouldBe Operators.logicalAnd
            // TODO Test code

            inside(andCall.astChildren.l) { case List(instanceOfCall: Call, isEmptyCall: Call) =>
              instanceOfCall.name shouldBe Operators.instanceOf
              instanceOfCall.code shouldBe "o instanceof String"

              isEmptyCall.name shouldBe "isEmpty"
              isEmptyCall.methodFullName shouldBe "java.lang.String.isEmpty:boolean()"

              inside(isEmptyCall.argument.l) { case List(fooAssignment: Call) =>
                fooAssignment.name shouldBe Operators.assignment
                // TODO Test code

                inside(fooAssignment.argument.l) { case List(valueAssign: Call, fooLiteral: Literal) =>
                  valueAssign.methodFullName shouldBe Operators.assignment
                  valueAssign.code shouldBe "value = (String) o"

                  inside(valueAssign.argument.l) { case List(valueIdentifier: Identifier, oCast: Call) =>
                    valueIdentifier.name shouldBe "value"
                    valueIdentifier.typeFullName shouldBe "java.lang.String"
                    valueIdentifier.code shouldBe "value"
                    valueIdentifier.refsTo.l shouldBe List(valueLocal)

                    oCast.code shouldBe "(String) o"
                  }
                }
              }
            }
          }
        }
      }
    }

    "a variable is introduced to the RHS of an || expression, mutated and introduced by an if" should {
      val cpg = code("""
                         |class Test {
                         |  static void test(Object o) {
                         |    if (!(o instanceof String value) || (value = "Foo").isEmpty()) {
                         |      return;
                         |    }
                         |    System.out.println(value);
                         |  }
                         |}
                         |""".stripMargin)

      "parse" in {
        cpg.call.name("isEmpty").nonEmpty shouldBe true
      }

      "be represented correctly" in {
        inside(cpg.method.name("test").body.astChildren.l) {
          case List(valueLocal: Local, ifStmt: ControlStructure, printCall: Call) =>
            valueLocal.name shouldBe "value"
            valueLocal.code shouldBe "String value"
            valueLocal.typeFullName shouldBe "java.lang.String"

            // TODO Fix code
            // ifStmt.code shouldBe "if (o instanceof String value && ((value = (String) o) = \"Foo\").isEmpty())"

            inside(ifStmt.condition.l) { case List(orCall: Call) =>
              orCall.name shouldBe Operators.logicalOr
              orCall.methodFullName shouldBe Operators.logicalOr
              // TODO Test code

              inside(orCall.astChildren.l) { case List(notCall: Call, isEmptyCall: Call) =>
                notCall.methodFullName shouldBe Operators.logicalNot
                // TODO Test code

                inside(notCall.argument.l) { case List(instanceOfCall: Call) =>
                  instanceOfCall.name shouldBe Operators.instanceOf
                  instanceOfCall.code shouldBe "o instanceof String"
                }

                isEmptyCall.name shouldBe "isEmpty"
                isEmptyCall.methodFullName shouldBe "java.lang.String.isEmpty:boolean()"

                inside(isEmptyCall.argument.l) { case List(fooAssignment: Call) =>
                  fooAssignment.name shouldBe Operators.assignment
                  // TODO Test code

                  inside(fooAssignment.argument.l) { case List(valueAssign: Call, fooLiteral: Literal) =>
                    valueAssign.methodFullName shouldBe Operators.assignment
                    valueAssign.code shouldBe "value = (String) o"

                    inside(valueAssign.argument.l) { case List(valueIdentifier: Identifier, oCast: Call) =>
                      valueIdentifier.name shouldBe "value"
                      valueIdentifier.typeFullName shouldBe "java.lang.String"
                      valueIdentifier.code shouldBe "value"
                      valueIdentifier.refsTo.l shouldBe List(valueLocal)

                      oCast.code shouldBe "(String) o"
                    }
                  }
                }
              }
            }

            printCall.name shouldBe "println"
            inside(printCall.argument.l) { case List(systemOutFieldAccess: Call, valueIdentifier: Identifier) =>
              systemOutFieldAccess.code shouldBe "System.out"

              valueIdentifier.name shouldBe "value"
              valueIdentifier.typeFullName shouldBe "java.lang.String"
              valueIdentifier.code shouldBe "value"
              valueIdentifier.refsTo.l shouldBe List(valueLocal)
            }
        }
      }
    }
  }

  "patterns in ternary expressions" when {
    "a variable is introduced to the then expression" should {
      val cpg = code("""
          |class Test {
          |  void test(Object o) {
          |    int x = o instanceof String s ? s.length() : -1;
          |  }
          |}
          |""".stripMargin)

      "parse" in {
        cpg.call.name("length").nonEmpty shouldBe true
      }

      "be represented correctly" in {
        inside(cpg.method.name("test").body.astChildren.l) { case List(sLocal: Local, xLocal: Local, xAssign: Call) =>
          sLocal.name shouldBe "s"
          sLocal.typeFullName shouldBe "java.lang.String"

          xLocal.name shouldBe "x"
          xLocal.typeFullName shouldBe "int"

          xAssign.methodFullName shouldBe Operators.assignment

          inside(xAssign.argument.l) { case List(xIdentifier: Identifier, ternaryExpr: Call) =>
            xIdentifier.name shouldBe "x"

            ternaryExpr.methodFullName shouldBe Operators.conditional
            inside(ternaryExpr.argument.l) { case List(instanceOfCall: Call, lengthCall: Call, minusCall: Call) =>
              instanceOfCall.name shouldBe Operators.instanceOf
              instanceOfCall.code shouldBe "o instanceof String"

              // TODO Test code
              lengthCall.name shouldBe "length"
              lengthCall.methodFullName shouldBe "java.lang.String.length:int()"
              inside(lengthCall.argument.l) { case List(sAssign: Call) =>
                sAssign.methodFullName shouldBe Operators.assignment
                sAssign.typeFullName shouldBe "java.lang.String"
                sAssign.code shouldBe "s = (String) o"

                inside(sAssign.argument.l) { case List(sIdentifier: Identifier, oCast: Call) =>
                  sIdentifier.name shouldBe "s"
                  sIdentifier.typeFullName shouldBe "java.lang.String"
                  sIdentifier.refsTo.l shouldBe List(sLocal)

                  oCast.name shouldBe Operators.cast
                  oCast.code shouldBe "(String) o"
                }
              }

              minusCall.name shouldBe Operators.minus
            }
          }
        }
      }
    }

    "a variable is introduced to the else expression" should {
      val cpg = code("""
                         |class Test {
                         |  void test(Object o) {
                         |    int x = !(o instanceof String s) ? -1: s.length();
                         |  }
                         |}
                         |""".stripMargin)

      "parse" in {
        cpg.call.name("length").nonEmpty shouldBe true
      }

      "be represented correctly" in {
        inside(cpg.method.name("test").body.astChildren.l) { case List(sLocal: Local, xLocal: Local, xAssign: Call) =>
          sLocal.name shouldBe "s"
          sLocal.typeFullName shouldBe "java.lang.String"

          xLocal.name shouldBe "x"
          xLocal.typeFullName shouldBe "int"

          xAssign.methodFullName shouldBe Operators.assignment

          inside(xAssign.argument.l) { case List(xIdentifier: Identifier, ternaryExpr: Call) =>
            xIdentifier.name shouldBe "x"

            ternaryExpr.methodFullName shouldBe Operators.conditional
            inside(ternaryExpr.argument.l) { case List(notCall: Call, minusCall: Call, lengthCall: Call) =>
              notCall.methodFullName shouldBe Operators.logicalNot

              inside(notCall.argument.l) { case List(instanceOfCall: Call) =>
                instanceOfCall.name shouldBe Operators.instanceOf
                instanceOfCall.code shouldBe "o instanceof String"
              }

              // TODO Test code
              lengthCall.name shouldBe "length"
              lengthCall.methodFullName shouldBe "java.lang.String.length:int()"
              inside(lengthCall.argument.l) { case List(sAssign: Call) =>
                sAssign.methodFullName shouldBe Operators.assignment
                sAssign.typeFullName shouldBe "java.lang.String"
                sAssign.code shouldBe "s = (String) o"

                inside(sAssign.argument.l) { case List(sIdentifier: Identifier, oCast: Call) =>
                  sIdentifier.name shouldBe "s"
                  sIdentifier.typeFullName shouldBe "java.lang.String"
                  sIdentifier.refsTo.l shouldBe List(sLocal)

                  oCast.name shouldBe Operators.cast
                  oCast.code shouldBe "(String) o"
                }
              }

              minusCall.name shouldBe Operators.minus
            }
          }
        }
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
          case List(_: Call, _: Block, elseStructure: ControlStructure) =>
            inside(elseStructure.astChildren.l) { case List(elseBlock: Block) =>
              elseBlock.ast.isLocal.name("s").typeFullName.l shouldBe List("java.lang.String")
            }
        }
      }

      "create the s assignment in the then block" in {
        inside(cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).astChildren.l) {
          case List(_: Call, _: Block, elseStructure: ControlStructure) =>
            inside(elseStructure.astChildren.l) { case List(elseBlock: Block) =>
            }
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
      val cpg = code("""
          |class Foo {
          |  void foo(Object o) {
          |    for(int i = 0; o instanceof String s && i < 42; i += s.length()) {
          |      System.out.println(i);
          |    }
          |  }
          |}
          |""".stripMargin)

      "be represented correctly" in {
        inside(cpg.method.name("foo").body.astChildren.l) { case List(sLocal: Local, forStmt: ControlStructure) =>
          sLocal.name shouldBe "s"
          sLocal.code shouldBe "String s"
          sLocal.typeFullName shouldBe "java.lang.String"

          forStmt.controlStructureType shouldBe ControlStructureTypes.FOR
          inside(forStmt.astChildren.l) {
            case List(iLocal: Local, iAssign: Call, condition: Call, update: Call, body: Block) =>
              iLocal.name shouldBe "i"

              iAssign.methodFullName shouldBe Operators.assignment
              iAssign.code shouldBe "int i = 0"

              condition.methodFullName shouldBe Operators.logicalAnd
              // TODO Check LHS arg

              update.methodFullName shouldBe Operators.assignmentPlus
              inside(update.argument.l) { case List(iIdentifier: Identifier, lengthCall: Call) =>
                iIdentifier.name shouldBe "i"
                iIdentifier.refsTo.l shouldBe List(iLocal)

                lengthCall.name shouldBe "length"
                lengthCall.methodFullName shouldBe "java.lang.String.length:int()"
                // TODO Test code
                // TODO This representation is technically not correct. It's possible to
                inside(lengthCall.argument.l) { case List(sAssignment: Call) =>
                  sAssignment.methodFullName shouldBe Operators.assignment
                  sAssignment.code shouldBe "s = (String) o"
                }
              }

              inside(body.astChildren.l) { case List(printlnCall: Call) =>
                printlnCall.code shouldBe "System.out.println(i)"
              }

          }
        }
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
                         |    for (; !(o instanceof String s);) {
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
            sAssign.code shouldBe "s = (String) o"

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
              valueCall.code shouldBe "((Box) o).value()"
              valueCall.typeFullName shouldBe "java.lang.String"

              inside(valueCall.argument.l) { case List(boxCast: Call) =>
                boxCast.name shouldBe Operators.cast
                boxCast.code shouldBe "(Box) o"
                boxCast.typeFullName shouldBe "box.Box"

                inside(boxCast.argument.l) { case List(boxType: TypeRef, oIdentifier: Identifier) =>
                  boxType.typeFullName shouldBe "box.Box"

                  oIdentifier.name shouldBe "o"
                  oIdentifier.typeFullName shouldBe "java.lang.Object"
                  oIdentifier.refsTo.l shouldBe cpg.parameter.name("o").l
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
            andCall.code shouldBe "(o instanceof Box) && (($obj0 = ((Box) o).value()) instanceof String)"

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
              instanceOfString.code shouldBe "($obj0 = ((Box) o).value()) instanceof String"
              instanceOfString.typeFullName shouldBe "boolean"

              inside(instanceOfString.argument.l) { case List(tmpAssign: Call, stringType: TypeRef) =>
                tmpAssign.name shouldBe Operators.assignment
                tmpAssign.methodFullName shouldBe Operators.assignment
                tmpAssign.code shouldBe "$obj0 = ((Box) o).value()"
                tmpAssign.typeFullName shouldBe "java.lang.Object"

                inside(tmpAssign.argument.l) { case List(tmpIdentifier0: Identifier, valueCall: Call) =>
                  tmpIdentifier0.name shouldBe "$obj0"
                  tmpIdentifier0.code shouldBe "$obj0"
                  tmpIdentifier0.typeFullName shouldBe "java.lang.Object"
                  tmpIdentifier0.refsTo.l shouldBe cpg.local.nameExact("$obj0").l

                  valueCall.name shouldBe "value"
                  valueCall.methodFullName shouldBe "box.Box.value:java.lang.Object()"
                  valueCall.code shouldBe "((Box) o).value()"
                  valueCall.typeFullName shouldBe "java.lang.Object"

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
            sAssignment.code shouldBe "s = (String) $obj0"

            inside(sAssignment.argument.l) { case List(sIdentifier: Identifier, stringCast: Call) =>
              sIdentifier.name shouldBe "s"
              sIdentifier.typeFullName shouldBe "java.lang.String"
              sIdentifier.code shouldBe "s"
              sIdentifier.refsTo.l shouldBe List(sLocal)

              stringCast.name shouldBe Operators.cast
              stringCast.methodFullName shouldBe Operators.cast
              stringCast.typeFullName shouldBe "java.lang.String"
              stringCast.code shouldBe "(String) $obj0"

              inside(stringCast.argument.l) { case List(stringType: TypeRef, tmpIdentifier0: Identifier) =>
                stringType.typeFullName shouldBe "java.lang.String"

                tmpIdentifier0.name shouldBe "$obj0"
                tmpIdentifier0.code shouldBe "$obj0"
                tmpIdentifier0.typeFullName shouldBe "java.lang.Object"
                tmpIdentifier0.refsTo.l shouldBe cpg.local.nameExact("$obj0").l
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
                         |    if (o instanceof PairBox(Pair(String s, Integer i))) {
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
          case List(oInstanceOfPairBox: Call) =>
            oInstanceOfPairBox.name shouldBe Operators.instanceOf
            oInstanceOfPairBox.methodFullName shouldBe Operators.instanceOf
            oInstanceOfPairBox.typeFullName shouldBe "boolean"
            oInstanceOfPairBox.code shouldBe "o instanceof PairBox"

            inside(oInstanceOfPairBox.argument.l) { case List(oIdentifier: Identifier, pairBoxType: TypeRef) =>
              oIdentifier.name shouldBe "o"
              oIdentifier.typeFullName shouldBe "java.lang.Object"
              oIdentifier.code shouldBe "o"
              oIdentifier.refsTo.l shouldBe oParameter

              pairBoxType.typeFullName shouldBe "box.PairBox"
              pairBoxType.code shouldBe "PairBox"
            }

        }
      }

      "have the correct lowering for the variable assignment" in {
        val oParameter = cpg.method.name("foo").parameter.name("o").l
        inside(cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).astChildren.isBlock.astChildren.l) {
          case List(sLocal: Local, sAssign: Call, iLocal: Local, iAssign: Call, sSink: Call, iSink: Call) =>
            sLocal.name shouldBe "s"
            sLocal.code shouldBe "String s"
            sLocal.typeFullName shouldBe "java.lang.String"

            iLocal.name shouldBe "i"
            iLocal.code shouldBe "Integer i"
            iLocal.typeFullName shouldBe "java.lang.Integer"

            sAssign.name shouldBe Operators.assignment
            sAssign.methodFullName shouldBe Operators.assignment
            sAssign.typeFullName shouldBe "java.lang.String"
            sAssign.code shouldBe "s = ($obj0 = ((PairBox) o).value()).first()"

            inside(sAssign.argument.l) { case List(sIdentifier: Identifier, firstCall: Call) =>
              sIdentifier.name shouldBe "s"
              sIdentifier.code shouldBe "s"
              sIdentifier.typeFullName shouldBe "java.lang.String"
              sIdentifier.refsTo.l shouldBe List(sLocal)

              firstCall.name shouldBe "first"
              firstCall.methodFullName shouldBe "box.Pair.first:java.lang.String()"
              firstCall.typeFullName shouldBe "java.lang.String"
              firstCall.code shouldBe "($obj0 = ((PairBox) o).value()).first()"

              inside(firstCall.argument.l) { case List(tmpAssign0: Call) =>
                tmpAssign0.name shouldBe Operators.assignment
                tmpAssign0.code shouldBe "$obj0 = ((PairBox) o).value()"
                tmpAssign0.typeFullName shouldBe "box.Pair"

                inside(tmpAssign0.argument.l) { case List(tmpIdentifier0: Identifier, valueCall: Call) =>
                  tmpIdentifier0.name shouldBe "$obj0"
                  tmpIdentifier0.code shouldBe "$obj0"
                  tmpIdentifier0.typeFullName shouldBe "box.Pair"
                  tmpIdentifier0.refsTo.l shouldBe cpg.method("foo").local.nameExact("$obj0").l

                  valueCall.name shouldBe "value"
                  valueCall.typeFullName shouldBe "box.Pair"
                  valueCall.methodFullName shouldBe "box.PairBox.value:box.Pair()"

                  inside(valueCall.argument.l) { case List(pairBoxCast: Call) =>
                    pairBoxCast.name shouldBe Operators.cast
                    pairBoxCast.code shouldBe "(PairBox) o"
                    pairBoxCast.typeFullName shouldBe "box.PairBox"

                    inside(pairBoxCast.argument.l) { case List(pairBoxType: TypeRef, oIdentifier: Identifier) =>
                      pairBoxType.typeFullName shouldBe "box.PairBox"

                      oIdentifier.name shouldBe "o"
                      oIdentifier.typeFullName shouldBe "java.lang.Object"
                      oIdentifier.refsTo.l shouldBe cpg.parameter.name("o").l
                    }
                  }
                }
              }
            }

            iAssign.name shouldBe Operators.assignment
            iAssign.methodFullName shouldBe Operators.assignment
            iAssign.typeFullName shouldBe "java.lang.Integer"
            iAssign.code shouldBe "i = $obj0.second()"

            inside(iAssign.argument.l) { case List(iIdentifier: Identifier, secondCall: Call) =>
              iIdentifier.name shouldBe "i"
              iIdentifier.code shouldBe "i"
              iIdentifier.typeFullName shouldBe "java.lang.Integer"
              iIdentifier.refsTo.l shouldBe List(iLocal)

              secondCall.name shouldBe "second"
              secondCall.methodFullName shouldBe "box.Pair.second:java.lang.Integer()"
              secondCall.code shouldBe "$obj0.second()"
              secondCall.typeFullName shouldBe "java.lang.Integer"

              inside(secondCall.argument.l) { case List(tmpIdentifier0: Identifier) =>
                tmpIdentifier0.name shouldBe "$obj0"
                tmpIdentifier0.typeFullName shouldBe "box.Pair"
                tmpIdentifier0.refsTo.l shouldBe cpg.local.nameExact("$obj0").l
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

    "a complex mixed record pattern" should {
      val cpg = code("""
                       |record A(B a0, C a1) {}
                       |record B(String b0) {}
                       |record C(D c0, F c1) {}
                       |record D(String d0, E d1) {}
                       |record E(String e0) {}
                       |record F(G f0) {}
                       |record G<T>(String g0, T g1) {}
                       |
                       |class Test {
                       |  void test(Object o) {
                       |    if (o instanceof A(B(String b0), C(D(String d0, E(String e0)), F(G(String g0, Integer g1))))) { }
                       |  }
                       |}
                       |""".stripMargin)

      "have the correct lowering for the type check" in {
        inside(cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).condition.l) {
          case List(firstAnd: Call) =>
            firstAnd.name shouldBe Operators.logicalAnd
            firstAnd.code shouldBe "(o instanceof A) && (($obj2 = ($obj1 = ($obj0 = ((A) o).a1()).c1().f0()).g1()) instanceof Integer)"
        }
      }

      "have the correct lowering for the assignments" in {
        inside(cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).astChildren.isBlock.astChildren.l) {
          case List(
                b0Local: Local,
                b0Assign: Call,
                d0Local: Local,
                d0Assign: Call,
                e0Local: Local,
                e0Assign: Call,
                g0Local: Local,
                g0Assign: Call,
                g1Local: Local,
                g1Assign: Call
              ) =>
            b0Local.name shouldBe "b0"
            d0Local.name shouldBe "d0"
            e0Local.name shouldBe "e0"
            g0Local.name shouldBe "g0"
            g1Local.name shouldBe "g1"

            b0Assign.code shouldBe "b0 = ((A) o).a0().b0()"
            d0Assign.code shouldBe "d0 = ($obj3 = $obj0.c0()).d0()"
            e0Assign.code shouldBe "e0 = $obj3.d1().e0()"
            g0Assign.code shouldBe "g0 = $obj1.g0()"
            g1Assign.code shouldBe "g1 = (Integer) $obj2"
        }
      }
    }

    "a mixed record pattern where nested first child and second child needs instanceof" should {
      val cpg = code("""
          |record Foo<T>(T value) {}
          |record Bar<T>(Foo left, T right) {}
          |
          |class Test {
          |  void test(Object o) {
          |    if (o instanceof Foo(Bar(Foo(String s), Integer i))) { }
          |  }
          |}
          |""".stripMargin)

      "parse" in {
        cpg.call.nameExact(Operators.instanceOf).nonEmpty shouldBe true
      }

      "have the correct lowering for the type check" in {
        inside(cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).condition.l) {
          case List(firstAnd: Call) =>
            firstAnd.name shouldBe Operators.logicalAnd
            firstAnd.code shouldBe "(o instanceof Foo) && ((($obj0 = ((Foo) o).value()) instanceof Bar) && ((($obj1 = ((Bar) $obj0).left().value()) instanceof String) && (($obj2 = ((Bar) $obj0).right()) instanceof Integer)))"
        }
      }

      "have the correct lowering for the assignments" in {
        inside(cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).astChildren.isBlock.astChildren.l) {
          case List(sLocal: Local, sAssign: Call, iLocal: Local, iAssign: Call) =>
            sLocal.name shouldBe "s"
            sAssign.code shouldBe "s = (String) $obj1"
            iLocal.name shouldBe "i"
            iAssign.code shouldBe "i = (Integer) $obj2"
        }
      }
    }

    "a mixed record pattern where only the second child needs instanceof" should {
      val cpg = code("""
          |record Foo<T>(T value) {}
          |record Bar<T>(String left, T right) {}
          |
          |class Test {
          |  void test(Object o) {
          |    if (o instanceof Foo(Bar(String s, Integer i))) { }
          |  }
          |}
          |""".stripMargin)

      "parse" in {
        cpg.call.nameExact(Operators.instanceOf).nonEmpty shouldBe true
      }

      "have the correct lowering for the type check" in {
        inside(cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).condition.l) {
          case List(firstAnd: Call) =>
            firstAnd.name shouldBe Operators.logicalAnd
            firstAnd.code shouldBe "(o instanceof Foo) && ((($obj0 = ((Foo) o).value()) instanceof Bar) && (($obj1 = ((Bar) $obj0).right()) instanceof Integer))"
        }
      }

      "have the correct lowering for the assignments" in {
        inside(cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).astChildren.isBlock.astChildren.l) {
          case List(sLocal: Local, sAssign: Call, iLocal: Local, iAssign: Call) =>
            sLocal.name shouldBe "s"
            sAssign.code shouldBe "s = ((Bar) $obj0).left()"
            iLocal.name shouldBe "i"
            iAssign.code shouldBe "i = (Integer) $obj1"
        }
      }
    }

    "a mixed generic record pattern is matched" should {
      val cpg = code("""
          |record Foo<T>(T foo) {}
          |record Bar(Baz bar) {}
          |record Baz<T>(T baz) {}
          |record Qux(String qux) {}
          |
          |class Test {
          |  void test(Object o) {
          |    if (o instanceof Foo(Bar(Baz(Qux(String s))))) {
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
          case List(firstAnd: Call) =>
            firstAnd.name shouldBe Operators.logicalAnd
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
            firstAnd.code shouldBe "(o instanceof Box) && ((($obj0 = ((Box) o).value()) instanceof Pair) && ((($obj1 = ((Pair) $obj0).first()) instanceof String) && (($obj2 = ((Pair) $obj0).second()) instanceof Integer)))"

            inside(firstAnd.argument.l) { case List(oInstanceOfBox: Call, secondAnd: Call) =>
              oInstanceOfBox.name shouldBe Operators.instanceOf
              oInstanceOfBox.methodFullName shouldBe Operators.instanceOf
              oInstanceOfBox.typeFullName shouldBe "boolean"
              oInstanceOfBox.code shouldBe "o instanceof Box"

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
              secondAnd.code shouldBe "(($obj0 = ((Box) o).value()) instanceof Pair) && ((($obj1 = ((Pair) $obj0).first()) instanceof String) && (($obj2 = ((Pair) $obj0).second()) instanceof Integer))"

              inside(secondAnd.argument.l) { case List(oValueInstanceOfPair: Call, thirdAnd: Call) =>
                oValueInstanceOfPair.name shouldBe Operators.instanceOf
                oValueInstanceOfPair.methodFullName shouldBe Operators.instanceOf
                oValueInstanceOfPair.typeFullName shouldBe "boolean"
                oValueInstanceOfPair.code shouldBe "($obj0 = ((Box) o).value()) instanceof Pair"

                inside(oValueInstanceOfPair.argument.l) { case List(tmpAssignment: Call, pairType: TypeRef) =>
                  inside(tmpAssignment.argument.l) { case List(tmpIdentifier0: Identifier, valueCall: Call) =>
                    valueCall.name shouldBe "value"
                    valueCall.methodFullName shouldBe "box.Box.value:java.lang.Object()"
                    valueCall.signature shouldBe "java.lang.Object()"
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
                  }

                  pairType.typeFullName shouldBe "box.Pair"
                  pairType.code shouldBe "Pair"
                }

                thirdAnd.name shouldBe Operators.logicalAnd
                thirdAnd.methodFullName shouldBe Operators.logicalAnd
                thirdAnd.typeFullName shouldBe "boolean"
                thirdAnd.code shouldBe "(($obj1 = ((Pair) $obj0).first()) instanceof String) && (($obj2 = ((Pair) $obj0).second()) instanceof Integer)"

                inside(thirdAnd.argument.l) { case List(firstInstanceOfString: Call, secondInstanceOfInteger: Call) =>
                  firstInstanceOfString.name shouldBe Operators.instanceOf
                  firstInstanceOfString.methodFullName shouldBe Operators.instanceOf
                  firstInstanceOfString.typeFullName shouldBe "boolean"
                  firstInstanceOfString.code shouldBe "($obj1 = ((Pair) $obj0).first()) instanceof String"

                  inside(firstInstanceOfString.argument.l) { case List(tmp1Assign: Call, stringType: TypeRef) =>
                    tmp1Assign.name shouldBe Operators.assignment
                    tmp1Assign.methodFullName shouldBe Operators.assignment
                    tmp1Assign.code shouldBe "$obj1 = ((Pair) $obj0).first()"
                    tmp1Assign.typeFullName shouldBe "java.lang.Object"

                    inside(tmp1Assign.argument.l) { case List(tmpIdentifier1: Identifier, firstCall: Call) =>
                      tmpIdentifier1.name shouldBe "$obj1"
                      tmpIdentifier1.code shouldBe "$obj1"
                      tmpIdentifier1.typeFullName shouldBe "java.lang.Object"
                      tmpIdentifier1.refsTo.l shouldBe cpg.local.nameExact("$obj1").l

                      firstCall.name shouldBe "first"
                      firstCall.methodFullName shouldBe "box.Pair.first:java.lang.Object()"
                      firstCall.typeFullName shouldBe "java.lang.Object"
                      firstCall.code shouldBe "((Pair) $obj0).first()"

                      inside(firstCall.argument.l) { case List(pairCast: Call) =>
                        pairCast.name shouldBe Operators.cast
                        pairCast.methodFullName shouldBe Operators.cast
                        pairCast.typeFullName shouldBe "box.Pair"
                        pairCast.code shouldBe "(Pair) $obj0"

                        inside(pairCast.argument.l) { case List(pairType: TypeRef, tmpIdentifier0: Identifier) =>
                          pairType.typeFullName shouldBe "box.Pair"

                          tmpIdentifier0.name shouldBe "$obj0"
                          tmpIdentifier0.code shouldBe "$obj0"
                          tmpIdentifier0.typeFullName shouldBe "java.lang.Object"
                          tmpIdentifier0.refsTo.l shouldBe cpg.local.nameExact("$obj0").l
                        }
                      }
                    }

                    stringType.typeFullName shouldBe "java.lang.String"
                    stringType.code shouldBe "String"
                  }

                  secondInstanceOfInteger.name shouldBe Operators.instanceOf
                  secondInstanceOfInteger.methodFullName shouldBe Operators.instanceOf
                  secondInstanceOfInteger.typeFullName shouldBe "boolean"
                  secondInstanceOfInteger.code shouldBe "($obj2 = ((Pair) $obj0).second()) instanceof Integer"

                  inside(secondInstanceOfInteger.argument.l) { case List(tmp2Assign: Call, integerType: TypeRef) =>
                    tmp2Assign.name shouldBe Operators.assignment
                    tmp2Assign.methodFullName shouldBe Operators.assignment
                    tmp2Assign.code shouldBe "$obj2 = ((Pair) $obj0).second()"
                    tmp2Assign.typeFullName shouldBe "java.lang.Object"

                    inside(tmp2Assign.argument.l) { case List(tmpIdentifier2: Identifier, secondCall: Call) =>
                      tmpIdentifier2.name shouldBe "$obj2"
                      tmpIdentifier2.code shouldBe "$obj2"
                      tmpIdentifier2.typeFullName shouldBe "java.lang.Object"
                      tmpIdentifier2.refsTo.l shouldBe cpg.local.nameExact("$obj2").l

                      secondCall.name shouldBe "second"
                      secondCall.methodFullName shouldBe "box.Pair.second:java.lang.Object()"
                      secondCall.typeFullName shouldBe "java.lang.Object"
                      secondCall.code shouldBe "((Pair) $obj0).second()"

                      inside(secondCall.argument.l) { case List(pairCast: Call) =>
                        pairCast.name shouldBe Operators.cast
                        pairCast.methodFullName shouldBe Operators.cast
                        pairCast.typeFullName shouldBe "box.Pair"
                        pairCast.code shouldBe "(Pair) $obj0"

                        inside(pairCast.argument.l) { case List(pairType: TypeRef, tmpIdentifier0: Identifier) =>
                          pairType.typeFullName shouldBe "box.Pair"
                          pairType.code shouldBe "Pair"

                          tmpIdentifier0.name shouldBe "$obj0"
                          tmpIdentifier0.code shouldBe "$obj0"
                          tmpIdentifier0.typeFullName shouldBe "java.lang.Object"
                          tmpIdentifier0.refsTo.l shouldBe cpg.local.nameExact("$obj0").l
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
          case List(sLocal: Local, sAssign: Call, iLocal: Local, iAssign: Call, sSink: Call, iSink: Call) =>
            sLocal.name shouldBe "s"
            sLocal.code shouldBe "String s"
            sLocal.typeFullName shouldBe "java.lang.String"

            iLocal.name shouldBe "i"
            iLocal.code shouldBe "Integer i"
            iLocal.typeFullName shouldBe "java.lang.Integer"

            sAssign.name shouldBe Operators.assignment
            sAssign.methodFullName shouldBe Operators.assignment
            sAssign.typeFullName shouldBe "java.lang.String"
            sAssign.code shouldBe "s = (String) $obj1"

            inside(sAssign.argument.l) { case List(sIdentifier: Identifier, stringCast: Call) =>
              sIdentifier.name shouldBe "s"
              sIdentifier.code shouldBe "s"
              sIdentifier.typeFullName shouldBe "java.lang.String"
              sIdentifier.refsTo.l shouldBe List(sLocal)

              stringCast.name shouldBe Operators.cast
              stringCast.methodFullName shouldBe Operators.cast
              stringCast.typeFullName shouldBe "java.lang.String"
              stringCast.code shouldBe "(String) $obj1"

              inside(stringCast.argument.l) { case List(stringType: TypeRef, tmpIdentifier1: Identifier) =>
                stringType.typeFullName shouldBe "java.lang.String"
                stringType.code shouldBe "String"

                tmpIdentifier1.name shouldBe "$obj1"
                tmpIdentifier1.code shouldBe "$obj1"
                tmpIdentifier1.typeFullName shouldBe "java.lang.Object"
                tmpIdentifier1.refsTo.l shouldBe cpg.local.nameExact("$obj1").l
              }

              iAssign.name shouldBe Operators.assignment
              iAssign.methodFullName shouldBe Operators.assignment
              iAssign.typeFullName shouldBe "java.lang.Integer"
              iAssign.code shouldBe "i = (Integer) $obj2"

              inside(iAssign.argument.l) { case List(iIdentifier: Identifier, integerCast: Call) =>
                iIdentifier.name shouldBe "i"
                iIdentifier.code shouldBe "i"
                iIdentifier.typeFullName shouldBe "java.lang.Integer"
                iIdentifier.refsTo.l shouldBe List(iLocal)

                integerCast.name shouldBe Operators.cast
                integerCast.methodFullName shouldBe Operators.cast
                integerCast.typeFullName shouldBe "java.lang.Integer"
                integerCast.code shouldBe "(Integer) $obj2"

                inside(integerCast.argument.l) { case List(integerType: TypeRef, tmpIdentifier2: Identifier) =>
                  integerType.typeFullName shouldBe "java.lang.Integer"
                  integerType.code shouldBe "Integer"

                  tmpIdentifier2.name shouldBe "$obj2"
                  tmpIdentifier2.code shouldBe "$obj2"
                  tmpIdentifier2.typeFullName shouldBe "java.lang.Object"
                  tmpIdentifier2.refsTo.l shouldBe cpg.local.nameExact("$obj2").l
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
        ) { case List(_: JumpTarget, instanceCheck: ControlStructure) =>
          inside(instanceCheck.astChildren.collectAll[Call].argument.l) {
            case List(oIdentifier: Identifier, stringType: TypeRef) =>
              oIdentifier.name shouldBe "o"
              oIdentifier.code shouldBe "o"
              oIdentifier.typeFullName shouldBe "java.lang.Object"

              stringType.typeFullName shouldBe "java.lang.String"
          }
          instanceCheck.code shouldBe "if (o instanceof String)"

          inside(instanceCheck.astChildren.l) { case List(instanceOfCall: Call, statementsBlock: Block) =>
            instanceOfCall.code shouldBe "o instanceof String"
            inside(statementsBlock.astChildren.l) { case List(sLocal: Local, sAssign: Call, sinkCall: Call) =>
              sLocal.name shouldBe "s"
              sLocal.typeFullName shouldBe "java.lang.String"
              sLocal.code shouldBe "String s"

              sAssign.name shouldBe Operators.assignment
              sAssign.methodFullName shouldBe Operators.assignment
              sAssign.code shouldBe "s = (String) o"

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

      "have the correct lowering for the type check" in {
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
        inside(
          cpg.controlStructure
            .controlStructureType(ControlStructureTypes.IF)
            .astChildren
            .isBlock
            .astChildren
            .l
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

            inside(valueCall.argument.l) { case List(oCast: Call) =>
              oCast.name shouldBe Operators.cast
              oCast.code shouldBe "(Box) o"
              oCast.typeFullName shouldBe "box.Box"
              oCast.lineNumber shouldBe Some(9)
              oCast.columnNumber shouldBe Some(12)

              inside(oCast.argument.l) { case List(boxType: TypeRef, oIdentifier: Identifier) =>
                boxType.typeFullName shouldBe "box.Box"

                oIdentifier.name shouldBe "o"
                oIdentifier.typeFullName shouldBe "java.lang.Object"
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

      "have the correct lowering for the type check" in {
        inside(cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).condition.l) {
          case List(andCall: Call) =>
            andCall.name shouldBe Operators.logicalAnd
            andCall.methodFullName shouldBe Operators.logicalAnd
            andCall.code shouldBe "(o instanceof Box) && (($obj0 = ((Box) o).value()) instanceof String)"

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
              instanceOfString.code shouldBe "($obj0 = ((Box) o).value()) instanceof String"
              instanceOfString.typeFullName shouldBe "boolean"

              inside(instanceOfString.argument.l) { case List(tmpAssign: Call, stringType: TypeRef) =>
                tmpAssign.name shouldBe Operators.assignment
                tmpAssign.methodFullName shouldBe Operators.assignment
                tmpAssign.code shouldBe "$obj0 = ((Box) o).value()"
                tmpAssign.typeFullName shouldBe "java.lang.Object"

                inside(tmpAssign.argument.l) { case List(tmpIdentifier0: Identifier, valueCall: Call) =>
                  tmpIdentifier0.name shouldBe "$obj0"
                  tmpIdentifier0.code shouldBe "$obj0"
                  tmpIdentifier0.typeFullName shouldBe "java.lang.Object"
                  tmpIdentifier0.refsTo.l shouldBe cpg.local.nameExact("$obj0").l

                  valueCall.name shouldBe "value"
                  valueCall.methodFullName shouldBe "box.Box.value:java.lang.Object()"
                  valueCall.code shouldBe "((Box) o).value()"
                  valueCall.typeFullName shouldBe "java.lang.Object"

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
                }

                stringType.typeFullName shouldBe "java.lang.String"
                stringType.code shouldBe "String"
              }
            }
        }
      }

      "have the correct lowering for the variable assignment" in {
        inside(
          cpg.controlStructure
            .controlStructureType(ControlStructureTypes.IF)
            .astChildren
            .isBlock
            .astChildren
            .l
        ) { case List(sLocal: Local, sAssignment: Call, _: Call) =>
          sLocal.name shouldBe "s"
          sLocal.typeFullName shouldBe "java.lang.String"
          sLocal.code shouldBe "String s"

          sAssignment.name shouldBe Operators.assignment
          sAssignment.methodFullName shouldBe Operators.assignment
          sAssignment.typeFullName shouldBe "java.lang.String"
          sAssignment.code shouldBe "s = (String) $obj0"

          inside(sAssignment.argument.l) { case List(sIdentifier: Identifier, stringCast: Call) =>
            sIdentifier.name shouldBe "s"
            sIdentifier.typeFullName shouldBe "java.lang.String"
            sIdentifier.code shouldBe "s"
            sIdentifier.refsTo.l shouldBe List(sLocal)

            stringCast.name shouldBe Operators.cast
            stringCast.methodFullName shouldBe Operators.cast
            stringCast.typeFullName shouldBe "java.lang.String"
            stringCast.code shouldBe "(String) $obj0"

            inside(stringCast.argument.l) { case List(stringType: TypeRef, tmpIdentifier0: Identifier) =>
              stringType.typeFullName shouldBe "java.lang.String"

              tmpIdentifier0.name shouldBe "$obj0"
              tmpIdentifier0.code shouldBe "$obj0"
              tmpIdentifier0.typeFullName shouldBe "java.lang.Object"
              tmpIdentifier0.refsTo.l shouldBe cpg.local.nameExact("$obj0").l
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

      "have the correct lowering for the type check" in {
        val oParameter = cpg.method.name("foo").parameter.name("o").l
        inside(cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).condition.l) {
          case List(oInstanceOfPairBox: Call) =>
            oInstanceOfPairBox.name shouldBe Operators.instanceOf
            oInstanceOfPairBox.methodFullName shouldBe Operators.instanceOf
            oInstanceOfPairBox.typeFullName shouldBe "boolean"
            oInstanceOfPairBox.code shouldBe "o instanceof PairBox"

            inside(oInstanceOfPairBox.argument.l) { case List(oIdentifier: Identifier, pairBoxType: TypeRef) =>
              oIdentifier.name shouldBe "o"
              oIdentifier.typeFullName shouldBe "java.lang.Object"
              oIdentifier.code shouldBe "o"
              oIdentifier.refsTo.l shouldBe oParameter

              pairBoxType.typeFullName shouldBe "box.PairBox"
              pairBoxType.code shouldBe "PairBox"
            }
        }
      }

      "have the correct lowering for the variable assignment" in {
        val oParameter = cpg.method.name("foo").parameter.name("o").l
        inside(cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).astChildren.isBlock.astChildren.l) {
          case List(sLocal: Local, sAssign: Call, iLocal: Local, iAssign: Call, sinkS: Call, sinkI: Call) =>
            sLocal.name shouldBe "s"
            sLocal.code shouldBe "String s"
            sLocal.typeFullName shouldBe "java.lang.String"

            iLocal.name shouldBe "i"
            iLocal.code shouldBe "Integer i"
            iLocal.typeFullName shouldBe "java.lang.Integer"

            sAssign.name shouldBe Operators.assignment
            sAssign.methodFullName shouldBe Operators.assignment
            sAssign.typeFullName shouldBe "java.lang.String"
            sAssign.code shouldBe "s = ($obj0 = ((PairBox) o).value()).first()"

            inside(sAssign.argument.l) { case List(sIdentifier: Identifier, firstCall: Call) =>
              sIdentifier.name shouldBe "s"
              sIdentifier.code shouldBe "s"
              sIdentifier.typeFullName shouldBe "java.lang.String"
              sIdentifier.refsTo.l shouldBe List(sLocal)

              firstCall.name shouldBe "first"
              firstCall.methodFullName shouldBe "box.Pair.first:java.lang.String()"
              firstCall.code shouldBe "($obj0 = ((PairBox) o).value()).first()"
              firstCall.typeFullName shouldBe "java.lang.String"

              inside(firstCall.argument.l) { case List(tmpAssign0: Call) =>
                tmpAssign0.name shouldBe Operators.assignment
                tmpAssign0.code shouldBe "$obj0 = ((PairBox) o).value()"
                tmpAssign0.typeFullName shouldBe "box.Pair"

                inside(tmpAssign0.argument.l) { case List(tmpIdentifier0: Identifier, valueCall: Call) =>
                  tmpIdentifier0.name shouldBe "$obj0"
                  tmpIdentifier0.typeFullName shouldBe "box.Pair"
                  tmpIdentifier0.refsTo.l shouldBe cpg.local.nameExact("$obj0").l

                  valueCall.name shouldBe "value"
                  valueCall.methodFullName shouldBe "box.PairBox.value:box.Pair()"
                  valueCall.code shouldBe "((PairBox) o).value()"

                  inside(valueCall.argument.l) { case List(pairBoxCast: Call) =>
                    pairBoxCast.name shouldBe Operators.cast
                    pairBoxCast.code shouldBe "(PairBox) o"
                    pairBoxCast.typeFullName shouldBe "box.PairBox"

                    inside(pairBoxCast.argument.l) { case List(pairBoxType: TypeRef, oIdentifier: Identifier) =>
                      pairBoxType.typeFullName shouldBe "box.PairBox"

                      oIdentifier.name shouldBe "o"
                      oIdentifier.typeFullName shouldBe "java.lang.Object"
                      oIdentifier.refsTo.l shouldBe cpg.parameter.name("o").l
                    }
                  }

                }
              }
            }

            iAssign.name shouldBe Operators.assignment
            iAssign.methodFullName shouldBe Operators.assignment
            iAssign.typeFullName shouldBe "java.lang.Integer"
            iAssign.code shouldBe "i = $obj0.second()"

            inside(iAssign.argument.l) { case List(iIdentifier: Identifier, secondCall: Call) =>
              iIdentifier.name shouldBe "i"
              iIdentifier.code shouldBe "i"
              iIdentifier.typeFullName shouldBe "java.lang.Integer"
              iIdentifier.refsTo.l shouldBe List(iLocal)

              secondCall.name shouldBe "second"
              secondCall.methodFullName shouldBe "box.Pair.second:java.lang.Integer()"
              secondCall.code shouldBe "$obj0.second()"

              inside(secondCall.argument.l) { case List(tmpIdentifier0: Identifier) =>
                tmpIdentifier0.name shouldBe "$obj0"
                tmpIdentifier0.typeFullName shouldBe "box.Pair"
                tmpIdentifier0.refsTo.l shouldBe cpg.local.nameExact("$obj0").l
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
          cpg.controlStructure
            .controlStructureType(ControlStructureTypes.SWITCH)
            .astChildren
            .isBlock
            .astChildren
            .isControlStructure
            .astChildren
            .isBlock
            .astChildren
            .l
        ) { case List(sLocal: Local, sAssign: Call, iLocal: Local, iAssign: Call, sSink: Call, iSink: Call) =>
          sLocal.name shouldBe "s"
          sLocal.code shouldBe "String s"
          sLocal.typeFullName shouldBe "java.lang.String"

          iLocal.name shouldBe "i"
          iLocal.code shouldBe "Integer i"
          iLocal.typeFullName shouldBe "java.lang.Integer"

          sAssign.name shouldBe Operators.assignment
          sAssign.methodFullName shouldBe Operators.assignment
          sAssign.typeFullName shouldBe "java.lang.String"
          sAssign.code shouldBe "s = (String) $obj1"

          inside(sAssign.argument.l) { case List(sIdentifier: Identifier, stringCast: Call) =>
            sIdentifier.name shouldBe "s"
            sIdentifier.code shouldBe "s"
            sIdentifier.typeFullName shouldBe "java.lang.String"
            sIdentifier.refsTo.l shouldBe List(sLocal)

            stringCast.name shouldBe Operators.cast
            stringCast.methodFullName shouldBe Operators.cast
            stringCast.typeFullName shouldBe "java.lang.String"
            stringCast.code shouldBe "(String) $obj1"

            inside(stringCast.argument.l) { case List(stringType: TypeRef, tmpIdentifier1: Identifier) =>
              stringType.typeFullName shouldBe "java.lang.String"
              stringType.code shouldBe "String"

              tmpIdentifier1.name shouldBe "$obj1"
              tmpIdentifier1.code shouldBe "$obj1"
              tmpIdentifier1.typeFullName shouldBe "java.lang.Object"
              tmpIdentifier1.refsTo.l shouldBe cpg.local.nameExact("$obj1").l
            }
          }

          iAssign.name shouldBe Operators.assignment
          iAssign.methodFullName shouldBe Operators.assignment
          iAssign.typeFullName shouldBe "java.lang.Integer"
          iAssign.code shouldBe "i = (Integer) $obj2"

          inside(iAssign.argument.l) { case List(iIdentifier: Identifier, integerCast: Call) =>
            iIdentifier.name shouldBe "i"
            iIdentifier.code shouldBe "i"
            iIdentifier.typeFullName shouldBe "java.lang.Integer"
            iIdentifier.refsTo.l shouldBe List(iLocal)

            integerCast.name shouldBe Operators.cast
            integerCast.methodFullName shouldBe Operators.cast
            integerCast.typeFullName shouldBe "java.lang.Integer"
            integerCast.code shouldBe "(Integer) $obj2"

            inside(integerCast.argument.l) { case List(integerType: TypeRef, tmpIdentifier2: Identifier) =>
              integerType.typeFullName shouldBe "java.lang.Integer"
              integerType.code shouldBe "Integer"

              tmpIdentifier2.name shouldBe "$obj2"
              tmpIdentifier2.code shouldBe "$obj2"
              tmpIdentifier2.typeFullName shouldBe "java.lang.Object"
              tmpIdentifier2.refsTo.l shouldBe cpg.local.nameExact("$obj2").l
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
            bAssign.code shouldBe "b = (Bar) o"

            inside(bAssign.argument.l) { case List(bIdentifier: Identifier, castCall: Call) =>
              bIdentifier.name shouldBe "b"
              bIdentifier.code shouldBe "b"
              bIdentifier.typeFullName shouldBe "ANY"
              bIdentifier.refsTo.l shouldBe List(bLocal)

              castCall.name shouldBe Operators.cast
              castCall.methodFullName shouldBe Operators.cast
              castCall.typeFullName shouldBe "ANY"
              castCall.code shouldBe "(Bar) o"

              inside(castCall.argument.l) { case List(barType: TypeRef, oIdentifier: Identifier) =>
                barType.code shouldBe "Bar"
                barType.typeFullName shouldBe "ANY"

                oIdentifier.name shouldBe "o"
                oIdentifier.typeFullName shouldBe "java.lang.Object"
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
            bAssign.typeFullName shouldBe "bar.Bar"
            bAssign.code shouldBe "b = (Bar) o"

            inside(bAssign.argument.l) { case List(bIdentifier: Identifier, castCall: Call) =>
              bIdentifier.name shouldBe "b"
              bIdentifier.code shouldBe "b"
              bIdentifier.typeFullName shouldBe "bar.Bar"
              bIdentifier.refsTo.l shouldBe List(bLocal)

              castCall.name shouldBe Operators.cast
              castCall.methodFullName shouldBe Operators.cast
              castCall.typeFullName shouldBe "bar.Bar"
              castCall.code shouldBe "(Bar) o"

              inside(castCall.argument.l) { case List(barType: TypeRef, oIdentifier: Identifier) =>
                barType.typeFullName shouldBe "bar.Bar"

                oIdentifier.name shouldBe "o"
                oIdentifier.code shouldBe "o"
                oIdentifier.typeFullName shouldBe "java.lang.Object"
                oIdentifier.refsTo.l shouldBe cpg.method.name("foo").parameter.name("o").l
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
          case List(firstAnd: Call) =>
            firstAnd.name shouldBe Operators.logicalAnd
            firstAnd.methodFullName shouldBe Operators.logicalAnd
            firstAnd.typeFullName shouldBe "boolean"
            firstAnd.code shouldBe "(o instanceof Bar) && ((($obj0 = ((Bar) o).<unknownField>()) instanceof Baz) && (($obj1 = ((Baz) $obj0).<unknownField>()) instanceof Qux))"

            inside(firstAnd.argument.l) { case List(instanceOfBar: Call, secondAnd: Call) =>
              instanceOfBar.name shouldBe Operators.instanceOf
              instanceOfBar.methodFullName shouldBe Operators.instanceOf
              instanceOfBar.code shouldBe "o instanceof Bar"
              instanceOfBar.typeFullName shouldBe "boolean"

              inside(secondAnd.argument.l) { case List(instanceOfBaz: Call, instanceOfQux: Call) =>
                instanceOfBaz.name shouldBe Operators.instanceOf
                instanceOfBaz.methodFullName shouldBe Operators.instanceOf
                instanceOfBaz.code shouldBe "($obj0 = ((Bar) o).<unknownField>()) instanceof Baz"
                instanceOfBaz.typeFullName shouldBe "boolean"

                inside(instanceOfBaz.argument.l) { case List(tmpAssign: Call, bazType: TypeRef) =>
                  inside(tmpAssign.argument.l) { case List(tmpIdentifier0: Identifier, fieldAccessor: Call) =>
                    tmpIdentifier0.name shouldBe "$obj0"
                    tmpIdentifier0.code shouldBe "$obj0"
                    tmpIdentifier0.typeFullName shouldBe "ANY"
                    tmpIdentifier0.refsTo.l shouldBe cpg.local.nameExact("$obj0").l

                    fieldAccessor.name shouldBe "<unknownField>"
                    fieldAccessor.methodFullName shouldBe "<unresolvedNamespace>.Bar.<unknownField>:<unresolvedSignature>(0)"
                    fieldAccessor.typeFullName shouldBe "ANY"
                    fieldAccessor.code shouldBe "((Bar) o).<unknownField>()"
                  }

                  bazType.typeFullName shouldBe "ANY"
                  bazType.code shouldBe "Baz"
                }
              }
            }
        }
      }

      "have the correct lowering for the variable assignment" in {
        inside(cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).astChildren.isBlock.astChildren.l) {
          case List(qLocal: Local, qAssign: Call, qSink: Call) =>
            qLocal.name shouldBe "q"
            qLocal.code shouldBe "Qux q"
            qLocal.typeFullName shouldBe "ANY"

            qAssign.name shouldBe Operators.assignment
            qAssign.methodFullName shouldBe Operators.assignment
            qAssign.typeFullName shouldBe "ANY"
            qAssign.code shouldBe "q = (Qux) $obj1"

            inside(qAssign.argument.l) { case List(qIdentifier: Identifier, quxCast: Call) =>
              qIdentifier.name shouldBe "q"
              qIdentifier.code shouldBe "q"
              qIdentifier.typeFullName shouldBe "ANY"
              qIdentifier.refsTo.l shouldBe List(qLocal)

              quxCast.name shouldBe Operators.cast

              inside(quxCast.argument.l) { case List(quxType: TypeRef, tmpIdentifier1: Identifier) =>
                quxType.code shouldBe "Qux"

                tmpIdentifier1.name shouldBe "$obj1"
                tmpIdentifier1.code shouldBe "$obj1"
                tmpIdentifier1.typeFullName shouldBe "ANY"
                tmpIdentifier1.refsTo.l shouldBe cpg.local.nameExact("$obj1").l
              }
            }
        }
      }
    }
  }

}
