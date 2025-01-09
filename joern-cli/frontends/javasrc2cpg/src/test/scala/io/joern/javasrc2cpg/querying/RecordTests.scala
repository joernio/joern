package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.{ModifierTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{Call, FieldIdentifier, Identifier, Literal, Method, Return}
import io.shiftleft.semanticcpg.language.*

class RecordTests extends JavaSrcCode2CpgFixture {

  "a record with a compact constructor" should {
    val cpg = code("""
                       |package foo;
                       |
                       |record Foo(String value) {
                       |  public Foo {
                       |    System.out.println(value);
                       |  }
                       |}
                       |""".stripMargin)

    "extend java.lang.Record" in {
      cpg.typeDecl("Foo").inheritsFromTypeFullName.l shouldBe List("java.lang.Record")
    }

    "have the correct representation for the compact constructor" in {
      inside(cpg.method.nameExact("<init>").l) { case List(constructor) =>
        constructor.fullName shouldBe "foo.Foo.<init>:void(java.lang.String)"

        inside(constructor.parameter.l) { case List(thisParam, valueParam) =>
          thisParam.name shouldBe "this"
          thisParam.typeFullName shouldBe "foo.Foo"

          valueParam.name shouldBe "value"
          valueParam.typeFullName shouldBe "java.lang.String"

          inside(constructor.body.astChildren.l) { case List(valueAssign: Call, printlnCall: Call) =>
            valueAssign.name shouldBe Operators.assignment
            valueAssign.methodFullName shouldBe Operators.assignment
            valueAssign.typeFullName shouldBe "java.lang.String"
            valueAssign.code shouldBe "this.value = value"

            inside(valueAssign.argument.l) { case List(fieldAccess: Call, valueIdentifier: Identifier) =>
              fieldAccess.name shouldBe Operators.fieldAccess
              fieldAccess.code shouldBe "this.value"
              fieldAccess.typeFullName shouldBe "java.lang.String"

              inside(fieldAccess.argument.l) {
                case List(thisIdentifier: Identifier, valueFieldIdentifier: FieldIdentifier) =>
                  thisIdentifier.name shouldBe "this"
                  thisIdentifier.typeFullName shouldBe "foo.Foo"
                  thisIdentifier.refsTo.l shouldBe List(thisParam)

                  valueFieldIdentifier.canonicalName shouldBe "value"
              }

              valueIdentifier.name shouldBe "value"
              valueIdentifier.typeFullName shouldBe "java.lang.String"
              valueIdentifier.refsTo.l shouldBe List(valueParam)
            }

            printlnCall.name shouldBe "println"
            printlnCall.code shouldBe "System.out.println(value)"
            inside(printlnCall.argument.l) { case List(_, valueIdentifier: Identifier) =>
              valueIdentifier.name shouldBe "value"
              valueIdentifier.refsTo.l shouldBe List(valueParam)
            }
          }
        }
      }
    }

    "have a private field for the parameter" in {
      inside(cpg.member.l) { case List(valueMember) =>
        valueMember.name shouldBe "value"
        valueMember.code shouldBe "String value"
        valueMember.typeFullName shouldBe "java.lang.String"
        valueMember.modifier.modifierType.l shouldBe List(ModifierTypes.PRIVATE)
      }
    }

    "have a public accessor method for the parameter" in {
      inside(cpg.method.name("value").l) { case List(valueMethod: Method) =>
        valueMethod.name shouldBe "value"
        valueMethod.fullName shouldBe "foo.Foo.value:java.lang.String()"
        valueMethod.code shouldBe "public String value()"
        valueMethod.lineNumber shouldBe Some(4)
        valueMethod.columnNumber shouldBe Some(12)

        val methodReturn = valueMethod.methodReturn
        methodReturn.typeFullName shouldBe "java.lang.String"
        methodReturn.lineNumber shouldBe Some(4)
        methodReturn.columnNumber shouldBe Some(12)

        inside(valueMethod.parameter.l) { case List(thisParam) =>
          thisParam.name shouldBe "this"
          thisParam.typeFullName shouldBe "foo.Foo"
          thisParam.lineNumber shouldBe Some(4)
          thisParam.columnNumber shouldBe Some(12)
        }

        inside(valueMethod.body.astChildren.l) { case List(returnStmt: Return) =>
          returnStmt.code shouldBe "return this.value"
          returnStmt.lineNumber shouldBe Some(4)
          returnStmt.columnNumber shouldBe Some(12)

          inside(returnStmt.astChildren.l) { case List(fieldAccess: Call) =>
            fieldAccess.name shouldBe Operators.fieldAccess
            fieldAccess.methodFullName shouldBe Operators.fieldAccess
            fieldAccess.code shouldBe "this.value"
            fieldAccess.typeFullName shouldBe "java.lang.String"
            fieldAccess.lineNumber shouldBe Some(4)
            fieldAccess.columnNumber shouldBe Some(12)

            inside(fieldAccess.argument.l) { case List(thisIdentifier: Identifier, fieldIdentifier: FieldIdentifier) =>
              thisIdentifier.name shouldBe "this"
              thisIdentifier.code shouldBe "this"
              thisIdentifier.typeFullName shouldBe "foo.Foo"
              thisIdentifier.refsTo.l shouldBe cpg.method.name("value").parameter.l
              thisIdentifier.lineNumber shouldBe Some(4)
              thisIdentifier.columnNumber shouldBe Some(12)

              fieldIdentifier.canonicalName shouldBe "value"
              fieldIdentifier.code shouldBe "value"
              fieldIdentifier.lineNumber shouldBe Some(4)
              fieldIdentifier.columnNumber shouldBe Some(12)
            }
          }
        }
      }
    }
  }

  "a record with an explicit non-canonical constructor" should {
    val cpg = code("""
                       |package foo;
                       |
                       |record Foo(String value) {
                       |    public Foo() {
                       |        this.value = "value";
                       |    }
                       |}
                       |""".stripMargin)

    "have the correct constructors" in {
      inside(cpg.method.nameExact("<init>").sortBy(_.parameter.size).l) {
        case List(explicitConstructor, canonicalConstructor) =>
          explicitConstructor.fullName shouldBe "foo.Foo.<init>:void()"

          inside(explicitConstructor.parameter.l) { case List(thisParam) =>
            thisParam.name shouldBe "this"
            thisParam.typeFullName shouldBe "foo.Foo"

            inside(explicitConstructor.body.astChildren.l) { case List(valueAssign: Call) =>
              valueAssign.name shouldBe Operators.assignment
              valueAssign.methodFullName shouldBe Operators.assignment
              valueAssign.typeFullName shouldBe "java.lang.String"
              valueAssign.code shouldBe "this.value = \"value\""

              inside(valueAssign.argument.l) { case List(fieldAccess: Call, valueLiteral: Literal) =>
                fieldAccess.name shouldBe Operators.fieldAccess
                fieldAccess.code shouldBe "this.value"
                fieldAccess.typeFullName shouldBe "java.lang.String"

                inside(fieldAccess.argument.l) {
                  case List(thisIdentifier: Identifier, valueFieldIdentifier: FieldIdentifier) =>
                    thisIdentifier.name shouldBe "this"
                    thisIdentifier.typeFullName shouldBe "foo.Foo"
                    thisIdentifier.refsTo.l shouldBe List(thisParam)

                    valueFieldIdentifier.canonicalName shouldBe "value"
                }

                valueLiteral.typeFullName shouldBe "java.lang.String"
                valueLiteral.code shouldBe "\"value\""
              }
            }
          }

          canonicalConstructor.fullName shouldBe "foo.Foo.<init>:void(java.lang.String)"

          inside(canonicalConstructor.parameter.l) { case List(thisParam, valueParam) =>
            thisParam.name shouldBe "this"
            thisParam.typeFullName shouldBe "foo.Foo"

            valueParam.name shouldBe "value"
            valueParam.typeFullName shouldBe "java.lang.String"

            inside(canonicalConstructor.body.astChildren.l) { case List(valueAssign: Call) =>
              valueAssign.name shouldBe Operators.assignment
              valueAssign.methodFullName shouldBe Operators.assignment
              valueAssign.typeFullName shouldBe "java.lang.String"
              valueAssign.code shouldBe "this.value = value"

              inside(valueAssign.argument.l) { case List(fieldAccess: Call, valueIdentifier: Identifier) =>
                fieldAccess.name shouldBe Operators.fieldAccess
                fieldAccess.code shouldBe "this.value"
                fieldAccess.typeFullName shouldBe "java.lang.String"

                inside(fieldAccess.argument.l) {
                  case List(thisIdentifier: Identifier, valueFieldIdentifier: FieldIdentifier) =>
                    thisIdentifier.name shouldBe "this"
                    thisIdentifier.typeFullName shouldBe "foo.Foo"
                    thisIdentifier.refsTo.l shouldBe List(thisParam)

                    valueFieldIdentifier.canonicalName shouldBe "value"
                }

                valueIdentifier.name shouldBe "value"
                valueIdentifier.typeFullName shouldBe "java.lang.String"
                valueIdentifier.refsTo.l shouldBe List(valueParam)
              }
            }
          }
      }
    }

    "have a private field for the parameter" in {
      inside(cpg.member.l) { case List(valueMember) =>
        valueMember.name shouldBe "value"
        valueMember.code shouldBe "String value"
        valueMember.typeFullName shouldBe "java.lang.String"
        valueMember.modifier.modifierType.l shouldBe List(ModifierTypes.PRIVATE)
      }
    }

    "have a public accessor method for the parameter" in {
      inside(cpg.method.name("value").l) { case List(valueMethod: Method) =>
        valueMethod.name shouldBe "value"
        valueMethod.fullName shouldBe "foo.Foo.value:java.lang.String()"
        valueMethod.code shouldBe "public String value()"
        valueMethod.lineNumber shouldBe Some(4)
        valueMethod.columnNumber shouldBe Some(12)

        val methodReturn = valueMethod.methodReturn
        methodReturn.typeFullName shouldBe "java.lang.String"
        methodReturn.lineNumber shouldBe Some(4)
        methodReturn.columnNumber shouldBe Some(12)

        inside(valueMethod.parameter.l) { case List(thisParam) =>
          thisParam.name shouldBe "this"
          thisParam.typeFullName shouldBe "foo.Foo"
          thisParam.lineNumber shouldBe Some(4)
          thisParam.columnNumber shouldBe Some(12)
        }

        inside(valueMethod.body.astChildren.l) { case List(returnStmt: Return) =>
          returnStmt.code shouldBe "return this.value"
          returnStmt.lineNumber shouldBe Some(4)
          returnStmt.columnNumber shouldBe Some(12)

          inside(returnStmt.astChildren.l) { case List(fieldAccess: Call) =>
            fieldAccess.name shouldBe Operators.fieldAccess
            fieldAccess.methodFullName shouldBe Operators.fieldAccess
            fieldAccess.code shouldBe "this.value"
            fieldAccess.typeFullName shouldBe "java.lang.String"
            fieldAccess.lineNumber shouldBe Some(4)
            fieldAccess.columnNumber shouldBe Some(12)

            inside(fieldAccess.argument.l) { case List(thisIdentifier: Identifier, fieldIdentifier: FieldIdentifier) =>
              thisIdentifier.name shouldBe "this"
              thisIdentifier.code shouldBe "this"
              thisIdentifier.typeFullName shouldBe "foo.Foo"
              thisIdentifier.refsTo.l shouldBe cpg.method.name("value").parameter.l
              thisIdentifier.lineNumber shouldBe Some(4)
              thisIdentifier.columnNumber shouldBe Some(12)

              fieldIdentifier.canonicalName shouldBe "value"
              fieldIdentifier.code shouldBe "value"
              fieldIdentifier.lineNumber shouldBe Some(4)
              fieldIdentifier.columnNumber shouldBe Some(12)
            }
          }
        }
      }
    }
  }

  "a record with an explicit canonical constructor" should {
    val cpg = code("""
                       |package foo;
                       |
                       |record Foo(String value) {
                       |    public Foo(String value) {
                       |        System.out.println(value);
                       |        this.value = value;
                       |    }
                       |}
                       |""".stripMargin)

    "have the correct constructor" in {
      inside(cpg.method.nameExact("<init>").l) { case List(constructor) =>
        constructor.fullName shouldBe "foo.Foo.<init>:void(java.lang.String)"

        inside(constructor.parameter.l) { case List(thisParam, valueParam) =>
          thisParam.name shouldBe "this"
          thisParam.typeFullName shouldBe "foo.Foo"

          valueParam.name shouldBe "value"
          valueParam.typeFullName shouldBe "java.lang.String"

          inside(constructor.body.astChildren.l) { case List(printlnCall: Call, valueAssign: Call) =>
            printlnCall.name shouldBe "println"
            printlnCall.code shouldBe "System.out.println(value)"

            valueAssign.name shouldBe Operators.assignment
            valueAssign.methodFullName shouldBe Operators.assignment
            valueAssign.typeFullName shouldBe "java.lang.String"
            valueAssign.code shouldBe "this.value = value"

            inside(valueAssign.argument.l) { case List(fieldAccess: Call, valueIdentifier: Identifier) =>
              fieldAccess.name shouldBe Operators.fieldAccess
              fieldAccess.code shouldBe "this.value"
              fieldAccess.typeFullName shouldBe "java.lang.String"

              inside(fieldAccess.argument.l) {
                case List(thisIdentifier: Identifier, valueFieldIdentifier: FieldIdentifier) =>
                  thisIdentifier.name shouldBe "this"
                  thisIdentifier.typeFullName shouldBe "foo.Foo"
                  thisIdentifier.refsTo.l shouldBe List(thisParam)

                  valueFieldIdentifier.canonicalName shouldBe "value"
              }

              valueIdentifier.name shouldBe "value"
              valueIdentifier.typeFullName shouldBe "java.lang.String"
              valueIdentifier.refsTo.l shouldBe List(valueParam)
            }
          }
        }
      }
    }

    "have a private field for the parameter" in {
      inside(cpg.member.l) { case List(valueMember) =>
        valueMember.name shouldBe "value"
        valueMember.code shouldBe "String value"
        valueMember.typeFullName shouldBe "java.lang.String"
        valueMember.modifier.modifierType.l shouldBe List(ModifierTypes.PRIVATE)
      }
    }

    "have a public accessor method for the parameter" in {
      inside(cpg.method.name("value").l) { case List(valueMethod: Method) =>
        valueMethod.name shouldBe "value"
        valueMethod.fullName shouldBe "foo.Foo.value:java.lang.String()"
        valueMethod.code shouldBe "public String value()"
        valueMethod.lineNumber shouldBe Some(4)
        valueMethod.columnNumber shouldBe Some(12)

        val methodReturn = valueMethod.methodReturn
        methodReturn.typeFullName shouldBe "java.lang.String"
        methodReturn.lineNumber shouldBe Some(4)
        methodReturn.columnNumber shouldBe Some(12)

        inside(valueMethod.parameter.l) { case List(thisParam) =>
          thisParam.name shouldBe "this"
          thisParam.typeFullName shouldBe "foo.Foo"
          thisParam.lineNumber shouldBe Some(4)
          thisParam.columnNumber shouldBe Some(12)
        }

        inside(valueMethod.body.astChildren.l) { case List(returnStmt: Return) =>
          returnStmt.code shouldBe "return this.value"
          returnStmt.lineNumber shouldBe Some(4)
          returnStmt.columnNumber shouldBe Some(12)

          inside(returnStmt.astChildren.l) { case List(fieldAccess: Call) =>
            fieldAccess.name shouldBe Operators.fieldAccess
            fieldAccess.methodFullName shouldBe Operators.fieldAccess
            fieldAccess.code shouldBe "this.value"
            fieldAccess.typeFullName shouldBe "java.lang.String"
            fieldAccess.lineNumber shouldBe Some(4)
            fieldAccess.columnNumber shouldBe Some(12)

            inside(fieldAccess.argument.l) { case List(thisIdentifier: Identifier, fieldIdentifier: FieldIdentifier) =>
              thisIdentifier.name shouldBe "this"
              thisIdentifier.code shouldBe "this"
              thisIdentifier.typeFullName shouldBe "foo.Foo"
              thisIdentifier.refsTo.l shouldBe cpg.method.name("value").parameter.l
              thisIdentifier.lineNumber shouldBe Some(4)
              thisIdentifier.columnNumber shouldBe Some(12)

              fieldIdentifier.canonicalName shouldBe "value"
              fieldIdentifier.code shouldBe "value"
              fieldIdentifier.lineNumber shouldBe Some(4)
              fieldIdentifier.columnNumber shouldBe Some(12)
            }
          }
        }
      }
    }
  }

  "a record with a generic parameter" should {
    val cpg = code("""
        |package foo;
        |
        |record Foo<T>(T value) {}
        |""".stripMargin)

    "have the correct default canonical constructor" in {
      inside(cpg.method.nameExact("<init>").l) { case List(constructor) =>
        constructor.fullName shouldBe "foo.Foo.<init>:void(java.lang.Object)"

        inside(constructor.parameter.l) { case List(thisParam, valueParam) =>
          thisParam.name shouldBe "this"
          thisParam.typeFullName shouldBe "foo.Foo"

          valueParam.name shouldBe "value"
          valueParam.typeFullName shouldBe "java.lang.Object"

          inside(constructor.body.astChildren.l) { case List(valueAssign: Call) =>
            valueAssign.name shouldBe Operators.assignment
            valueAssign.methodFullName shouldBe Operators.assignment
            valueAssign.typeFullName shouldBe "java.lang.Object"
            valueAssign.code shouldBe "this.value = value"

            inside(valueAssign.argument.l) { case List(fieldAccess: Call, valueIdentifier: Identifier) =>
              fieldAccess.name shouldBe Operators.fieldAccess
              fieldAccess.code shouldBe "this.value"
              fieldAccess.typeFullName shouldBe "java.lang.Object"

              inside(fieldAccess.argument.l) {
                case List(thisIdentifier: Identifier, valueFieldIdentifier: FieldIdentifier) =>
                  thisIdentifier.name shouldBe "this"
                  thisIdentifier.typeFullName shouldBe "foo.Foo"
                  thisIdentifier.refsTo.l shouldBe List(thisParam)

                  valueFieldIdentifier.canonicalName shouldBe "value"
              }

              valueIdentifier.name shouldBe "value"
              valueIdentifier.typeFullName shouldBe "java.lang.Object"
              valueIdentifier.refsTo.l shouldBe List(valueParam)
            }
          }
        }
      }
    }

    "have a private field for the parameter" in {
      inside(cpg.member.l) { case List(valueMember) =>
        valueMember.name shouldBe "value"
        valueMember.code shouldBe "T value"
        valueMember.typeFullName shouldBe "java.lang.Object"
        valueMember.modifier.modifierType.l shouldBe List(ModifierTypes.PRIVATE)
      }
    }

    "have a public accessor method for the parameter" in {
      inside(cpg.method.name("value").l) { case List(valueMethod: Method) =>
        valueMethod.name shouldBe "value"
        valueMethod.fullName shouldBe "foo.Foo.value:java.lang.Object()"
        valueMethod.code shouldBe "public T value()"
        valueMethod.lineNumber shouldBe Some(4)
        valueMethod.columnNumber shouldBe Some(15)

        val methodReturn = valueMethod.methodReturn
        methodReturn.typeFullName shouldBe "java.lang.Object"
        methodReturn.lineNumber shouldBe Some(4)
        methodReturn.columnNumber shouldBe Some(15)

        inside(valueMethod.parameter.l) { case List(thisParam) =>
          thisParam.name shouldBe "this"
          thisParam.typeFullName shouldBe "foo.Foo"
          thisParam.lineNumber shouldBe Some(4)
          thisParam.columnNumber shouldBe Some(15)
        }

        inside(valueMethod.body.astChildren.l) { case List(returnStmt: Return) =>
          returnStmt.code shouldBe "return this.value"
          returnStmt.lineNumber shouldBe Some(4)
          returnStmt.columnNumber shouldBe Some(15)

          inside(returnStmt.astChildren.l) { case List(fieldAccess: Call) =>
            fieldAccess.name shouldBe Operators.fieldAccess
            fieldAccess.methodFullName shouldBe Operators.fieldAccess
            fieldAccess.code shouldBe "this.value"
            fieldAccess.typeFullName shouldBe "java.lang.Object"
            fieldAccess.lineNumber shouldBe Some(4)
            fieldAccess.columnNumber shouldBe Some(15)

            inside(fieldAccess.argument.l) { case List(thisIdentifier: Identifier, fieldIdentifier: FieldIdentifier) =>
              thisIdentifier.name shouldBe "this"
              thisIdentifier.code shouldBe "this"
              thisIdentifier.typeFullName shouldBe "foo.Foo"
              thisIdentifier.refsTo.l shouldBe cpg.method.name("value").parameter.l
              thisIdentifier.lineNumber shouldBe Some(4)
              thisIdentifier.columnNumber shouldBe Some(15)

              fieldIdentifier.canonicalName shouldBe "value"
              fieldIdentifier.code shouldBe "value"
              fieldIdentifier.lineNumber shouldBe Some(4)
              fieldIdentifier.columnNumber shouldBe Some(15)
            }
          }
        }
      }
    }
  }

  "a simple record with no explicit body" should {
    val cpg = code("""
        |package foo;
        |
        |record Foo(String value) {}
        |""".stripMargin)

    "have the correct default canonical constructor" in {
      inside(cpg.method.nameExact("<init>").l) { case List(constructor) =>
        constructor.fullName shouldBe "foo.Foo.<init>:void(java.lang.String)"

        inside(constructor.parameter.l) { case List(thisParam, valueParam) =>
          thisParam.name shouldBe "this"
          thisParam.typeFullName shouldBe "foo.Foo"

          valueParam.name shouldBe "value"
          valueParam.typeFullName shouldBe "java.lang.String"

          inside(constructor.body.astChildren.l) { case List(valueAssign: Call) =>
            valueAssign.name shouldBe Operators.assignment
            valueAssign.methodFullName shouldBe Operators.assignment
            valueAssign.typeFullName shouldBe "java.lang.String"
            valueAssign.code shouldBe "this.value = value"

            inside(valueAssign.argument.l) { case List(fieldAccess: Call, valueIdentifier: Identifier) =>
              fieldAccess.name shouldBe Operators.fieldAccess
              fieldAccess.code shouldBe "this.value"
              fieldAccess.typeFullName shouldBe "java.lang.String"

              inside(fieldAccess.argument.l) {
                case List(thisIdentifier: Identifier, valueFieldIdentifier: FieldIdentifier) =>
                  thisIdentifier.name shouldBe "this"
                  thisIdentifier.typeFullName shouldBe "foo.Foo"
                  thisIdentifier.refsTo.l shouldBe List(thisParam)

                  valueFieldIdentifier.canonicalName shouldBe "value"
              }

              valueIdentifier.name shouldBe "value"
              valueIdentifier.typeFullName shouldBe "java.lang.String"
              valueIdentifier.refsTo.l shouldBe List(valueParam)
            }
          }
        }
      }
    }

    "have a private field for the parameter" in {
      inside(cpg.member.l) { case List(valueMember) =>
        valueMember.name shouldBe "value"
        valueMember.code shouldBe "String value"
        valueMember.typeFullName shouldBe "java.lang.String"
        valueMember.modifier.modifierType.l shouldBe List(ModifierTypes.PRIVATE)
      }
    }

    "have a public accessor method for the parameter" in {
      inside(cpg.method.name("value").l) { case List(valueMethod: Method) =>
        valueMethod.name shouldBe "value"
        valueMethod.fullName shouldBe "foo.Foo.value:java.lang.String()"
        valueMethod.code shouldBe "public String value()"
        valueMethod.lineNumber shouldBe Some(4)
        valueMethod.columnNumber shouldBe Some(12)

        val methodReturn = valueMethod.methodReturn
        methodReturn.typeFullName shouldBe "java.lang.String"
        methodReturn.lineNumber shouldBe Some(4)
        methodReturn.columnNumber shouldBe Some(12)

        inside(valueMethod.parameter.l) { case List(thisParam) =>
          thisParam.name shouldBe "this"
          thisParam.typeFullName shouldBe "foo.Foo"
          thisParam.lineNumber shouldBe Some(4)
          thisParam.columnNumber shouldBe Some(12)
        }

        inside(valueMethod.body.astChildren.l) { case List(returnStmt: Return) =>
          returnStmt.code shouldBe "return this.value"
          returnStmt.lineNumber shouldBe Some(4)
          returnStmt.columnNumber shouldBe Some(12)

          inside(returnStmt.astChildren.l) { case List(fieldAccess: Call) =>
            fieldAccess.name shouldBe Operators.fieldAccess
            fieldAccess.methodFullName shouldBe Operators.fieldAccess
            fieldAccess.code shouldBe "this.value"
            fieldAccess.typeFullName shouldBe "java.lang.String"
            fieldAccess.lineNumber shouldBe Some(4)
            fieldAccess.columnNumber shouldBe Some(12)

            inside(fieldAccess.argument.l) { case List(thisIdentifier: Identifier, fieldIdentifier: FieldIdentifier) =>
              thisIdentifier.name shouldBe "this"
              thisIdentifier.code shouldBe "this"
              thisIdentifier.typeFullName shouldBe "foo.Foo"
              thisIdentifier.refsTo.l shouldBe cpg.method.name("value").parameter.l
              thisIdentifier.lineNumber shouldBe Some(4)
              thisIdentifier.columnNumber shouldBe Some(12)

              fieldIdentifier.canonicalName shouldBe "value"
              fieldIdentifier.code shouldBe "value"
              fieldIdentifier.lineNumber shouldBe Some(4)
              fieldIdentifier.columnNumber shouldBe Some(12)
            }
          }
        }
      }
    }
  }
}
