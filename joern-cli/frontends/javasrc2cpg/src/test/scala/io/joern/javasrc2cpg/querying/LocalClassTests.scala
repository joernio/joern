package io.joern.javasrc2cpg.querying

import io.shiftleft.semanticcpg.language.*
import io.joern.javasrc2cpg.testfixtures.JavaSrcCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.{Binding, Call, FieldIdentifier, Identifier, Literal}
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.DispatchTypes

class LocalClassTests extends JavaSrcCode2CpgFixture {
  "simple local classes" should {
    val cpg = code("""
        |package foo;
        |
        |class Foo {
        |    int capturedMember;
        |    static int staticMember;
        |
        |    void enclosingMethod(int capturedParam) {
        |        int capturedLocal = 1;
        |        class Local {
        |            void noCaptures(int localParam) {
        |                sink(localParam);
        |            } 
        |
        |            void capturesParam() {
        |                sink(capturedParam);
        |            }
        |
        |            void capturesMember() {
        |                sink(capturedMember);
        |            }
        |
        |            void capturesLocal() {
        |                sink(capturedLocal);
        |            }
        |
        |            void staticAccess() {
        |                sink(staticMember);
        |            }
        |        };
        |    }
        |}
        |""".stripMargin)

    @inline def localDecl = cpg.typeDecl.fullName("foo.Foo.enclosingMethod.Local")

    "have only one Local typeDecl" in {
      cpg.typeDecl.name(".*Local.*").fullName.toSet shouldBe Set("foo.Foo.enclosingMethod.Local")
    }

    "have the correct bindings for methods defined in the local class" in {
      inside(cpg.all.collectAll[Binding].nameExact("noCaptures").l) { case List(noCapturesBinding) =>
        noCapturesBinding.methodFullName shouldBe "foo.Foo.enclosingMethod.Local.noCaptures:void(int)"
        noCapturesBinding.bindingTypeDecl.fullName shouldBe "foo.Foo.enclosingMethod.Local"
        noCapturesBinding.signature shouldBe "void(int)"
      }
    }

    "have the correct binding for the default constructor" in {
      inside(cpg.all.collectAll[Binding].nameExact("<init>").methodFullName(".*Local.*").l) {
        case List(noCapturesBinding) =>
          noCapturesBinding.methodFullName shouldBe "foo.Foo.enclosingMethod.Local.<init>:void()"
          noCapturesBinding.bindingTypeDecl.fullName shouldBe "foo.Foo.enclosingMethod.Local"
          noCapturesBinding.signature shouldBe "void()"
      }
    }

    "have the correct code set" in {
      localDecl.code.l shouldBe List("class Local")
    }

    "have the correct inheritsFromTypeFullName set" in {
      localDecl.inheritsFromTypeFullName.l shouldBe List("java.lang.Object")
    }

    "have the correct line and column numbers set" in {
      localDecl.lineNumber.l shouldBe List(10)
      localDecl.columnNumber.l shouldBe List(9)
    }

    "have the correct number of members created for captures" in {
      localDecl.member.size shouldBe 3
    }

    "have an outerClass member for the captured class" in {
      localDecl.member.name("outerClass").typeFullName.l shouldBe List("foo.Foo")
    }

    "have a member for the captured parameter" in {
      localDecl.member.name("capturedParam").typeFullName.l shouldBe List("int")
    }

    "have a member for the captured local" in {
      localDecl.member.name("capturedLocal").typeFullName.l shouldBe List("int")
    }

    "not have a member for the static member in the outer class" in {
      localDecl.member.name("staticMember").isEmpty shouldBe true
    }

    "not have a member for the captured member in the outer class" in {
      localDecl.member.name("capturedMember").isEmpty shouldBe true
    }

    "have a default constructor with parameters for captured variables" in {
      localDecl.method.nameExact("<init>").parameter.l match {
        case List(thisParam, outerClassParam, capturedLocalParam, capturedParamParam) =>
          thisParam.name shouldBe "this"
          thisParam.typeFullName shouldBe "foo.Foo.enclosingMethod.Local"
          thisParam.index shouldBe 0
          thisParam.dynamicTypeHintFullName shouldBe List("foo.Foo.enclosingMethod.Local")

          outerClassParam.name shouldBe "outerClass"
          outerClassParam.typeFullName shouldBe "foo.Foo"
          outerClassParam.index shouldBe 1

          capturedLocalParam.name shouldBe "capturedLocal"
          capturedLocalParam.typeFullName shouldBe "int"
          capturedLocalParam.index shouldBe 2

          capturedParamParam.name shouldBe "capturedParam"
          capturedParamParam.typeFullName shouldBe "int"
          capturedParamParam.index shouldBe 3

        case result => fail(s"Unexpected result ${result}")
      }
    }

    "have assignments for captured variables in the default constructor" in {
      localDecl.method.nameExact("<init>").l match {
        case List(localInit) =>
          localDecl.method.nameExact("<init>").body.astChildren.l match {
            case List(outerClassAssign: Call, capturedLocalAssign: Call, capturedParamAssign: Call) =>
              outerClassAssign.methodFullName shouldBe Operators.assignment
              capturedLocalAssign.methodFullName shouldBe Operators.assignment
              capturedParamAssign.methodFullName shouldBe Operators.assignment

              outerClassAssign.argument.l match {
                case List(thisOuterClass: Call, outerClass: Identifier) =>
                  thisOuterClass.methodFullName shouldBe Operators.fieldAccess
                  thisOuterClass.argument.l match {
                    case List(thisIdentifier: Identifier, field: FieldIdentifier) =>
                      thisIdentifier.name shouldBe "this"
                      thisIdentifier.typeFullName shouldBe "foo.Foo.enclosingMethod.Local"
                      thisIdentifier.refOut.l shouldBe localInit.parameter.name("this").l

                      field.canonicalName shouldBe "outerClass"

                    case result => fail(s"Unexpected result ${result}")
                  }

                  outerClass.name shouldBe "outerClass"
                  outerClass.typeFullName shouldBe "foo.Foo"
                  outerClass.refOut.l shouldBe localInit.parameter.name("outerClass").l

                case result => fail(s"Unexpected result ${result}")
              }

              capturedLocalAssign.argument.l match {
                case List(thisCapturedLocal: Call, capturedLocal: Identifier) =>
                  thisCapturedLocal.methodFullName shouldBe Operators.fieldAccess
                  thisCapturedLocal.argument.l match {
                    case List(thisIdentifier: Identifier, field: FieldIdentifier) =>
                      thisIdentifier.name shouldBe "this"
                      thisIdentifier.typeFullName shouldBe "foo.Foo.enclosingMethod.Local"
                      thisIdentifier.refOut.l shouldBe localInit.parameter.name("this").l

                      field.canonicalName shouldBe "capturedLocal"
                    case result => fail(s"Unexpected result ${result}")
                  }

                  capturedLocal.name shouldBe "capturedLocal"
                  capturedLocal.typeFullName shouldBe "int"
                  capturedLocal.refOut.l shouldBe localInit.parameter.name("capturedLocal").l
                case result => fail(s"Unexpected result ${result}")
              }

              capturedParamAssign.argument.l match {
                case List(thisCapturedParam: Call, capturedParam: Identifier) =>
                  thisCapturedParam.methodFullName shouldBe Operators.fieldAccess
                  thisCapturedParam.argument.l match {
                    case List(thisIdentifier: Identifier, field: FieldIdentifier) =>
                      thisIdentifier.name shouldBe "this"
                      thisIdentifier.typeFullName shouldBe "foo.Foo.enclosingMethod.Local"
                      thisIdentifier.refOut.l shouldBe localInit.parameter.name("this").l

                      field.canonicalName shouldBe "capturedParam"
                    case result => fail(s"Unexpected result ${result}")
                  }

                  capturedParam.name shouldBe "capturedParam"
                  capturedParam.typeFullName shouldBe "int"
                  capturedParam.refOut.l shouldBe localInit.parameter.name("capturedParam").l
                case result => fail(s"Unexpected result ${result}")
              }
            case result => fail(s"Unexpected result ${result}")
          }
        case result => fail(s"Unexpected result ${result}")
      }
    }
  }

  "local classes in a static context" should {
    val cpg = code("""
        |package foo;
        |
        |class Foo {
        |    int member;
        |    static int staticMember;
        |
        |    static void enclosingMethod(int capturedParam) {
        |        int capturedLocal = 1;
        |        class Local {
        |            void noCaptures(int localParam) {
        |                sink(localParam);
        |            }
        |
        |            void capturesParam() {
        |                sink(capturedParam);
        |            }
        |
        |            void capturesMember() {
        |                // This is not legal Java code
        |                sink(member);
        |            }
        |
        |            void staticAccess() {
        |                sink(staticMember);
        |            }
        |
        |            void capturesLocal() {
        |                sink(capturedLocal);
        |            }
        |        };
        |    }
        |}
        |""".stripMargin)

    @inline def localDecl = cpg.typeDecl.fullName("foo.Foo.enclosingMethod.Local")

    "have only one Local typeDecl" in {
      cpg.typeDecl.name(".*Local.*").fullName.toSet shouldBe Set("foo.Foo.enclosingMethod.Local")
    }

    "have the correct number of members created for captures" in {
      localDecl.member.size shouldBe 2
    }

    "have an not outerClass member for a captured class" in {
      localDecl.member.name("outerClass").isEmpty shouldBe true
    }

    "have a member for the captured parameter" in {
      localDecl.member.name("capturedParam").typeFullName.l shouldBe List("int")
    }

    "have a member for the captured local" in {
      localDecl.member.name("capturedLocal").typeFullName.l shouldBe List("int")
    }

    "not have a member for the static member in the outer class" in {
      localDecl.member.name("staticMember").isEmpty shouldBe true
    }

    "not have a member for the captured member in the outer class" in {
      localDecl.member.name("capturedMember").isEmpty shouldBe true
    }

    "have a default constructor with a parameter for the captured parameter" in {
      localDecl.method.nameExact("<init>").parameter.l match {
        case List(thisParam, capturedLocalParam, capturedParamParam) =>
          thisParam.name shouldBe "this"
          thisParam.typeFullName shouldBe "foo.Foo.enclosingMethod.Local"
          thisParam.index shouldBe 0
          thisParam.dynamicTypeHintFullName shouldBe List("foo.Foo.enclosingMethod.Local")

          capturedLocalParam.name shouldBe "capturedLocal"
          capturedLocalParam.typeFullName shouldBe "int"
          capturedLocalParam.index shouldBe 1

          capturedParamParam.name shouldBe "capturedParam"
          capturedParamParam.typeFullName shouldBe "int"
          capturedParamParam.index shouldBe 2
        case result => fail(s"Unexpected result ${result.map(_.name)}")
      }
    }

    "have an assignment for the captured parameter in the default constructor" in {
      localDecl.method.nameExact("<init>").l match {
        case List(localInit) =>
          localDecl.method.nameExact("<init>").body.astChildren.l match {
            case List(capturedLocalAssign: Call, capturedParamAssign: Call) =>
              capturedParamAssign.methodFullName shouldBe Operators.assignment
              capturedLocalAssign.methodFullName shouldBe Operators.assignment

              capturedParamAssign.argument.l match {
                case List(thisCapturedParam: Call, capturedParam: Identifier) =>
                  thisCapturedParam.methodFullName shouldBe Operators.fieldAccess
                  thisCapturedParam.argument.l match {
                    case List(thisIdentifier: Identifier, field: FieldIdentifier) =>
                      thisIdentifier.name shouldBe "this"
                      thisIdentifier.typeFullName shouldBe "foo.Foo.enclosingMethod.Local"
                      thisIdentifier.refOut.l shouldBe localInit.parameter.name("this").l

                      field.canonicalName shouldBe "capturedParam"
                    case result => fail(s"Unexpected result ${result}")
                  }

                  capturedParam.name shouldBe "capturedParam"
                  capturedParam.typeFullName shouldBe "int"
                  capturedParam.refOut.l shouldBe localInit.parameter.name("capturedParam").l
                case result => fail(s"Unexpected result ${result}")
              }

              capturedLocalAssign.argument.l match {
                case List(thisCapturedLocal: Call, capturedLocal: Identifier) =>
                  thisCapturedLocal.methodFullName shouldBe Operators.fieldAccess
                  thisCapturedLocal.argument.l match {
                    case List(thisIdentifier: Identifier, field: FieldIdentifier) =>
                      thisIdentifier.name shouldBe "this"
                      thisIdentifier.typeFullName shouldBe "foo.Foo.enclosingMethod.Local"
                      thisIdentifier.refOut.l shouldBe localInit.parameter.name("this").l

                      field.canonicalName shouldBe "capturedLocal"
                    case result => fail(s"Unexpected result ${result}")
                  }

                  capturedLocal.name shouldBe "capturedLocal"
                  capturedLocal.typeFullName shouldBe "int"
                  capturedLocal.refOut.l shouldBe localInit.parameter.name("capturedLocal").l
                case result => fail(s"Unexpected result ${result}")
              }
            case result => fail(s"Unexpected result ${result}")
          }
        case result => fail(s"Unexpected result ${result}")
      }
    }
  }

  "object creation expressions for local classes without used captures" should {
    val cpg = code("""
        |package foo;
        |
        |class Foo {
        |    void enclosingMethod(int outerParam) {
        |        int outerLocal = 2;
        |        class Local {
        |            String foo() {
        |                return "Local";
        |            }
        |        };
        |        Local l = new Local();
        |    }
        |}
        |""".stripMargin)

    @inline def localDecl = cpg.typeDecl.fullName("foo.Foo.enclosingMethod.Local")

    "have only one Local typeDecl" in {
      cpg.typeDecl.name(".*Local.*").fullName.toSet shouldBe Set("foo.Foo.enclosingMethod.Local")
    }

    "have a field for the captured outer class" in {
      localDecl.member.l match {
        case List(outerClassMember) =>
          outerClassMember.name shouldBe "outerClass"
          outerClassMember.typeFullName shouldBe "foo.Foo"
        case result => fail(s"Unexpected result ${result}")
      }
    }

    "have an init parameter for the captured outer class" in {
      localDecl.method.nameExact("<init>").parameter.l match {
        case List(thisParam, outerClassParam) =>
          thisParam.name shouldBe "this"
          thisParam.index shouldBe 0
          thisParam.typeFullName shouldBe "foo.Foo.enclosingMethod.Local"
          thisParam.dynamicTypeHintFullName shouldBe List("foo.Foo.enclosingMethod.Local")

          outerClassParam.name shouldBe "outerClass"
          outerClassParam.typeFullName shouldBe "foo.Foo"
          outerClassParam.index shouldBe 1

        case result => fail(s"Found unexpected output ${result}")
      }
    }

    "have an assignment for the captured outer class in the default constructor" in {
      localDecl.method.nameExact("<init>").l match {
        case List(localInit) =>
          localDecl.method.nameExact("<init>").body.astChildren.l match {
            case List(outerClassAssign: Call) =>
              outerClassAssign.methodFullName shouldBe Operators.assignment
              outerClassAssign.argument.l match {
                case List(thisOuterClass: Call, outerClass: Identifier) =>
                  thisOuterClass.methodFullName shouldBe Operators.fieldAccess
                  thisOuterClass.argument.l match {
                    case List(thisIdentifier: Identifier, field: FieldIdentifier) =>
                      thisIdentifier.name shouldBe "this"
                      thisIdentifier.typeFullName shouldBe "foo.Foo.enclosingMethod.Local"
                      thisIdentifier.refOut.l shouldBe localInit.parameter.name("this").l

                      field.canonicalName shouldBe "outerClass"
                    case result => fail(s"Unexpected result ${result}")
                  }

                  outerClass.name shouldBe "outerClass"
                  outerClass.typeFullName shouldBe "foo.Foo"
                  outerClass.refOut.l shouldBe localInit.parameter.name("outerClass").l
                case result => fail(s"Unexpected result ${result}")
              }
            case result => fail(s"Unexpected result ${result}")
          }
        case result => fail(s"Unexpected result ${result}")
      }
    }
  }

  // TODO: Actually add object creation test
  "object creation expressions for local classes in a static context without used captures" should {
    val cpg = code("""
        |package foo;
        |
        |class Foo {
        |    static void enclosingMethod(int outerParam) {
        |        int outerLocal = 2;
        |        class Local {
        |            String foo() {
        |                return "Local";
        |            }
        |        };
        |    }
        |}
        |""".stripMargin)

    @inline def localDecl = cpg.typeDecl.fullName("foo.Foo.enclosingMethod.Local")

    "have only one Local typeDecl" in {
      cpg.typeDecl.name(".*Local.*").fullName.toSet shouldBe Set("foo.Foo.enclosingMethod.Local")
    }

    "not have a field for the captured outer class" in {
      localDecl.member.isEmpty shouldBe true
    }

    "not have an init parameter for the outer class" in {
      localDecl.method.nameExact("<init>").parameter.l match {
        case List(thisParam) =>
          thisParam.name shouldBe "this"
          thisParam.index shouldBe 0
          thisParam.typeFullName shouldBe "foo.Foo.enclosingMethod.Local"
          thisParam.dynamicTypeHintFullName shouldBe List("foo.Foo.enclosingMethod.Local")
        case result => fail(s"Unexpected result ${result}")
      }
    }

    "not have an assignment for the outer class in the default constructor" in {
      localDecl.method.nameExact("<init>").body.astChildren.isEmpty shouldBe true
    }
  }

  "local classes with some used captures and some unused" should {
    val cpg = code("""
        |package foo;
        |
        |class Foo {
        |    int capturedMember;
        |    static int staticMember;
        |
        |    void enclosingMethod(int capturedParam) {
        |        int capturedLocal = 1;
        |        class Local {
        |            void noCaptures(int localParam) {
        |                sink(localParam);
        |            } 
        |
        |            void capturesMember() {
        |                sink(capturedMember);
        |            }
        |
        |            void capturesLocal() {
        |                sink(capturedLocal);
        |            }
        |
        |            void staticAccess() {
        |                sink(staticMember);
        |            }
        |        };
        |    }
        |}
        |""".stripMargin)

    @inline def localDecl = cpg.typeDecl.fullName("foo.Foo.enclosingMethod.Local")

    "have only one Local typeDecl" in {
      cpg.typeDecl.name(".*Local.*").fullName.toSet shouldBe Set("foo.Foo.enclosingMethod.Local")
    }

    "have the correct full name set" in {
      localDecl.fullName.l shouldBe List("foo.Foo.enclosingMethod.Local")
    }

    "have the correct code set" in {
      localDecl.code.l shouldBe List("class Local")
    }

    "have the correct inheritsFromTypeFullName set" in {
      localDecl.inheritsFromTypeFullName.l shouldBe List("java.lang.Object")
    }

    "have the correct line and column numbers set" in {
      localDecl.lineNumber.l shouldBe List(10)
      localDecl.columnNumber.l shouldBe List(9)
    }

    "have the correct number of members created for captures" in {
      localDecl.member.size shouldBe 2
    }

    "have an outerClass member for the captured class" in {
      localDecl.member.name("outerClass").typeFullName.l shouldBe List("foo.Foo")
    }

    "not have a member for the unused captured parameter" in {
      localDecl.member.name("capturedParam").isEmpty shouldBe true
    }

    "have a member for the captured local" in {
      localDecl.member.name("capturedLocal").typeFullName.l shouldBe List("int")
    }

    "not have a member for the static member in the outer class" in {
      localDecl.member.name("staticMember").isEmpty shouldBe true
    }

    "not have a member for the captured member in the outer class" in {
      localDecl.member.name("capturedMember").isEmpty shouldBe true
    }

    "have a default constructor with parameters for used captures" in {
      localDecl.method.nameExact("<init>").parameter.l match {
        case List(thisParam, outerClassParam, capturedLocalParam) =>
          thisParam.name shouldBe "this"
          thisParam.typeFullName shouldBe "foo.Foo.enclosingMethod.Local"
          thisParam.index shouldBe 0
          thisParam.dynamicTypeHintFullName shouldBe List("foo.Foo.enclosingMethod.Local")

          outerClassParam.name shouldBe "outerClass"
          outerClassParam.typeFullName shouldBe "foo.Foo"
          outerClassParam.index shouldBe 1

          capturedLocalParam.name shouldBe "capturedLocal"
          capturedLocalParam.typeFullName shouldBe "int"
          capturedLocalParam.index shouldBe 2

        case result => fail(s"Unexpected result ${result}")
      }
    }

    "have assignments for used captures in the default constructor" in {
      localDecl.method.nameExact("<init>").l match {
        case List(localInit) =>
          localDecl.method.nameExact("<init>").body.astChildren.l match {
            case List(outerClassAssign: Call, capturedLocalAssign: Call) =>
              outerClassAssign.methodFullName shouldBe Operators.assignment
              capturedLocalAssign.methodFullName shouldBe Operators.assignment

              outerClassAssign.argument.l match {
                case List(thisOuterClass: Call, outerClass: Identifier) =>
                  thisOuterClass.methodFullName shouldBe Operators.fieldAccess
                  thisOuterClass.argument.l match {
                    case List(thisIdentifier: Identifier, field: FieldIdentifier) =>
                      thisIdentifier.name shouldBe "this"
                      thisIdentifier.typeFullName shouldBe "foo.Foo.enclosingMethod.Local"
                      thisIdentifier.refOut.l shouldBe localInit.parameter.name("this").l

                      field.canonicalName shouldBe "outerClass"
                    case result => fail(s"Unexpected result ${result}")
                  }

                  outerClass.name shouldBe "outerClass"
                  outerClass.typeFullName shouldBe "foo.Foo"
                  outerClass.refOut.l shouldBe localInit.parameter.name("outerClass").l
                case result => fail(s"Unexpected result ${result}")
              }

              capturedLocalAssign.argument.l match {
                case List(thisCapturedLocal: Call, capturedLocal: Identifier) =>
                  thisCapturedLocal.methodFullName shouldBe Operators.fieldAccess
                  thisCapturedLocal.argument.l match {
                    case List(thisIdentifier: Identifier, field: FieldIdentifier) =>
                      thisIdentifier.name shouldBe "this"
                      thisIdentifier.typeFullName shouldBe "foo.Foo.enclosingMethod.Local"
                      thisIdentifier.refOut.l shouldBe localInit.parameter.name("this").l

                      field.canonicalName shouldBe "capturedLocal"
                    case result => fail(s"Unexpected result ${result}")
                  }

                  capturedLocal.name shouldBe "capturedLocal"
                  capturedLocal.typeFullName shouldBe "int"
                  capturedLocal.refOut.l shouldBe localInit.parameter.name("capturedLocal").l
                case result => fail(s"Unexpected result ${result}")
              }
            case result => fail(s"Unexpected result ${result}")
          }
        case result => fail(s"Unexpected result ${result}")
      }
    }
  }

  "local classes with a single explicit constructor" should {
    val cpg = code("""
        |package foo;
        |
        |class Foo {
        |    void enclosingMethod(int outerParam) {
        |        int outerLocal = 2;
        |        class Local {
        |            public Local(int ctxParam) {
        |              sink(ctxParam);
        |            }
        |
        |            void captureOuters() {
        |              sink(outerParam + outerLocal);
        |            }
        |        };
        |    }
        |}
        |""".stripMargin)

    @inline def localConstructor = cpg.typeDecl.name(".*Local.*").method.nameExact("<init>")

    "have the original parameter followed by params for captures" in {
      localConstructor.parameter.l match {
        case List(thisParam, ctxParam, outerClassParam, outerLocalParam, outerParamParam) =>
          thisParam.name shouldBe "this"
          thisParam.index shouldBe 0

          ctxParam.name shouldBe "ctxParam"
          ctxParam.index shouldBe 1

          outerClassParam.name shouldBe "outerClass"
          outerClassParam.index shouldBe 2

          outerLocalParam.name shouldBe "outerLocal"
          outerLocalParam.index shouldBe 3

          outerParamParam.name shouldBe "outerParam"
          outerParamParam.index shouldBe 4

        case result => fail(s"Unexpected result ${result}")
      }
    }

    "have assignments for captures before the constructor body" in {
      localConstructor.body.astChildren.code.l shouldBe List(
        "this.outerClass = outerClass",
        "this.outerLocal = outerLocal",
        "this.outerParam = outerParam",
        "sink(ctxParam)"
      )
    }
  }

  "local classes with multiple unchained explicit constructors" should {
    val cpg = code("""
        |package foo;
        |
        |class Foo {
        |    void enclosingMethod(int outerParam) {
        |        class Local {
        |            public Local() { }
        |
        |            public Local(int ctxParam) {
        |              sink(ctxParam);
        |            }
        |
        |            void captureOuters() {
        |              sink(outerParam);
        |            }
        |        };
        |    }
        |}
        |""".stripMargin)

    @inline def constructors =
      cpg.typeDecl.fullName("foo.Foo.enclosingMethod.Local").method.nameExact("<init>").sortBy(_.parameter.size)

    "have correct bindings for the explicit constructors" in {
      val bindings = cpg.all.collectAll[Binding].toList
      inside(cpg.all.collectAll[Binding].nameExact("<init>").methodFullName(".*Local.*").sortBy(_.signature.length).l) {
        case List(noArgBinding, explicitArgBinding) =>
          noArgBinding.methodFullName shouldBe "foo.Foo.enclosingMethod.Local.<init>:void()"
          noArgBinding.bindingTypeDecl.fullName shouldBe "foo.Foo.enclosingMethod.Local"
          noArgBinding.signature shouldBe "void()"

          explicitArgBinding.methodFullName shouldBe "foo.Foo.enclosingMethod.Local.<init>:void(int)"
          explicitArgBinding.bindingTypeDecl.fullName shouldBe "foo.Foo.enclosingMethod.Local"
          explicitArgBinding.signature shouldBe "void(int)"
      }
    }

    "have params for captured members for both constructors" in {
      constructors.head.parameter.name.l shouldBe List("this", "outerClass", "outerParam")
      constructors.last.parameter.name.l shouldBe List("this", "ctxParam", "outerClass", "outerParam")
    }

    "have assignments for the captures in both constructors" in {
      constructors.head.body.astChildren.code.l shouldBe List(
        "this.outerClass = outerClass",
        "this.outerParam = outerParam"
      )
      constructors.last.body.astChildren.code.l shouldBe List(
        "this.outerClass = outerClass",
        "this.outerParam = outerParam",
        "sink(ctxParam)"
      )
    }
  }

  "local classes with chained explicit constructors" should {
    val cpg = code("""
        |package foo;
        |
        |class Foo {
        |    void enclosingMethod(int outerParam) {
        |        class Local {
        |            public Local() { }
        |
        |            public Local(int ctxParam) {
        |              this();
        |              sink(ctxParam);
        |            }
        |
        |            void captureOuters() {
        |              sink(outerParam);
        |            }
        |        };
        |    }
        |}
        |""".stripMargin)

    @inline def constructors =
      cpg.typeDecl.fullName("foo.Foo.enclosingMethod.Local").method.nameExact("<init>").sortBy(_.parameter.size)

    "not have any orphan locals or parameters" in {
      cpg.local.filter(_.astIn.isEmpty).l shouldBe List()
      cpg.parameter.filter(_.astIn.isEmpty).l shouldBe List()
    }

    "have ref edges from the outer class identifier to the parameter" in {
      inside(cpg.method.nameExact("<init>").filter(_.parameter.name.contains("ctxParam")).l) { case List(constructor) =>
        constructor.ast.isIdentifier.name("outerClass").refsTo.l shouldBe constructor.parameter.name("outerClass").l
      }
    }

    "have params for captured members for both constructors" in {
      constructors.head.parameter.name.l shouldBe List("this", "outerClass", "outerParam")
      constructors.last.parameter.name.l shouldBe List("this", "ctxParam", "outerClass", "outerParam")
    }

    "have assignments for the captures only in constructors that do not call further constructors" in {
      constructors.head.body.astChildren.code.l shouldBe List(
        "this.outerClass = outerClass",
        "this.outerParam = outerParam"
      )
      val lastBody = constructors.last.body.astChildren.l
      inside(constructors.last.body.astChildren.l) { case List(thisCall: Call, sinkCall: Call) =>
        thisCall.argument.code.l shouldBe List("this", "outerClass", "outerParam")
        sinkCall.code shouldBe "sink(ctxParam)"
      }
    }
  }

  "local classes with calls to captured methods" should {
    val cpg = code("""
        |package foo;
        |
        |class Foo {
        |    void foo() {}
        |
        |    void enclosingMethod(int outerParam) {
        |        class Local {
        |            void callsOuter() {
        |                foo();
        |            }
        |        };
        |    }
        |}
        |""".stripMargin)

    "call the captured method on the outerClass field" in {
      cpg.call.name("foo").l match {
        case List(fooCall) =>
          fooCall.name shouldBe "foo"
          fooCall.methodFullName shouldBe "foo.Foo.foo:void()"
          fooCall.code shouldBe "this.outerClass.foo()"
          fooCall.signature shouldBe "void()"
          fooCall.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH

          fooCall.receiver.l match {
            case List(receiverFieldAccess: Call) =>
              receiverFieldAccess.name shouldBe Operators.fieldAccess
              receiverFieldAccess.typeFullName shouldBe "foo.Foo"

              receiverFieldAccess.argument.l match {
                case List(thisIdentifier: Identifier, outerClassFieldId: FieldIdentifier) =>
                  thisIdentifier.name shouldBe "this"
                  thisIdentifier.typeFullName shouldBe "foo.Foo.enclosingMethod.Local"

                  outerClassFieldId.canonicalName shouldBe "outerClass"
                case result => fail(s"Unexpected result ${result}")
              }
            case result => fail(s"Unexpected result ${result}")
          }
        case result => fail(s"Unexpected result ${result}")
      }
    }
  }

  "local classes with calls to static methods in the outer class" should {
    val cpg = code("""
        |package foo;
        |
        |class Foo {
        |    static void foo() {}
        |
        |    void enclosingMethod(int outerParam) {
        |        class Local {
        |            void callsOuter() {
        |                foo();
        |            }
        |        };
        |    }
        |}
        |""".stripMargin)

    "not represent the call as a call to the outerClass field" in {
      cpg.call.name("foo").l match {
        case List(fooCall) =>
          fooCall.name shouldBe "foo"
          fooCall.methodFullName shouldBe "foo.Foo.foo:void()"
          fooCall.code shouldBe "foo()"
          fooCall.signature shouldBe "void()"
          fooCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
          fooCall.receiver.isEmpty shouldBe true
        case result => fail(s"Unexpected result ${result}")
      }
    }
  }

  "object creation expressions for local classes" when {
    "it is a local class with captures and explicit constructor arguments" should {
      val cpg = code("""
          |package foo;
          |
          |class Foo {
          |  int fooMember;
          |
          |  void fooMethod(int fooParam) {
          |    int fooLocal = 0;
          |    class Local {
          |      public Local(int argument) {}
          |
          |      void usesCaptures() {
          |        sink(fooMember, fooLocal, fooParam);
          |      }
          |    }
          |
          |    sink(new Local(0));
          |  }
          |}
          |""".stripMargin)

      @inline def initCall = cpg.call.name("sink").ast.collectAll[Call].nameExact("<init>")

      "have the correct init node properties" in {
        initCall.l match {
          case List(call) =>
            call.methodFullName shouldBe "foo.Foo.fooMethod.Local.<init>:void(int)"
            call.signature shouldBe "void(int)"
          // pendingUntilFixed(call.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH)
          case result => fail(s"Unexpected result ${result}")
        }
      }

      "have the correct init receiver" in {
        initCall.receiver.l match {
          case List(receiver: Identifier) =>
            receiver.typeFullName shouldBe "foo.Foo.fooMethod.Local"
          case result => fail(s"Unexpected result ${result}")
        }
      }

      "have the explicit and capture arguments in the correct order" in {
        initCall.argument.l match {
          case List(
                _,
                explicitArg: Literal,
                outerClass: Identifier,
                capturedLocal: Identifier,
                capturedParam: Identifier
              ) =>
            explicitArg.code shouldBe "0"
            explicitArg.argumentIndex shouldBe 1

            outerClass.name shouldBe "this"
            outerClass.argumentIndex shouldBe 2
            outerClass.refOut.l shouldBe cpg.method.name("fooMethod").parameter.index(0).l

            capturedLocal.name shouldBe "fooLocal"
            capturedLocal.argumentIndex shouldBe 3
            capturedLocal.refOut.l shouldBe cpg.method.name("fooMethod").local.name("fooLocal").l

            capturedParam.name shouldBe "fooParam"
            capturedParam.argumentIndex shouldBe 4
            capturedParam.refOut.l shouldBe cpg.method.name("fooMethod").parameter.name("fooParam").l

          case result => fail(s"Unexpected result ${result}")
        }
      }
    }

    "it is a local class with captures and no explicit constructor arguments" should {
      val cpg = code("""
          |package foo;
          |
          |class Foo {
          |  int fooMember;
          |
          |  void foo(int fooParam) {
          |    int fooLocal = 0;
          |
          |    class Local {
          |      public Local() {}
          |      void usesCaptures() {
          |        sink(fooMember, fooLocal, fooParam);
          |      }
          |    }
          |
          |    sink(new Local());
          |  }
          |}
          |""".stripMargin)

      @inline def initCall = cpg.call.name("sink").ast.collectAll[Call].nameExact("<init>")

      "have the correct init node properties" in {
        initCall.l match {
          case List(call) =>
            call.methodFullName shouldBe "foo.Foo.foo.Local.<init>:void()"
            call.signature shouldBe "void()"
          // pendingUntilFixed(call.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH)
          case result => fail(s"Unexpected result ${result}")
        }
      }

      "have the correct init receiver" in {
        initCall.receiver.l match {
          case List(receiver: Identifier) =>
            receiver.typeFullName shouldBe "foo.Foo.foo.Local"
          case result => fail(s"Unexpected result ${result}")
        }
      }

      "have the explicit and capture arguments in the correct order" in {
        initCall.argument.l match {
          case List(_, outerClass: Identifier, capturedLocal: Identifier, capturedParam: Identifier) =>
            outerClass.name shouldBe "this"
            outerClass.argumentIndex shouldBe 1
            outerClass.refOut.l shouldBe cpg.method.name("foo").parameter.index(0).l

            capturedLocal.name shouldBe "fooLocal"
            capturedLocal.argumentIndex shouldBe 2
            capturedLocal.refOut.l shouldBe cpg.method.name("foo").local.name("fooLocal").l

            capturedParam.name shouldBe "fooParam"
            capturedParam.argumentIndex shouldBe 3
            capturedParam.refOut.l shouldBe cpg.method.name("foo").parameter.name("fooParam").l
          case result => fail(s"Unexpected result ${result}")
        }
      }
    }

    "it is a local class with only some used captures" should {
      val cpg = code("""
          |package foo;
          |
          |class Foo {
          |  int fooMember;
          |
          |  void fooMethod(int fooParam) {
          |    int fooLocal = 0;
          |
          |    class Local {
          |      public Local() {}
          |      void usesCaptures() {
          |        sink(fooMember, fooLocal);
          |      }
          |    }
          |
          |    sink(new Local());
          |  }
          |}
          |""".stripMargin)

      @inline def initCall = cpg.call.name("sink").ast.collectAll[Call].nameExact("<init>")

      "have the correct init node properties" in {
        initCall.l match {
          case List(call) =>
            call.methodFullName shouldBe "foo.Foo.fooMethod.Local.<init>:void()"
            call.signature shouldBe "void()"
          // pendingUntilFixed(call.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH)
          case result => fail(s"Unexpected result ${result}")
        }
      }

      "have the correct init receiver" in {
        initCall.receiver.l match {
          case List(receiver: Identifier) =>
            receiver.typeFullName shouldBe "foo.Foo.fooMethod.Local"
          case result => fail(s"Unexpected result ${result}")
        }
      }

      "have the explicit and capture arguments in the correct order" in {
        initCall.argument.l match {
          case List(_, outerClass: Identifier, capturedLocal: Identifier) =>
            outerClass.name shouldBe "this"
            outerClass.argumentIndex shouldBe 1
            outerClass.refOut.l shouldBe cpg.method.name("fooMethod").parameter.index(0).l

            capturedLocal.name shouldBe "fooLocal"
            capturedLocal.argumentIndex shouldBe 2
            capturedLocal.refOut.l shouldBe cpg.method.name("fooMethod").local.name("fooLocal").l
          case result => fail(s"Unexpected result ${result}")
        }
      }
    }

    "it is a local class without any captures" should {
      val cpg = code("""
          |package foo;
          |
          |class Foo {
          |  int fooMember;
          |
          |  void fooMethod(int fooParam) {
          |    int fooLocal = 0;
          |
          |    class Local {
          |      public Local() {}
          |    }
          |
          |    sink(new Local());
          |  }
          |}
          |""".stripMargin)

      @inline def initCall = cpg.call.name("sink").ast.collectAll[Call].nameExact("<init>")

      "have the correct init node properties" in {
        initCall.l match {
          case List(call) =>
            call.methodFullName shouldBe "foo.Foo.fooMethod.Local.<init>:void()"
            call.signature shouldBe "void()"
          // pendingUntilFixed(call.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH)
          case result => fail(s"Unexpected result ${result}")
        }
      }

      "have the correct init receiver" in {
        initCall.receiver.l match {
          case List(receiver: Identifier) =>
            receiver.typeFullName shouldBe "foo.Foo.fooMethod.Local"
          case result => fail(s"Unexpected result ${result}")
        }
      }

      "have the explicit and capture arguments in the correct order" in {
        initCall.argument.l match {
          case List(_, outerClass: Identifier) =>
            outerClass.name shouldBe "this"
            outerClass.argumentIndex shouldBe 1
            outerClass.refOut.l shouldBe cpg.method.name("fooMethod").parameter.index(0).l
          case result => fail(s"Unexpected result ${result}")
        }
      }
    }

    "it is a local class in a static context with captures all used" should {
      val cpg = code("""
          |package foo;
          |
          |class Foo {
          |  int fooMember;
          |
          |  static void fooMethod(int fooParam) {
          |    int fooLocal = 0;
          |    class Local {
          |      public Local(int argument) {}
          |
          |      void usesCaptures() {
          |        sink(fooLocal, fooParam);
          |      }
          |    }
          |
          |    sink(new Local(0));
          |  }
          |}
          |""".stripMargin)

      @inline def initCall = cpg.call.name("sink").ast.collectAll[Call].nameExact("<init>")

      "have the correct init node properties" in {
        initCall.l match {
          case List(call) =>
            call.methodFullName shouldBe "foo.Foo.fooMethod.Local.<init>:void(int)"
            call.signature shouldBe "void(int)"
          // pendingUntilFixed(call.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH)
          case result => fail(s"Unexpected result ${result}")
        }
      }

      "have the correct init receiver" in {
        initCall.receiver.l match {
          case List(receiver: Identifier) =>
            receiver.typeFullName shouldBe "foo.Foo.fooMethod.Local"
          case result => fail(s"Unexpected result ${result}")
        }
      }

      "have the explicit and capture arguments in the correct order" in {
        initCall.argument.l match {
          case List(_, explicitArg: Literal, capturedLocal: Identifier, capturedParam: Identifier) =>
            explicitArg.code shouldBe "0"
            explicitArg.argumentIndex shouldBe 1

            capturedLocal.name shouldBe "fooLocal"
            capturedLocal.argumentIndex shouldBe 2
            capturedLocal.refOut.l shouldBe cpg.method.name("fooMethod").local.name("fooLocal").l

            capturedParam.name shouldBe "fooParam"
            capturedParam.argumentIndex shouldBe 3
            capturedParam.refOut.l shouldBe cpg.method.name("fooMethod").parameter.name("fooParam").l
          case result => fail(s"Unexpected result ${result}")
        }
      }
    }

    "it is a local class in a static context with only some used captures" should {
      val cpg = code("""
          |package foo;
          |
          |class Foo {
          |  int fooMember;
          |
          |  static void fooMethod(int fooParam) {
          |    int fooLocal = 0;
          |    class Local {
          |      public Local() {}
          |
          |      void usesCaptures() {
          |        sink(fooLocal);
          |      }
          |    }
          |
          |    sink(new Local());
          |  }
          |}
          |""".stripMargin)

      @inline def initCall = cpg.call.name("sink").ast.collectAll[Call].nameExact("<init>")

      "have the correct init node properties" in {
        initCall.l match {
          case List(call) =>
            call.methodFullName shouldBe "foo.Foo.fooMethod.Local.<init>:void()"
            call.signature shouldBe "void()"
          // pendingUntilFixed(call.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH)
          case result => fail(s"Unexpected result ${result}")
        }
      }

      "have the correct init receiver" in {
        initCall.receiver.l match {
          case List(receiver: Identifier) =>
            receiver.typeFullName shouldBe "foo.Foo.fooMethod.Local"
          case result => fail(s"Unexpected result ${result}")
        }
      }

      "have the explicit and capture arguments in the correct order" in {
        initCall.argument.l match {
          case List(_, capturedLocal: Identifier) =>
            capturedLocal.name shouldBe "fooLocal"
            capturedLocal.argumentIndex shouldBe 1
            capturedLocal.refOut.l shouldBe cpg.method.name("fooMethod").local.name("fooLocal").l
          case result => fail(s"Unexpected result ${result}")
        }
      }
    }

    "it is a local class in a static context without any captures" should {
      val cpg = code("""
          |package foo;
          |
          |class Foo {
          |  int fooMember;
          |
          |  static void fooMethod(int fooParam) {
          |    int fooLocal = 0;
          |    class Local {
          |      public Local(int argument) {}
          |    }
          |
          |    sink(new Local(0));
          |  }
          |}
          |""".stripMargin)

      @inline def initCall = cpg.call.name("sink").ast.collectAll[Call].nameExact("<init>")

      "have the correct init node properties" in {
        initCall.l match {
          case List(call) =>
            call.methodFullName shouldBe "foo.Foo.fooMethod.Local.<init>:void(int)"
            call.signature shouldBe "void(int)"
          // pendingUntilFixed(call.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH)
          case result => fail(s"Unexpected result ${result}")
        }
      }

      "have the correct init receiver" in {
        initCall.receiver.l match {
          case List(receiver: Identifier) =>
            receiver.typeFullName shouldBe "foo.Foo.fooMethod.Local"
          case result => fail(s"Unexpected result ${result}")
        }
      }

      "only the explicit arg" in {
        initCall.argument.l match {
          case List(_, explicitArg: Literal) =>
            explicitArg.code shouldBe "0"
            explicitArg.argumentIndex shouldBe 1
          case result => fail(s"Unexpected result ${result}")
        }
      }
    }
  }

  "nested local classes without any static contexts" when {
    "there is a member access to an outer class member, which is in turn a capture" should {
      val cpg = code("""
          |package foo;
          |
          |class Foo {
          |  void foo(int fooParam) {
          |    class Bar {
          |      void bar() {
          |        class Baz {
          |          void baz() {
          |            sink(fooParam);
          |          }
          |        }
          |      }
          |    }
          |  }
          |}
          |""".stripMargin)

      "have the correct nested structure for the access" in {
        cpg.call.name("sink").argument.l match {
          case List(thisArg: Identifier, fooAccess: Call) =>
            thisArg.name shouldBe "this"

            fooAccess.name shouldBe Operators.fieldAccess
            fooAccess.typeFullName shouldBe "int"

            fooAccess.argument.l match {
              case List(outerClassAccess: Call, fooFieldIdentifier: FieldIdentifier) =>
                outerClassAccess.name shouldBe Operators.fieldAccess
                outerClassAccess.typeFullName shouldBe "foo.Foo.foo.Bar"

                outerClassAccess.argument.l match {
                  case List(thisIdentifier: Identifier, outerFieldIdentifier: FieldIdentifier) =>
                    thisIdentifier.name shouldBe "this"
                    thisIdentifier.typeFullName shouldBe "foo.Foo.foo.Bar.bar.Baz"
                    thisIdentifier.refOut.l shouldBe cpg.method.name("baz").parameter.index(0).l

                    outerFieldIdentifier.canonicalName shouldBe "outerClass"
                  case result => fail(s"Unexpected result ${result}")
                }

                fooFieldIdentifier.canonicalName shouldBe "fooParam"
              case result => fail(s"Unexpected result ${result}")
            }
          case result => fail(s"Unexpected result ${result}")
        }
      }

      "have a constructor parameter for the captured param in the outer local class" in {
        cpg.typeDecl.name("Bar").method.nameExact("<init>").parameter.l match {
          case List(thisParam, outerClassParam, capturedParamParam) =>
            thisParam.name shouldBe "this"
            thisParam.typeFullName shouldBe "foo.Foo.foo.Bar"

            outerClassParam.name shouldBe "outerClass"
            outerClassParam.typeFullName shouldBe "foo.Foo"

            capturedParamParam.name shouldBe "fooParam"
            capturedParamParam.typeFullName shouldBe "int"
          case result => fail(s"Unexpected result ${result}")
        }
      }

      "not have a constructor paramater for the captured param in the inner local class" in {
        cpg.typeDecl.name("Baz").method.nameExact("<init>").parameter.l match {
          case List(thisParam, outerClassParam) =>
            thisParam.name shouldBe "this"
            thisParam.typeFullName shouldBe "foo.Foo.foo.Bar.bar.Baz"

            outerClassParam.name shouldBe "outerClass"
            outerClassParam.typeFullName shouldBe "foo.Foo.foo.Bar"
          case result => fail(s"Unexpected result ${result}")
        }
      }
    }

    "there is a member access to the outer-outer class" should {
      val cpg = code("""
          |package foo;
          |
          |class Foo {
          |  int fooMember;
          |  void foo() {
          |    class Bar {
          |      void bar() {
          |        class Baz {
          |          void baz() {
          |            sink(fooMember);
          |          }
          |        };
          |      }
          |    };
          |  }
          |}
          |""".stripMargin)
      // The member access happens through a double outer class access
      "have the correct double outer class access structure" in {
        cpg.call.name("sink").argument.l match {
          case List(thisArg: Identifier, fooMemberAccess: Call) =>
            thisArg.name shouldBe "this"

            fooMemberAccess.name shouldBe Operators.fieldAccess
            fooMemberAccess.code shouldBe "this.outerClass.outerClass.fooMember"
            fooMemberAccess.typeFullName shouldBe "int"

            fooMemberAccess.argument.l match {
              case List(barMemberAccess: Call, fooMemberIdentifier: FieldIdentifier) =>
                fooMemberIdentifier.canonicalName shouldBe "fooMember"

                barMemberAccess.name shouldBe Operators.fieldAccess
                barMemberAccess.code shouldBe "this.outerClass.outerClass"
                barMemberAccess.typeFullName shouldBe "foo.Foo"

                barMemberAccess.argument.l match {
                  case List(bazMemberAccess: Call, barClassIdentifier: FieldIdentifier) =>
                    barClassIdentifier.canonicalName shouldBe "outerClass"

                    bazMemberAccess.name shouldBe Operators.fieldAccess
                    bazMemberAccess.code shouldBe "this.outerClass"
                    bazMemberAccess.typeFullName shouldBe "foo.Foo.foo.Bar"

                    bazMemberAccess.argument.l match {
                      case List(thisIdentifier: Identifier, bazClassIdentifier: FieldIdentifier) =>
                        thisIdentifier.name shouldBe "this"
                        thisIdentifier.refOut.l shouldBe cpg.method.name("baz").parameter.index(0).l
                        thisIdentifier.typeFullName shouldBe "foo.Foo.foo.Bar.bar.Baz"

                        bazClassIdentifier.canonicalName shouldBe "outerClass"
                      case result => fail(s"Unexpected result ${result}")
                    }
                  case result => fail(s"Unexpected result ${result}")
                }
              case result => fail(s"Unexpected result ${result}")
            }
          case result => fail(s"Unexpected result ${result}")
        }
      }
    }

    "there is a call to a method in the outer-outer class" should {
      val cpg = code("""
          |package foo;
          |
          |class Foo {
          |  void outerCall() {}
          |  void foo() {
          |    class Bar {
          |      void bar() {
          |        class Baz {
          |          void baz() {
          |            outerCall();
          |          }
          |        };
          |      }
          |    };
          |  }
          |}
        """.stripMargin)

      "have the correct receiver structure" in {
        cpg.call.name("outerCall").l match {
          case List(outerCallCall: Call) =>
            outerCallCall.methodFullName shouldBe "foo.Foo.outerCall:void()"

            outerCallCall.receiver.l match {
              case List(barMemberAccess: Call) =>
                barMemberAccess.name shouldBe Operators.fieldAccess
                barMemberAccess.code shouldBe "this.outerClass.outerClass"
                barMemberAccess.typeFullName shouldBe "foo.Foo"

                barMemberAccess.argument.l match {
                  case List(bazMemberAccess: Call, barClassIdentifier: FieldIdentifier) =>
                    barClassIdentifier.canonicalName shouldBe "outerClass"

                    bazMemberAccess.name shouldBe Operators.fieldAccess
                    bazMemberAccess.code shouldBe "this.outerClass"
                    bazMemberAccess.typeFullName shouldBe "foo.Foo.foo.Bar"

                    bazMemberAccess.argument.l match {
                      case List(thisIdentifier: Identifier, bazClassIdentifier: FieldIdentifier) =>
                        thisIdentifier.name shouldBe "this"
                        thisIdentifier.refOut.l shouldBe cpg.method.name("baz").parameter.index(0).l
                        thisIdentifier.typeFullName shouldBe "foo.Foo.foo.Bar.bar.Baz"

                        bazClassIdentifier.canonicalName shouldBe "outerClass"
                      case result => fail(s"Unexpected result ${result}")
                    }
                  case result => fail(s"Unexpected result ${result}")
                }
              case result => fail(s"Unexpected result ${result}")
            }
          case result => fail(s"Unexpected result ${result}")
        }
      }
    }
  }

  "local classes in a nested static context" should {
    val cpg = code("""
        |public class Test {
        |    int testMember = 1;
        |
        |    void test(int testParam) {
        |        int testLocal = 2;
        |
        |        class Foo {
        |            int fooMember = 4;
        |
        |            static void foo(int fooParam) {
        |                int fooLocal = 8;
        |
        |                class Bar {
        |                    int barMember = 16;
        |
        |                    void bar(int barParam) {
        |                        int barLocal = 32;
        |
        |                        class Baz {
        |                            void baz() {
        |                                // testMember, testParam, testLocal, fooMember not captured
        |                                sink(fooParam, fooLocal, barMember, barParam, barLocal);
        |                            }
        |                        }
        |                    }
        |                }
        |            }
        |
        |            void fooCaptures() {
        |                 sink(testMember, testParam, testLocal);
        |            }
        |        }
        |    }
        |}
        """.stripMargin)

    "have the correct parameters for the outermost local class constructor" in {
      cpg.typeDecl.name("Foo").method.nameExact("<init>").parameter.l match {
        case List(thisParam, outerClassParam, capturedLocalParam, capturedParamParam) =>
          thisParam.name shouldBe "this"
          thisParam.typeFullName shouldBe "Test.test.Foo"

          outerClassParam.name shouldBe "outerClass"
          outerClassParam.typeFullName shouldBe "Test"

          capturedLocalParam.name shouldBe "testLocal"
          capturedLocalParam.typeFullName shouldBe "int"

          capturedParamParam.name shouldBe "testParam"
          capturedParamParam.typeFullName shouldBe "int"
        case result => fail(s"Unexpected result ${result}")
      }
    }

    "have the correct parameters for the outermost static local class constructor" in {
      cpg.typeDecl.name("Bar").method.nameExact("<init>").parameter.l match {
        case List(thisParam, capturedLocalParam, capturedParamParam) =>
          thisParam.name shouldBe "this"
          thisParam.typeFullName shouldBe "Test.test.Foo.foo.Bar"

          capturedLocalParam.name shouldBe "fooLocal"
          capturedLocalParam.typeFullName shouldBe "int"

          capturedParamParam.name shouldBe "fooParam"
          capturedParamParam.typeFullName shouldBe "int"
        case result => fail(s"Unexpected result ${result}")
      }
    }

    "have the correct parameters for the innermost local class constructor" in {
      cpg.typeDecl.name("Baz").method.nameExact("<init>").parameter.l match {
        case List(thisParam, outerClassParam, capturedLocalParam, capturedParamParam) =>
          thisParam.name shouldBe "this"
          thisParam.typeFullName shouldBe "Test.test.Foo.foo.Bar.bar.Baz"

          outerClassParam.name shouldBe "outerClass"
          outerClassParam.typeFullName shouldBe "Test.test.Foo.foo.Bar"

          capturedLocalParam.name shouldBe "barLocal"
          capturedLocalParam.typeFullName shouldBe "int"

          capturedParamParam.name shouldBe "barParam"
          capturedParamParam.typeFullName shouldBe "int"
        case result => fail(s"Unexpected result ${result}")
      }
    }

    "have the correct members for the outermost local class" in {
      cpg.typeDecl.name("Foo").member.l match {
        case List(outerClassMember, capturedLocalMember, capturedParamMember, fooMember) =>
          outerClassMember.name shouldBe "outerClass"
          outerClassMember.typeFullName shouldBe "Test"

          capturedLocalMember.name shouldBe "testLocal"
          capturedLocalMember.typeFullName shouldBe "int"

          capturedParamMember.name shouldBe "testParam"
          capturedParamMember.typeFullName shouldBe "int"

          fooMember.name shouldBe "fooMember"
          fooMember.typeFullName shouldBe "int"
        case result => fail(s"Unexpected result ${result.map(_.code)}")
      }
    }

    "have the correct members for the outermost static local class" in {
      cpg.typeDecl.name("Bar").member.l match {
        case List(capturedLocalMember, capturedParamMember, barMember) =>
          capturedLocalMember.name shouldBe "fooLocal"
          capturedLocalMember.typeFullName shouldBe "int"

          capturedParamMember.name shouldBe "fooParam"
          capturedParamMember.typeFullName shouldBe "int"

          barMember.name shouldBe "barMember"
          barMember.typeFullName shouldBe "int"
        case result => fail(s"Unexpected result ${result.map(_.name)}")
      }
    }

    "have the correct members for the innermost local class" in {
      cpg.typeDecl.name("Baz").member.l match {
        case List(outerClassMember, capturedLocalMember, capturedParamMember) =>
          outerClassMember.name shouldBe "outerClass"
          outerClassMember.typeFullName shouldBe "Test.test.Foo.foo.Bar"

          capturedLocalMember.name shouldBe "barLocal"
          capturedLocalMember.typeFullName shouldBe "int"

          capturedParamMember.name shouldBe "barParam"
          capturedParamMember.typeFullName shouldBe "int"
        case result => fail(s"Unexpected result ${result}")
      }
    }
  }
}
