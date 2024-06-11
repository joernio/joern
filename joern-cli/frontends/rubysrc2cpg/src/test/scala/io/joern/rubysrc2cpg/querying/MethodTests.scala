package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.passes.Defines as RDefines
import io.joern.rubysrc2cpg.passes.GlobalTypes.kernelPrefix
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, NodeTypes, Operators}
import io.shiftleft.semanticcpg.language.*

class MethodTests extends RubyCode2CpgFixture {

  "`def f(x) = 1`" should {
    val cpg = code("""
        |def f(x) = 1
        |""".stripMargin)

    "be represented by a METHOD node" in {
      val List(f) = cpg.method.name("f").l

      f.fullName shouldBe "Test0.rb:<global>::program:f"
      f.isExternal shouldBe false
      f.lineNumber shouldBe Some(2)
      f.numberOfLines shouldBe 1

      val List(x) = f.parameter.name("x").l
      x.index shouldBe 1
      x.isVariadic shouldBe false
      x.lineNumber shouldBe Some(2)
    }

    "have a corresponding bound type" in {
      val List(fType) = cpg.typeDecl("f").l
      fType.fullName shouldBe "Test0.rb:<global>::program:f"
      fType.code shouldBe "def f(x) = 1"
      fType.astParentFullName shouldBe "Test0.rb:<global>::program:f"
      fType.astParentType shouldBe NodeTypes.METHOD
      val List(fMethod) = fType.iterator.boundMethod.l
      fType.fullName shouldBe "Test0.rb:<global>::program:f"
    }

    "create a 'fake' method for the file" in {
      val List(m) = cpg.method.nameExact(RDefines.Program).l
      m.fullName shouldBe "Test0.rb:<global>::program"
      m.isModule.nonEmpty shouldBe true

      val List(t) = cpg.typeDecl.nameExact(RDefines.Program).l
      m.fullName shouldBe "Test0.rb:<global>::program"
      m.isModule.nonEmpty shouldBe true
      t.methodBinding.methodFullName.toSet should contain(m.fullName)
    }
  }

  "`def f ... return 1 ... end` is represented by a METHOD node" in {
    val cpg = code("""
                     |def f
                     | return 1
                     |end
                     |""".stripMargin)

    val List(f) = cpg.method.name("f").l

    f.fullName shouldBe "Test0.rb:<global>::program:f"
    f.isExternal shouldBe false
    f.lineNumber shouldBe Some(2)
    f.numberOfLines shouldBe 3
    f.parameter.size shouldBe 1
  }

  "`def f = puts 'hi'` is represented by a METHOD node returning `puts 'hi'`" in {
    val cpg = code("""
        |def f = puts 'hi'
        |""".stripMargin)

    val List(f) = cpg.method.name("f").l

    f.fullName shouldBe "Test0.rb:<global>::program:f"
    f.isExternal shouldBe false
    f.lineNumber shouldBe Some(2)
    f.numberOfLines shouldBe 1
    f.parameter.size shouldBe 1

    val List(r: Return) = f.methodReturn.cfgIn.l: @unchecked
    r.code shouldBe "puts 'hi'"
    r.lineNumber shouldBe Some(2)
  }

  "`def f(x) = x.class` is represented by a METHOD node returning `x.class`" in {
    val cpg = code("""
        |def f(x) = x.class
        |""".stripMargin)

    val List(f) = cpg.method.name("f").l

    f.fullName shouldBe "Test0.rb:<global>::program:f"
    f.isExternal shouldBe false
    f.lineNumber shouldBe Some(2)
    f.numberOfLines shouldBe 1
    f.parameter.size shouldBe 2

    val List(r: Return) = f.methodReturn.cfgIn.l: @unchecked
    r.code shouldBe "x.class"
    r.lineNumber shouldBe Some(2)
  }

  "def(a, x=\"default\")...end" should {
    val cpg = code("""
        |def foo(a, x="default")
        | puts(x)
        |end
        |""".stripMargin)

    "generated IF control structure for only the default value" in {
      inside(cpg.method.name("foo").l) {
        case fooFunc :: Nil =>
          inside(fooFunc.controlStructure.isIf.l) {
            case defaultIf :: Nil =>
              inside(defaultIf.condition.isCall.l) {
                case defaultIfCond :: Nil =>
                  defaultIfCond.code shouldBe "!defined?(x)"
                  defaultIfCond.methodFullName shouldBe Operators.logicalNot
                case _ => fail("Only expected on call for condition")
              }

              inside(defaultIf.whenTrue.isBlock.astChildren.isCall.l) {
                case thenCall :: Nil =>
                  thenCall.code shouldBe "x=\"default\""
                  thenCall.methodFullName shouldBe Operators.assignment
                case _ => fail("Only one call expected in true block for default parameter")
              }

            case _ => fail("Expected single if statement for default parameter")
          }
        case _ => fail("Expected one method")
      }
    }
  }

  "def(a, x=\"default\", y=123)...end" should {
    val cpg = code("""
                     |def foo(a, x="default", y=123)
                     | puts(x)
                     |end
                     |""".stripMargin)

    "generated IF control structure for only the default value" in {
      inside(cpg.method.name("foo").l) {
        case fooFunc :: Nil =>
          inside(fooFunc.controlStructure.isIf.l) {
            case defaultIfForX :: defaultIfForY :: Nil =>
              inside(defaultIfForX.condition.isCall.l) {
                case defaultIfCond :: Nil =>
                  defaultIfCond.code shouldBe "!defined?(x)"
                  defaultIfCond.methodFullName shouldBe Operators.logicalNot
                case _ => fail("Only expected one call for condition")
              }

              inside(defaultIfForX.whenTrue.isBlock.astChildren.isCall.l) {
                case thenCall :: Nil =>
                  thenCall.code shouldBe "x=\"default\""
                  thenCall.methodFullName shouldBe Operators.assignment
                case _ => fail("Only one call expected in true block for default parameter")
              }

              inside(defaultIfForY.condition.isCall.l) {
                case defaultIfCond :: Nil =>
                  defaultIfCond.code shouldBe "!defined?(y)"
                  defaultIfCond.methodFullName shouldBe Operators.logicalNot
                case _ => fail("Only expected one call for condition")
              }

              inside(defaultIfForY.whenTrue.isBlock.astChildren.isCall.l) {
                case thenCall :: Nil =>
                  thenCall.code shouldBe "y=123"
                  thenCall.methodFullName shouldBe Operators.assignment
                case _ => fail("Only one call expected in true block for default parameter")
              }

            case _ => fail("Expected two if statements for two default parameters")
          }
        case _ => fail("Expected one method")
      }
    }
  }

  "`def self.f(x) ... end` is represented by a METHOD inside the TYPE_DECL node" in {
    val cpg = code("""
                     |class C
                     | def self.f(x)
                     |  x + 1
                     | end
                     |end
                     |""".stripMargin)

    inside(cpg.typeDecl.name("C").l) {
      case classC :: Nil =>
        inside(classC.method.name("f").l) {
          case funcF :: Nil =>
            inside(funcF.parameter.l) {
              case thisParam :: xParam :: Nil =>
                thisParam.code shouldBe RDefines.Self
                thisParam.typeFullName shouldBe "Test0.rb:<global>::program.C"
                thisParam.index shouldBe 0
                thisParam.isVariadic shouldBe false

                xParam.code shouldBe "x"
                xParam.index shouldBe 1
                xParam.isVariadic shouldBe false

              case _ => fail("Expected two parameters")
            }
          case _ => fail("Expected one method")
        }
      case _ => fail("Expected one class definition")
    }
  }

  "`def class << self << f(x) ... end` is represented by a METHOD inside the TYPE_DECL node" in {
    val cpg = code("""
                     |class C
                     | class << self
                     |  def f(x)
                     |   x + 1
                     |  end
                     | end
                     |end
                     |""".stripMargin)

    inside(cpg.typeDecl.name("C").l) {
      case classC :: Nil =>
        inside(classC.method.name("f").l) {
          case funcF :: Nil =>
            inside(funcF.parameter.l) {
              case thisParam :: xParam :: Nil =>
                thisParam.code shouldBe RDefines.Self
                thisParam.typeFullName shouldBe "Test0.rb:<global>::program.C"
                thisParam.index shouldBe 0
                thisParam.isVariadic shouldBe false

                xParam.code shouldBe "x"
                xParam.index shouldBe 1
                xParam.isVariadic shouldBe false
              case _ => fail("Expected two parameters")
            }
          case _ => fail("Expected one method")
        }
      case _ => fail("Expected one class definition")
    }
  }

  "array/hash (variadic) parameters" should {

    val cpg = code("""
        |def foo(*xs)
        |end
        |
        |def bar(**ys)
        |end
        |""".stripMargin)

    "be interpreted as an array type parameter if a single star given" in {
      inside(cpg.method("foo").parameter.indexGt(0).l) {
        case xs :: Nil =>
          xs.name shouldBe "xs"
          xs.code shouldBe "*xs"
          xs.isVariadic shouldBe true
          xs.typeFullName shouldBe s"$kernelPrefix.Array"
        case xs => fail(s"Expected `foo` to have one parameter, got [${xs.code.mkString(", ")}]")
      }
    }

    "be interpreted as a hash type parameter if two stars given" in {
      inside(cpg.method("bar").parameter.indexGt(0).l) {
        case ys :: Nil =>
          ys.name shouldBe "ys"
          ys.code shouldBe "**ys"
          ys.isVariadic shouldBe true
          ys.typeFullName shouldBe s"$kernelPrefix.Hash"
        case xs => fail(s"Expected `foo` to have one parameter, got [${xs.code.mkString(", ")}]")
      }
    }

  }

  "aliased methods" should {

    val cpg = code("""
        |class Foo
        |    @x = 0
        |    def x=(z)
        |        @x = z
        |    end
        |
        |    def x
        |        @x
        |    end
        |
        |    alias x= bar=
        |end
        |
        |foo = Foo.new
        |
        |foo.bar= 1
        |
        |puts foo.x # => 1
        |""".stripMargin)

    "create a method under `Foo` for both `x=`, `x`, and `bar=`, where `bar=` forwards parameters to a call to `x=`" in {
      inside(cpg.typeDecl("Foo").l) {
        case foo :: Nil =>
          inside(foo.method.nameNot(RDefines.Initialize).l) {
            case xeq :: x :: bar :: Nil =>
              xeq.name shouldBe "x="
              x.name shouldBe "x"
              bar.name shouldBe "bar="

              xeq.parameter.name.l shouldBe bar.parameter.name.l
              // bar forwards parameters to a call to the aliased method
              inside(bar.call.name("x=").l) {
                case barCall :: Nil =>
                  inside(barCall.argument.l) {
                    case _ :: (z: Identifier) :: Nil =>
                      z.name shouldBe "z"
                      z.argumentIndex shouldBe 1
                    case xs =>
                      fail(s"Expected a two arguments for the call `x=`,  instead got [${xs.code.mkString(",")}]")
                  }
                  barCall.code shouldBe "x=(z)"
                case xs => fail(s"Expected a single call to `bar=`,  instead got [${xs.code.mkString(",")}]")
              }
            case xs => fail(s"Expected a three virtual methods under `Foo`, instead got [${xs.code.mkString(",")}]")
          }
        case xs => fail(s"Expected a single type decl for `Foo`, instead got [${xs.code.mkString(",")}]")
      }
    }
  }

  "Singleton Methods for module scope" should {
    val cpg = code("""
        |module F
        | def F.bar(x)
        |   x
        | end
        |end
        |
        |def F.baz(x)
        |  x
        |end
        |
        |F::bar(p)
        |""".stripMargin)

    "exist under the module TYPE_DECL" in {
      inside(cpg.typeDecl.name("F").method.l) {
        case bar :: baz :: Nil =>
          inside(bar.parameter.l) {
            case thisParam :: xParam :: Nil =>
              thisParam.name shouldBe "this"
              thisParam.code shouldBe "F"
              thisParam.typeFullName shouldBe "Test0.rb:<global>::program.F"

              xParam.name shouldBe "x"
            case xs => fail(s"Expected two parameters, got ${xs.name.mkString(", ")}")
          }

          inside(baz.parameter.l) {
            case thisParam :: xParam :: Nil =>
              thisParam.name shouldBe "this"
              thisParam.code shouldBe "F"
              thisParam.typeFullName shouldBe "Test0.rb:<global>::program.F"

              xParam.name shouldBe "x"
              xParam.code shouldBe "x"
            case xs => fail(s"Expected two parameters, got ${xs.name.mkString(", ")}")
          }
        case xs => fail(s"Expected bar and baz to exist under F, instead got ${xs.code.mkString(", ")}")
      }
    }

    "have bindings to the singleton module TYPE_DECL" in {
      // Note: we cannot bind baz as this is a dynamic assignment to `F` which is trickier to determine
      cpg.typeDecl.name("F<class>").methodBinding.methodFullName.l shouldBe List("Test0.rb:<global>::program.F:bar")
    }

    "baz should not exist in the :program block" in {
      inside(cpg.method.name(":program").l) {
        case prog :: Nil =>
          inside(prog.block.astChildren.isMethod.name("baz").l) {
            case Nil => // passing case
            case _   => fail("Baz should not exist under program method block")
          }
        case _ => fail("Expected one Method for :program")
      }
    }
  }

  "A Boolean method" should {
    val cpg = code("""
        |def exists?
        |  true
        |end
        |""".stripMargin)

    "be represented by a METHOD node" in {
      inside(cpg.method.name("exists\\?").l) {
        case existsMethod :: Nil =>
          existsMethod.fullName shouldBe "Test0.rb:<global>::program:exists?"
          existsMethod.isExternal shouldBe false

          inside(existsMethod.methodReturn.cfgIn.l) {
            case existsMethodReturn :: Nil =>
              existsMethodReturn.code shouldBe "true"
            case _ => fail("Expected return for method")
          }

        case xs => fail(s"Expected one method, found ${xs.name.mkString(", ")}")
      }
    }
  }

  "Explicit ReturnExpression as last statement" should {
    val cpg = code("""
        |  def foo
        |    return true if 1 < 2
        |  end
        |""".stripMargin)

    "Represented by Return Node" in {
      inside(cpg.method.name("foo").l) {
        case fooMethod :: Nil =>
          inside(fooMethod.methodReturn.toReturn.l) {
            case returnTrue :: returnNil :: Nil =>
              returnTrue.code shouldBe "return true"

              val List(trueVal: Literal) = returnTrue.astChildren.isLiteral.l: @unchecked
              trueVal.code shouldBe "true"

              returnNil.code shouldBe "return nil"

              val List(nilVal: Literal) = returnNil.astChildren.isLiteral.l: @unchecked
              nilVal.code shouldBe "nil"
            case xs => fail(s"Expected two returns, got ${xs.code.mkString(", ")} instead")
          }
        case xs => fail(s"Expected one method for foo, got ${xs.name.mkString(", ")} instead")
      }
    }
  }

  "break unless statement" should {
    val cpg = code("""
        |  def foo
        |    bar do
        |      break unless 1 < 2
        |    end
        |  end
        |""".stripMargin)

    "generate control structure for break" in {
      inside(cpg.method.name("foo").l) {
        case fooMethod :: Nil =>
          inside(fooMethod.astChildren.isMethod.name("<lambda>0").l) {
            case loopMethod :: Nil =>
              inside(loopMethod.block.astChildren.isControlStructure.l) {
                case ifStruct :: Nil =>
                  inside(ifStruct.astChildren.isBlock.l) {
                    case breakBlock :: nilBlock :: Nil =>
                      inside(breakBlock.astChildren.isControlStructure.l) {
                        case breakStruct :: Nil =>
                          breakStruct.code shouldBe "break"
                        case xs =>
                          fail(s"Expected on control structure for break, got ${xs.code.mkString(", ")} instead")
                      }
                    case xs => fail(s"Expected block for break and nil, got ${xs.code.mkString(", ")} instead")
                  }
                case xs => fail(s"Expected one control structure for if, got ${xs.code.mkString(", ")} instead")
              }

              inside(loopMethod.methodReturn.toReturn.l) {
                case lambdaRet :: Nil =>
                  lambdaRet.code shouldBe "return nil" // break statements cannot be returned, so only false branch should be present which returns nil for UnlessExpression
                case xs => fail(s"Expected one return for lambda function, got ${xs.code.mkString(", ")} instead")
              }
            case xs => fail(s"Expected one lambda method, got ${xs.name.mkString(", ")} instead")
          }
        case xs => fail(s"Expected one method for foo, got ${xs.name.mkString(", ")} instead")
      }
    }

    "generate one return for lambda loop do block" in {
      inside(cpg.method.name("foo").l) {
        case fooMethod :: Nil =>
          inside(fooMethod.astChildren.isMethod.name("<lambda>0").l) {
            case loopMethod :: Nil =>
              inside(loopMethod.methodReturn.toReturn.l) {
                case lambdaRet :: Nil =>
                  lambdaRet.code shouldBe "return nil" // break statements cannot be returned, so only false branch should be present which returns nil for UnlessExpression
                case xs => fail(s"Expected one return for lambda function, got ${xs.code.mkString(", ")} instead")
              }
            case xs => fail(s"Expected one lambda method, got ${xs.name.mkString(", ")} instead")
          }
        case xs => fail(s"Expected one method for foo, got ${xs.name.mkString(", ")} instead")
      }
    }
  }

  "RescueExpression with `yield` as Statement body" should {
    val cpg = code("""
        |  def foo
        |    1
        |    yield
        |  ensure
        |    2
        |  end
        |""".stripMargin)

    "Should be represented as a TRY structure" in {
      inside(cpg.method.name("foo").tryBlock.l) {
        case tryBlock :: Nil =>
          tryBlock.controlStructureType shouldBe ControlStructureTypes.TRY

          inside(tryBlock.astChildren.l) {
            case body :: ensureBody :: Nil =>
              body.ast.isLiteral.code.l shouldBe List("1")
              body.order shouldBe 1

              ensureBody.ast.isLiteral.code.l shouldBe List("2")
              ensureBody.order shouldBe 3
            case xs => fail(s"Expected body and ensureBody, got ${xs.code.mkString(", ")} instead")
          }
        case xs => fail(s"Expected one method, found ${xs.method.name.mkString(", ")} instead")
      }
    }
  }

  "Chained method calls" should {
    val cpg = code("""
        |class Foo
        | def authenticate(email, password)
        |   auth = nil
        |   if a == Digest::MD5.hexdigest(password)
        |     auth = a
        |   end
        | end
        |end
        |""".stripMargin)

    "have chained calls" in {
      inside(cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).condition.headOption) {
        case Some(ifCond: Call) =>
          inside(ifCond.argument.l) {
            case (leftArg: Identifier) :: (rightArg: Call) :: Nil =>
              rightArg.name shouldBe "hexdigest"
              rightArg.code shouldBe "Digest::MD5.hexdigest(password)"

              inside(rightArg.argument.l) {
                case (md5: Call) :: (passwordArg: Identifier) :: Nil =>
                  md5.name shouldBe "MD5"
                  inside(md5.argument.l) {
                    case (digest: Identifier) :: Nil =>
                      digest.name shouldBe "Digest"
                    case xs => fail(s"Expected 1 argument, got ${xs.code.mkString(", ")} instead")
                  }
                case xs => fail(s"Expected identifier and call, got ${xs.code.mkString(", ")} instead")
              }
            case xs => fail(s"Expected 3 arguments, got ${xs.code.mkString(", ")} instead")
          }
        case None => fail("Expected if-condition")
      }
    }
  }

  "METHOD_REF and TYPE_REF nodes" should {
    val cpg = code(
      """
        |module A
        | def foo
        | end
        |end
        |
        |class B
        |end
        |
        |def c
        |end
        |""".stripMargin,
      "t1.rb"
    )
      .moreCode(
        """
          |require 't1'
          |class D
          |end
          |
          |def e
          |end
          |""".stripMargin,
        "t2.rb"
      )

    "be directly under :program" in {
      inside(cpg.method.name(RDefines.Program).filename("t1.rb").assignment.l) {
        case moduleAssignment :: classAssignment :: methodAssignment :: Nil =>
          moduleAssignment.code shouldBe "self.A = class A (...)"
          classAssignment.code shouldBe "self.B = class B (...)"
          methodAssignment.code shouldBe "self.c = def c (...)"

          inside(moduleAssignment.argument.l) {
            case (lhs: Call) :: (rhs: TypeRef) :: Nil =>
              lhs.code shouldBe "self.A"
              lhs.name shouldBe Operators.fieldAccess
              rhs.typeFullName shouldBe "t1.rb:<global>::program.A"
            case xs => fail(s"Expected lhs and rhs, instead got ${xs.code.mkString(",")}")
          }

          inside(classAssignment.argument.l) {
            case (lhs: Call) :: (rhs: TypeRef) :: Nil =>
              lhs.code shouldBe "self.B"
              lhs.name shouldBe Operators.fieldAccess
              rhs.typeFullName shouldBe "t1.rb:<global>::program.B"
            case xs => fail(s"Expected lhs and rhs, instead got ${xs.code.mkString(",")}")
          }

          inside(methodAssignment.argument.l) {
            case (lhs: Call) :: (rhs: MethodRef) :: Nil =>
              lhs.code shouldBe "self.c"
              lhs.name shouldBe Operators.fieldAccess
              rhs.methodFullName shouldBe "t1.rb:<global>::program:c"
              rhs.typeFullName shouldBe "t1.rb:<global>::program:c"
            case xs => fail(s"Expected lhs and rhs, instead got ${xs.code.mkString(",")}")
          }

        case xs => fail(s"Expected three assignments, got [${xs.code.mkString(",")}]")
      }
    }

    "not be present in other files" in {
      inside(cpg.method.name(RDefines.Program).filename("t2.rb").assignment.l) {
        case classAssignment :: methodAssignment :: Nil =>
          classAssignment.code shouldBe "self.D = class D (...)"
          methodAssignment.code shouldBe "self.e = def e (...)"

          inside(classAssignment.argument.l) {
            case (lhs: Call) :: (rhs: TypeRef) :: Nil =>
              lhs.code shouldBe "self.D"
              lhs.name shouldBe Operators.fieldAccess
              rhs.typeFullName shouldBe "t2.rb:<global>::program.D"
            case xs => fail(s"Expected lhs and rhs, instead got ${xs.code.mkString(",")}")
          }

          inside(methodAssignment.argument.l) {
            case (lhs: Call) :: (rhs: MethodRef) :: Nil =>
              lhs.code shouldBe "self.e"
              lhs.name shouldBe Operators.fieldAccess
              rhs.methodFullName shouldBe "t2.rb:<global>::program:e"
              rhs.typeFullName shouldBe "t2.rb:<global>::program:e"
            case xs => fail(s"Expected lhs and rhs, instead got ${xs.code.mkString(",")}")
          }

        case xs => fail(s"Expected two assignments, got [${xs.code.mkString(",")}]")
      }
    }

    "be placed directly before each entity's definition" in {
      inside(cpg.method.name(RDefines.Program).filename("t1.rb").block.astChildren.l) {
        case (a1: Call) :: (_: TypeDecl) :: (_: TypeDecl) :: (a2: Call) :: (_: TypeDecl) :: (_: TypeDecl) :: (a3: Call) :: (_: Method) :: (_: TypeDecl) :: Nil =>
          a1.code shouldBe "self.A = class A (...)"
          a2.code shouldBe "self.B = class B (...)"
          a3.code shouldBe "self.c = def c (...)"
        case xs => fail(s"Expected assignments to appear before definitions, instead got [$xs]")
      }
    }
  }
}
