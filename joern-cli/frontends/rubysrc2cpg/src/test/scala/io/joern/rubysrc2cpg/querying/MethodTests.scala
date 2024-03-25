package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.Return
import io.shiftleft.semanticcpg.language.*

class MethodTests extends RubyCode2CpgFixture {

  "`def f(x) = 1` is represented by a METHOD node" in {
    val cpg = code("""
                     |def f(x) = 1
                     |""".stripMargin)

    val List(f) = cpg.method.name("f").l

    f.fullName shouldBe "Test0.rb:<global>::program:f"
    f.isExternal shouldBe false
    f.lineNumber shouldBe Some(2)
    f.numberOfLines shouldBe 1

    val List(x) = f.parameter.name("x").l
    x.index shouldBe 0
    x.isVariadic shouldBe false
    x.lineNumber shouldBe Some(2)
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
    f.parameter.size shouldBe 0
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
    f.parameter.size shouldBe 0

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
    f.parameter.size shouldBe 1

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
                thisParam.code shouldBe "this"
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
                thisParam.code shouldBe "this"
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
      inside(cpg.method("foo").parameter.l) {
        case xs :: Nil =>
          xs.name shouldBe "xs"
          xs.code shouldBe "*xs"
          xs.isVariadic shouldBe true
          xs.typeFullName shouldBe "__builtin.Array"
        case xs => fail(s"Expected `foo` to have one parameter, got [${xs.code.mkString(", ")}]")
      }
    }

    "be interpreted as a hash type parameter if two stars given" in {
      inside(cpg.method("bar").parameter.l) {
        case ys :: Nil =>
          ys.name shouldBe "ys"
          ys.code shouldBe "**ys"
          ys.isVariadic shouldBe true
          ys.typeFullName shouldBe "__builtin.Hash"
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
          inside(foo.method.nameNot(Defines.ConstructorMethodName, Defines.StaticInitMethodName).l) {
            case xeq :: x :: bar :: Nil =>
              xeq.name shouldBe "x="
              x.name shouldBe "x"
              bar.name shouldBe "bar="

              xeq.parameter.name.l shouldBe bar.parameter.name.l
              // bar forwards parameters to a call to the aliased method
              inside(bar.call.name("x=").l) {
                case barCall :: Nil =>
                  barCall.argument.isIdentifier.name.head shouldBe "z"
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

    "be represented by a a METHOD node" in {
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

}
