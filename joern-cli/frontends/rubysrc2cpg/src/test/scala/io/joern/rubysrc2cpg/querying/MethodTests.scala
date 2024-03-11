package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
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

}
