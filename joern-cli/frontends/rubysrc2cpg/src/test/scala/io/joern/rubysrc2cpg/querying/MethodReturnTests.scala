package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.passes.Defines as RubyDefines
import io.joern.rubysrc2cpg.passes.Defines.{Main, RubyOperators}
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, Operators}
import io.shiftleft.semanticcpg.language.*

class MethodReturnTests extends RubyCode2CpgFixture {

  "implicit RETURN node for `x * x` exists" in {
    val cpg = code("""
                     |def f(x) = x * x
                     |""".stripMargin)

    val List(f)         = cpg.method.name("f").l
    val List(r: Return) = f.methodReturn.cfgIn.l: @unchecked
    r.code shouldBe "x * x"
    r.lineNumber shouldBe Some(2)
  }

  "explicit RETURN node for `x * x` exists" in {
    val cpg = code("""
                     |def f(x)
                     | return x * x
                     |end
                     |""".stripMargin)

    val List(f)         = cpg.method.name("f").l
    val List(r: Return) = f.methodReturn.cfgIn.l: @unchecked
    r.code shouldBe "return x * x"
    r.lineNumber shouldBe Some(3)
  }

  "implicit RETURN node for `x = 1` exists" in {
    val cpg = code("""
                     |def f(x) = x = 1
                     |""".stripMargin)

    // Lowered as `def f(x) = x = 1; return x`
    val List(f)         = cpg.method.name("f").l
    val List(r: Return) = f.methodReturn.cfgIn.l: @unchecked
    r.code shouldBe "x = 1"
    r.lineNumber shouldBe Some(2)
  }

  "implicit RETURN node for `x` exists" in {
    val cpg = code("""
        |def f(x)
        | x
        |end
        |""".stripMargin)

    val List(f)         = cpg.method.name("f").l
    val List(r: Return) = f.methodReturn.cfgIn.l: @unchecked
    r.code shouldBe "x"
    r.lineNumber shouldBe Some(3)
  }

  "implicit RETURN node for `puts x` exists" in {
    val cpg = code("""
        |def f(x)
        | puts x
        |end
        |""".stripMargin)

    val List(f)         = cpg.method.name("f").l
    val List(r: Return) = f.methodReturn.cfgIn.l: @unchecked

    r.code shouldBe "puts x"
    r.lineNumber shouldBe Some(3)

    val List(c: Call) = r.astChildren.isCall.l
    c.methodFullName shouldBe RubyDefines.prefixAsKernelDefined("puts")
    c.lineNumber shouldBe Some(3)
    c.code shouldBe "puts x"
  }

  "implicit RETURN node for `x.class` exists" in {
    val cpg = code("""
        |def f(x)
        | x.class
        |end""".stripMargin)

    val List(f)         = cpg.method.name("f").l
    val List(r: Return) = f.methodReturn.cfgIn.l: @unchecked

    r.code shouldBe "x.class"
    r.lineNumber shouldBe Some(3)
  }

  "implicit RETURN node for `[]` exists" in {
    val cpg = code("""
        |def f
        | []
        |end
        |""".stripMargin)

    val List(f)         = cpg.method.name("f").l
    val List(r: Return) = f.methodReturn.cfgIn.l: @unchecked

    r.code shouldBe "[]"
    r.lineNumber shouldBe Some(3)

    val List(arr: Block) = r.astChildren.isBlock.l
    arr.code shouldBe "[]"
  }

  "implicit RETURN node for index access exists" in {
    val cpg = code("""
                     |def f
                     | l = [1, 2, 3]
                     | l[1]
                     |end
                     |""".stripMargin)

    val List(f)         = cpg.method.name("f").l
    val List(r: Return) = f.methodReturn.cfgIn.l: @unchecked

    r.code shouldBe "l[1]"
    r.lineNumber shouldBe Some(4)

    val List(c: Call) = r.astChildren.isCall.l
    c.methodFullName shouldBe Operators.indexAccess
    val List(arg1, arg2) = c.argument.l
    arg1.argumentIndex shouldBe 1
    arg2.argumentIndex shouldBe 2
  }

  "implicit RETURN node for `{x:0}` exists" in {
    val cpg = code("""
        |def f = {x:0}
        |""".stripMargin)

    val List(f)         = cpg.method.name("f").l
    val List(r: Return) = f.methodReturn.cfgIn.l: @unchecked

    r.code shouldBe "{x:0}"
    r.lineNumber shouldBe Some(2)

    val List(c: Call) = r.astChildren.isBlock.astChildren.assignment.source.isCall.l
    c.methodFullName shouldBe RubyOperators.hashInitializer
  }

  "implicit RETURN node for `def f ... end` exists" in {
    val cpg = code("""
        |def f
        | def g = 0
        |end
        |""".stripMargin)

    // Lowered as `def f; def g = 0; return :g; end`
    val List(f)         = cpg.method.name("f").l
    val List(r: Return) = f.methodReturn.cfgIn.l: @unchecked

    val List(s: Literal) = r.astChildren.isLiteral.l
    s.code shouldBe ":g"
    s.typeFullName shouldBe RubyDefines.prefixAsCoreType("Symbol")
  }

  "explicit RETURN node for `\"\"` exists" in {
    val cpg = code("""
        |def foo
        | return ""
        |end
        |""".stripMargin)

    val List(f)         = cpg.method.name("foo").l
    val List(r: Return) = f.methodReturn.cfgIn.l: @unchecked
    r.code shouldBe "return \"\""
    r.lineNumber shouldBe Some(3)
  }

  "implicit RETURN node for `if-end` expression" in {
    val cpg = code("""
        |def f
        | if true then
        |   20
        | end
        |end""".stripMargin)

    // Lowered as `def f; if true then return 20 else return nil end`

    inside(cpg.method.name("f").l) {
      case f :: Nil =>
        // Check the two return statements
        inside(f.methodReturn.toReturn.l) {
          case return20 :: returnNil :: Nil =>
            return20.code shouldBe "20"
            return20.lineNumber shouldBe Some(4)
            val List(twenty: Literal) = return20.astChildren.l: @unchecked
            twenty.code shouldBe "20"
            twenty.lineNumber shouldBe Some(4)
            twenty.typeFullName shouldBe RubyDefines.prefixAsCoreType("Integer")

            returnNil.code shouldBe "return nil"
            returnNil.lineNumber shouldBe Some(3)
            val List(nil: Literal) = returnNil.astChildren.l: @unchecked
            nil.code shouldBe "nil"
            nil.lineNumber shouldBe Some(3)
            nil.typeFullName shouldBe RubyDefines.prefixAsCoreType("NilClass")
          case xs => fail(s"Expected exactly two return nodes, instead got [${xs.code.mkString(",")}]")
        }
      case xs => fail(s"Expected exactly one method with the name `f`, instead got [${xs.code.mkString(",")}]")
    }
  }

  "implicit RETURN node for `if-else-end` expression" in {
    val cpg = code("""
        |def f
        | if true then
        |   20
        | else
        |   40
        | end
        |end
        |""".stripMargin)

    inside(cpg.method.name("f").l) {
      case f :: Nil =>
        // Check the two return statements
        inside(f.methodReturn.toReturn.l) {
          case return20 :: return40 :: Nil =>
            return20.code shouldBe "20"
            return20.lineNumber shouldBe Some(4)
            val List(twenty: Literal) = return20.astChildren.l: @unchecked
            twenty.code shouldBe "20"
            twenty.lineNumber shouldBe Some(4)
            twenty.typeFullName shouldBe RubyDefines.prefixAsCoreType("Integer")

            return40.code shouldBe "40"
            return40.lineNumber shouldBe Some(6)
            val List(forty: Literal) = return40.astChildren.l: @unchecked
            forty.code shouldBe "40"
            forty.lineNumber shouldBe Some(6)
            forty.typeFullName shouldBe RubyDefines.prefixAsCoreType("Integer")
          case xs => fail(s"Expected exactly two return nodes, instead got [${xs.code.mkString(",")}]")
        }
      case xs => fail(s"Expected exactly one method with the name `f`, instead got [${xs.code.mkString(",")}]")
    }
  }

  "implicit return of nested control flow" in {
    val cpg = code("""
      | def f
      |  if true
      |   if true
      |    1
      |   else
      |    2
      |   end
      |  else
      |   if true
      |    3
      |   else
      |    4
      |   end
      |  end
      | end
      |""".stripMargin)

    inside(cpg.method.name("f").l) {
      case f :: Nil =>
        inside(cpg.methodReturn.toReturn.l) {
          case return1 :: return2 :: return3 :: return4 :: Nil =>
            return1.code shouldBe "1"
            return1.lineNumber shouldBe Some(5)

            return2.code shouldBe "2"
            return2.lineNumber shouldBe Some(7)

            return3.code shouldBe "3"
            return3.lineNumber shouldBe Some(11)

            return4.code shouldBe "4"
            return4.lineNumber shouldBe Some(13)

          case xs => fail(s"Expected 4 returns, instead got [${xs.code.mkString(",")}]")
        }
      case xs => fail(s"Expected exactly one method with the name `f`, instead got [${xs.code.mkString(",")}]")
    }
  }

  "implicit RETURN node for ternary expression" in {
    val cpg = code("""
        |def f(x) = x ? 20 : 40
        |""".stripMargin)

    inside(cpg.method.name("f").l) {
      case f :: Nil =>
        // Check the two return statements
        inside(f.methodReturn.toReturn.l) {
          case return20 :: return40 :: Nil =>
            return20.code shouldBe "20"
            return20.lineNumber shouldBe Some(2)
            val List(twenty: Literal) = return20.astChildren.l: @unchecked
            twenty.code shouldBe "20"
            twenty.lineNumber shouldBe Some(2)
            twenty.typeFullName shouldBe RubyDefines.prefixAsCoreType("Integer")

            return40.code shouldBe "40"
            return40.lineNumber shouldBe Some(2)
            val List(forty: Literal) = return40.astChildren.l: @unchecked
            forty.code shouldBe "40"
            forty.lineNumber shouldBe Some(2)
            forty.typeFullName shouldBe RubyDefines.prefixAsCoreType("Integer")
          case xs => fail(s"Expected exactly two return nodes, instead got [${xs.code.mkString(",")}]")
        }
      case xs => fail(s"Expected exactly one method with the name `f`, instead got [${xs.code.mkString(",")}]")
    }
  }

  "implicit RETURN node for MEMBER CALL" in {
    val cpg = code("""
                     |class F
                     |  def self.x(y)
                     |   puts(y)
                     |  end
                     |end
                     |
                     |def j()
                     |  f = F.new
                     |  f.x(1)
                     |end
                     |""".stripMargin)

    inside(cpg.method.name("j").l) {
      case jMethod :: Nil =>
        inside(jMethod.methodReturn.toReturn.l) {
          case retMemAccess :: Nil =>
            retMemAccess.code shouldBe "f.x(1)"

            val List(call: Call) = retMemAccess.astChildren.l: @unchecked
            call.name shouldBe "x"
          case xs => fail(s"Expected exactly one return nodes, instead got [${xs.code.mkString(",")}]")
        }
      case _ => fail("Only one method expected")
    }
  }

  "implicit RETURN node for super call" in {
    val cpg = code("""
                     |def j
                     |  super(only: ["a"])
                     |end
                     |""".stripMargin)

    inside(cpg.method.name("j").l) {
      case jMethod :: Nil =>
        inside(jMethod.methodReturn.toReturn.l) {
          case retAssoc :: Nil =>
            retAssoc.code shouldBe "super(only: [\"a\"])"

            val List(call: Call) = retAssoc.astChildren.l: @unchecked
            call.name shouldBe "super"
            call.code shouldBe "super(only: [\"a\"])"
          case xs => fail(s"Expected exactly one return nodes, instead got [${xs.code.mkString(",")}]")
        }
      case _ => fail("Only one method expected")
    }
  }

  "implicit RETURN node for RubyCallWithBlock" should {
    val cpg = code("""
                     | def foo &block
                     |  puts block.call
                     | end
                     |
                     | def bar
                     |  foo do
                     |   "hello"
                     |  end
                     | end
                     |""".stripMargin)

    "Create closureMethod and return structures" in {
      inside(cpg.method.name("bar").l) {
        case bar :: Nil =>
          inside(bar.astChildren.collectAll[Method].l) {
            case closureMethod :: Nil =>
              closureMethod.name shouldBe "<lambda>0"
              closureMethod.fullName shouldBe s"Test0.rb:$Main.bar.<lambda>0"
            case xs => fail(s"Expected closure method, but found ${xs.code.mkString(", ")} instead")
          }

          inside(bar.methodReturn.toReturn.l) {
            case barReturn :: Nil =>
              inside(barReturn.astChildren.l) {
                case (returnCall: Call) :: Nil =>
                  returnCall.code should startWith("foo")

                  returnCall.name shouldBe "foo"

                  val List(_, arg: TypeRef) = returnCall.argument.l: @unchecked
                  arg.typeFullName shouldBe s"Test0.rb:$Main.bar.<lambda>0&Proc"
                case xs => fail(s"Expected one call for return, but found ${xs.code.mkString(", ")} instead")
              }

            case xs => fail(s"Expected one return, but found ${xs.code.mkString(", ")} instead")
          }
        case xs => fail(s"Expected one method, but found ${xs.code.mkString(", ")} instead")
      }
    }

    "have no parameters in the closure declaration" in {
      inside(cpg.method("<lambda>0").parameter.indexGt(0).l) {
        case Nil => // pass
        case xs  => fail(s"Expected the closure to have no parameters, instead got [${xs.code.mkString(", ")}]")
      }
    }

    "have the return node under the closure (returning the literal)" in {
      inside(cpg.method("<lambda>0").block.astChildren.l) {
        case ret :: Nil =>
          ret.code shouldBe "\"hello\""
        case xs => fail(s"Expected the closure to have a single call, instead got [${xs.code.mkString(", ")}]")
      }
    }
  }

  "implicit return of a heredoc should return a literal" in {
    val cpg = code("""
          |def foo
          | <<-EOM
          |   puts "hello"
          | EOM
          |end
          |""".stripMargin)

    inside(cpg.method.nameExact("foo").methodReturn.toReturn.astChildren.l) {
      case (heredoc: Literal) :: Nil =>
        heredoc.typeFullName shouldBe RubyDefines.prefixAsCoreType("String")
        heredoc.code should startWith("   puts \"hello\"")
      case xs => fail(s"Expected a single literal node, instead got [${xs.code.mkString(", ")}]")
    }
  }

  "a return in an expression position without arguments should generate a return node with no children" in {
    val cpg = code("""
        |def foo
        | return unless baz()
        | bar()
        |end
        |""".stripMargin)

    inside(cpg.method.nameExact("foo").ast.isReturn.l) {
      case ret1 :: ret2 :: ret3 :: Nil =>
        ret1.code shouldBe "return nil"
        ret1.astChildren.size shouldBe 1
        ret1.astParent.astParent.code shouldBe "return unless baz()"

        ret2.code shouldBe "return"
        ret2.astChildren.size shouldBe 0
        ret2.astParent.astParent.code shouldBe "return unless baz()"

        ret3.code shouldBe "bar()"
      case xs => fail(s"Expected 3 return nodes, got ${xs.size}")
    }
  }

  "a return with multiple values" in {
    val cpg = code("""
        |def foo
        | return 1, :z => 1
        |end
        |""".stripMargin)

    inside(cpg.method.nameExact("foo").ast.isReturn.headOption) {
      case Some(ret) =>
        val List(oneLiteral: Literal, zAssoc: Call) = ret.astChildren.l: @unchecked
        oneLiteral.code shouldBe "1"
        zAssoc.code shouldBe ":z => 1"
        zAssoc.methodFullName shouldBe RubyOperators.association

        inside(zAssoc.argument.l) {
          case (key: Literal) :: (value: Literal) :: Nil =>
            key.code shouldBe ":z"
            value.code shouldBe "1"
          case xs => fail(s"Expected two args, got ${xs.code.mkString(",")}")
        }
      case None => fail(s"Expected at least one return node")
    }
  }

  "Return with method invocation without parentheses" in {
    val cpg = code("""
        |def foo()
        | return render json: {}, status: :internal_server_error unless success
        |end
        |""".stripMargin)

    inside(cpg.method.name("foo").body.astChildren.isControlStructure.l) {
      case ifNode :: Nil =>
        ifNode.controlStructureType shouldBe ControlStructureTypes.IF

        val List(successCall: Call) = ifNode.condition.l: @unchecked
        successCall.code shouldBe "self.success"

        val List(ifReturnFalse: Return) = ifNode.whenFalse.isBlock.astChildren.isReturn.l
        ifReturnFalse.code shouldBe "return render json: {}, status: :internal_server_error"

        val List(_, jsonArg: Block, statusArg: Literal) = ifReturnFalse.astChildren.isCall.argument.l: @unchecked
        jsonArg.argumentName shouldBe Some("json")
        jsonArg.code shouldBe "<empty>"

        val List(_: Identifier, hashInitCall: Call) = jsonArg.astChildren.isCall.argument.l: @unchecked
        hashInitCall.methodFullName shouldBe RubyOperators.hashInitializer

        statusArg.argumentName shouldBe Some("status")
        statusArg.code shouldBe ":internal_server_error"

        val List(ifReturnTrue: Return) = ifNode.whenTrue.isBlock.astChildren.isReturn.l
        ifReturnTrue.code shouldBe "return nil"

      case xs => fail(s"Expected two method returns, got [${xs.code.mkString(",")}]")
    }
  }
}
