package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.passes.Defines as RDefines
import io.joern.rubysrc2cpg.passes.Defines.{Main, RubyOperators}
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, NodeTypes, Operators}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes.{Assignment, FieldAccess}

class MethodTests extends RubyCode2CpgFixture {

  "`def f(x) = 1`" should {
    val cpg = code("""
        |def f(x) = 1
        |f(1)
        |""".stripMargin)

    "be represented by a METHOD node" in {
      val List(f) = cpg.method.name("f").l

      f.fullName shouldBe s"Test0.rb:$Main.f"
      f.isExternal shouldBe false
      f.lineNumber shouldBe Some(2)
      f.numberOfLines shouldBe 1

      val List(x) = f.parameter.name("x").l
      x.index shouldBe 1
      x.isVariadic shouldBe false
      x.lineNumber shouldBe Some(2)

      val List(fSelf) = f.parameter.name(RDefines.Self).l
      fSelf.index shouldBe 0
      fSelf.isVariadic shouldBe false
      fSelf.lineNumber shouldBe Some(2)
      fSelf.referencingIdentifiers.size shouldBe 0

      val List(mSelf) = cpg.method.isModule.parameter.name(RDefines.Self).l
      mSelf.index shouldBe 0
      mSelf.isVariadic shouldBe false
      mSelf.lineNumber shouldBe Some(2)
      mSelf.referencingIdentifiers.size shouldBe 3
    }

    "have a corresponding bound type" in {
      val List(fType) = cpg.typeDecl("f").l
      fType.fullName shouldBe s"Test0.rb:$Main.f"
      fType.code shouldBe "def f(x) = 1"
      fType.astParentFullName shouldBe s"Test0.rb:$Main"
      fType.astParentType shouldBe NodeTypes.METHOD
      val List(fMethod) = fType.iterator.boundMethod.l
      fType.fullName shouldBe s"Test0.rb:$Main.f"
    }

    "create a 'fake' method for the file" in {
      val List(m) = cpg.method.nameExact(RDefines.Main).l
      m.fullName shouldBe s"Test0.rb:$Main"
      m.isModule.nonEmpty shouldBe true
    }
  }

  "`def f ... return 1 ... end` is represented by a METHOD node" in {
    val cpg = code("""
                     |def f
                     | return 1
                     |end
                     |""".stripMargin)

    val List(f) = cpg.method.name("f").l

    f.fullName shouldBe s"Test0.rb:$Main.f"
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

    f.fullName shouldBe s"Test0.rb:$Main.f"
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

    f.fullName shouldBe s"Test0.rb:$Main.f"
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
                thisParam.typeFullName shouldBe s"Test0.rb:$Main.C<class>"
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
                thisParam.typeFullName shouldBe s"Test0.rb:$Main.C<class>"
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
          xs.typeFullName shouldBe RDefines.prefixAsCoreType("Array")
        case xs => fail(s"Expected `foo` to have one parameter, got [${xs.code.mkString(", ")}]")
      }
    }

    "be interpreted as a hash type parameter if two stars given" in {
      inside(cpg.method("bar").parameter.indexGt(0).l) {
        case ys :: Nil =>
          ys.name shouldBe "ys"
          ys.code shouldBe "**ys"
          ys.isVariadic shouldBe true
          ys.typeFullName shouldBe RDefines.prefixAsCoreType("Hash")
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
          inside(foo.method.nameNot(RDefines.Initialize, RDefines.TypeDeclBody).l) {
            case xeq :: x :: bar :: Nil =>
              xeq.name shouldBe "x="
              x.name shouldBe "x"
              bar.name shouldBe "bar="

              bar.parameter.name.l shouldBe List("self", "args", "&block")
              // bar forwards parameters to a call to the aliased method
              inside(bar.call.name("x=").l) {
                case barCall :: Nil =>
                  inside(barCall.argument.l) {
                    case _ :: (args: Call) :: (blockId: Identifier) :: Nil =>
                      args.name shouldBe RubyOperators.splat
                      args.code shouldBe "*args"
                      args.argumentIndex shouldBe 1

                      blockId.name shouldBe "&block"
                      blockId.code shouldBe "&block"
                      blockId.argumentIndex shouldBe 2
                    case xs =>
                      fail(s"Expected a two arguments for the call `x=`,  instead got [${xs.code.mkString(",")}]")
                  }
                  barCall.code shouldBe "x=(*args, &block)"
                case xs => fail(s"Expected a single call to `bar=`,  instead got [${xs.code.mkString(",")}]")
              }
            case xs => fail(s"Expected a three virtual methods under `Foo`, instead got [${xs.code.mkString(",")}]")
          }
        case xs => fail(s"Expected a single type decl for `Foo`, instead got [${xs.code.mkString(",")}]")
      }
    }
  }

  "aliased methods with `alias_method`" should {
    val cpg = code("""
        |class Foo
        |  def aliasable(bbb)
        |    puts bbb
        |  end
        |
        |  alias_method :print_something, :aliasable
        |
        |  def someMethod(aaa)
        |    print_something(aaa)
        |  end
        |end
        |
        |""".stripMargin)

    "similarly alias the method as if it were calling `alias`" in {
      inside(cpg.typeDecl("Foo").l) {
        case foo :: Nil =>
          inside(foo.method.nameNot(RDefines.Initialize, RDefines.TypeDeclBody).l) {
            case a :: p :: s :: Nil =>
              a.name shouldBe "aliasable"
              p.name shouldBe "print_something"
              s.name shouldBe "someMethod"

              p.parameter.name.l shouldBe List("self", "args", "&block")
              // bar forwards parameters to a call to the aliased method
              inside(p.call.name("aliasable").l) {
                case aliasableCall :: Nil =>
                  inside(aliasableCall.argument.l) {
                    case _ :: (args: Call) :: (blockId: Identifier) :: Nil =>
                      args.name shouldBe RubyOperators.splat
                      args.code shouldBe "*args"
                      args.argumentIndex shouldBe 1

                      blockId.name shouldBe "&block"
                      blockId.code shouldBe "&block"
                      blockId.argumentIndex shouldBe 2
                    case xs =>
                      fail(
                        s"Expected a two arguments for the call `aliasable`,  instead got [${xs.code.mkString(",")}]"
                      )
                  }
                  aliasableCall.code shouldBe "aliasable(*args, &block)"
                case xs => fail(s"Expected a single call to `aliasable`,  instead got [${xs.code.mkString(",")}]")
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
      inside(cpg.typeDecl.name("F").method.nameExact("bar", "baz").l) {
        case bar :: baz :: Nil =>
          inside(bar.parameter.l) {
            case thisParam :: xParam :: Nil =>
              thisParam.name shouldBe RDefines.Self
              thisParam.code shouldBe "F"
              thisParam.typeFullName shouldBe s"Test0.rb:$Main.F<class>"

              xParam.name shouldBe "x"
            case xs => fail(s"Expected two parameters, got ${xs.name.mkString(", ")}")
          }

          inside(baz.parameter.l) {
            case thisParam :: xParam :: Nil =>
              thisParam.name shouldBe RDefines.Self
              thisParam.code shouldBe "F"
              thisParam.typeFullName shouldBe s"Test0.rb:$Main.F<class>"

              xParam.name shouldBe "x"
              xParam.code shouldBe "x"
            case xs => fail(s"Expected two parameters, got ${xs.name.mkString(", ")}")
          }
        case xs => fail(s"Expected bar and baz to exist under F, instead got ${xs.code.mkString(", ")}")
      }
    }

    // TODO: we cannot bind baz as this is a dynamic assignment to `F` which is trickier to determine
    //   Also, double check bindings
    "have bindings to the singleton module TYPE_DECL" ignore {
      cpg.typeDecl.name("F<class>").methodBinding.methodFullName.l shouldBe List(s"Test0.rb:$Main.F.bar")
    }

    "baz should not exist in the <main> block" in {
      inside(cpg.method.isModule.l) {
        case prog :: Nil =>
          inside(prog.block.astChildren.isMethod.name("baz").l) {
            case Nil => // passing case
            case _   => fail("Baz should not exist under program method block")
          }
        case _ => fail("Expected one Method for <block>")
      }
    }
  }

  "Singleton methods binding to an unresolvable variable/type should bind to the next AST parent" in {
    val cpg = code("""
        |class C
        | def something.foo
        | end
        |end
        |""".stripMargin)

    val foo = cpg.method.nameExact("foo").head

    foo.definingTypeDecl.map(_.name) shouldBe Option("C")
    foo.astParent shouldBe cpg.typeDecl("C").head
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
          existsMethod.fullName shouldBe s"Test0.rb:$Main.exists?"
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
                    case nilBlock :: breakBlock :: Nil =>
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
      inside(cpg.method.name("foo").controlStructure.l) {
        case tryStruct :: emptyElseStruct :: ensureStruct :: Nil =>
          tryStruct.controlStructureType shouldBe ControlStructureTypes.TRY
          val body = tryStruct.astChildren.head
          body.ast.isLiteral.code.l shouldBe List("1")

          emptyElseStruct.controlStructureType shouldBe ControlStructureTypes.ELSE
          emptyElseStruct.ast.isLiteral.code.l shouldBe List("nil")

          ensureStruct.controlStructureType shouldBe ControlStructureTypes.FINALLY
          ensureStruct.ast.isLiteral.code.l shouldBe List("2")

        case xs => fail(s"Expected three structures, got ${xs.code.mkString(",")}")
      }
    }
  }

  "Chained method calls" should {
    val cpg = code("""
        |class Foo
        | def authenticate(email, password)
        |   auth = nil
        |   a = getPass()
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
              leftArg.name shouldBe "a"

              rightArg.name shouldBe "hexdigest"
              rightArg.code shouldBe "(<tmp-1> = Digest::MD5).hexdigest(password)"

              val hexDigestFa = rightArg.receiver.head.asInstanceOf[FieldAccess]
              hexDigestFa.code shouldBe "(<tmp-1> = Digest::MD5).hexdigest"

              val tmp1Assign = hexDigestFa.argument(1).asInstanceOf[Assignment]
              tmp1Assign.code shouldBe "<tmp-1> = Digest::MD5"

              val md5Fa = tmp1Assign.source.asInstanceOf[FieldAccess]
              md5Fa.code shouldBe "(<tmp-0> = Digest)::MD5"

              val tmp0Assign = md5Fa.argument(1).asInstanceOf[Assignment]
              tmp0Assign.code shouldBe "<tmp-0> = Digest"

              val digestFa = tmp0Assign.source.asInstanceOf[FieldAccess]
              digestFa.argument(1).asInstanceOf[Identifier].name shouldBe RDefines.Self
              digestFa.argument(2).asInstanceOf[FieldIdentifier].canonicalName shouldBe "Digest"
            case xs => fail(s"Expected 2 arguments, got ${xs.code.mkString(", ")} instead")
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
      inside(cpg.method.name(RDefines.Main).filename("t1.rb").assignment.l) {
        case moduleAssignment :: classAssignment :: methodAssignment :: Nil =>
          moduleAssignment.code shouldBe "self.A = module A (...)"
          classAssignment.code shouldBe "self.B = class B (...)"
          methodAssignment.code shouldBe "self.c = def c (...)"

          inside(moduleAssignment.argument.l) {
            case (lhs: Call) :: (rhs: TypeRef) :: Nil =>
              lhs.code shouldBe "self.A"
              lhs.name shouldBe Operators.fieldAccess
              rhs.typeFullName shouldBe s"t1.rb:$Main.A<class>"
            case xs => fail(s"Expected lhs and rhs, instead got ${xs.code.mkString(",")}")
          }

          inside(classAssignment.argument.l) {
            case (lhs: Call) :: (rhs: TypeRef) :: Nil =>
              lhs.code shouldBe "self.B"
              lhs.name shouldBe Operators.fieldAccess
              rhs.typeFullName shouldBe s"t1.rb:$Main.B<class>"
            case xs => fail(s"Expected lhs and rhs, instead got ${xs.code.mkString(",")}")
          }

          inside(methodAssignment.argument.l) {
            case (lhs: Call) :: (rhs: MethodRef) :: Nil =>
              lhs.code shouldBe "self.c"
              lhs.name shouldBe Operators.fieldAccess
              rhs.methodFullName shouldBe s"t1.rb:$Main.c"
              rhs.typeFullName shouldBe s"t1.rb:$Main.c"
            case xs => fail(s"Expected lhs and rhs, instead got ${xs.code.mkString(",")}")
          }

        case xs => fail(s"Expected three assignments, got [${xs.code.mkString(",")}]")
      }
    }

    "not be present in other files" in {
      inside(cpg.method.name(RDefines.Main).filename("t2.rb").assignment.l) {
        case classAssignment :: methodAssignment :: Nil =>
          classAssignment.code shouldBe "self.D = class D (...)"
          methodAssignment.code shouldBe "self.e = def e (...)"

          inside(classAssignment.argument.l) {
            case (lhs: Call) :: (rhs: TypeRef) :: Nil =>
              lhs.code shouldBe "self.D"
              lhs.name shouldBe Operators.fieldAccess
              rhs.typeFullName shouldBe s"t2.rb:$Main.D<class>"
            case xs => fail(s"Expected lhs and rhs, instead got ${xs.code.mkString(",")}")
          }

          inside(methodAssignment.argument.l) {
            case (lhs: Call) :: (rhs: MethodRef) :: Nil =>
              lhs.code shouldBe "self.e"
              lhs.name shouldBe Operators.fieldAccess
              rhs.methodFullName shouldBe s"t2.rb:$Main.e"
              rhs.typeFullName shouldBe s"t2.rb:$Main.e"
            case xs => fail(s"Expected lhs and rhs, instead got ${xs.code.mkString(",")}")
          }

        case xs => fail(s"Expected two assignments, got [${xs.code.mkString(",")}]")
      }
    }

    "be placed in order of definition" in {
      inside(cpg.method.name(RDefines.Main).filename("t1.rb").block.astChildren.isCall.l) {
        case (a1: Call) :: (a2: Call) :: (a3: Call) :: (a4: Call) :: (a5: Call) :: Nil =>
          a1.code shouldBe "self.A = module A (...)"
          a2.code shouldBe "(<tmp-0> = self::A)::<body>()"
          a3.code shouldBe "self.B = class B (...)"
          a4.code shouldBe "(<tmp-1> = self::B)::<body>()"
          a5.code shouldBe "self.c = def c (...)"
        case xs => fail(s"Expected assignments to appear before definitions, instead got [${xs.mkString("\n")}]")
      }
    }
  }

  "Splatting and normal argument" in {
    val cpg = code("""
        |def foo(*x, y)
        |end
        |""".stripMargin)

    inside(cpg.method.name("foo").l) {
      case fooMethod :: Nil =>
        inside(fooMethod.method.parameter.l) {
          case selfArg :: splatArg :: normalArg :: Nil =>
            splatArg.code shouldBe "*x"
            splatArg.index shouldBe 1

            normalArg.code shouldBe "y"
            normalArg.index shouldBe 2
          case xs => fail(s"Expected two parameters, got [${xs.code.mkString(",")}]")
        }
      case xs => fail(s"Expected one method, got [${xs.code.mkString(",")}]")
    }
  }

  "Splatting argument in call" in {
    val cpg = code("""
        |def foo(a, b)
        |end
        |
        |x = 1,2
        |foo(*x, y)
        |""".stripMargin)

    inside(cpg.call.name("foo").l) {
      case fooCall :: Nil =>
        inside(fooCall.argument.l) {
          case selfArg :: xArg :: yArg :: Nil =>
            xArg.code shouldBe "*x"
            yArg.code shouldBe "self.y"
          case xs => fail(s"Expected two args, got [${xs.code.mkString(",")}]")
        }
      case xs => fail(s"Expected one call to foo, got [${xs.code.mkString(",")}]")
    }
  }

  "a nested method declaration inside of a do-block should connect the member node to the bound type decl" in {
    val cpg = code("""
        |foo do
        | def bar
        | end
        |end
        |""".stripMargin)

    val parentType = cpg.member("bar").typeDecl.head
    parentType.isLambda should not be empty
    parentType.methodBinding.methodFullName.head should endWith("<lambda>0")
  }

  "a method that is redefined should have a counter suffixed to ensure uniqueness" in {
    val cpg = code("""
          |def foo;end
          |def bar;end
          |def foo;end
          |def foo;end
          |""".stripMargin)

    cpg.method.name("(foo|bar).*").name.l shouldBe List("foo", "bar", "foo", "foo")
    cpg.method.name("(foo|bar).*").fullName.l shouldBe List(
      s"Test0.rb:$Main.foo",
      s"Test0.rb:$Main.bar",
      s"Test0.rb:$Main.foo0",
      s"Test0.rb:$Main.foo1"
    )
  }

  "MemberCall with a function name the same as a reserved keyword" in {
    val cpg = code("""
        |batch.retry!()
        |""".stripMargin)

    inside(cpg.call.name(".*retry!").l) {
      case batchCall :: Nil =>
        batchCall.name shouldBe "retry!"
        batchCall.code shouldBe "(<tmp-0> = batch).retry!()"

        inside(batchCall.receiver.l) {
          case (receiverCall: Call) :: Nil =>
            receiverCall.name shouldBe Operators.fieldAccess
            receiverCall.code shouldBe "(<tmp-0> = batch).retry!"

            val selfBatch = receiverCall.argument(1).asInstanceOf[Call]
            selfBatch.code shouldBe "<tmp-0> = batch"

            val retry = receiverCall.argument(2).asInstanceOf[FieldIdentifier]
            retry.code shouldBe "retry!"

          case xs => fail(s"Expected one receiver for call, got [${xs.code.mkString(",")}]")
        }

      case xs => fail(s"Expected one method for batch.retry, got [${xs.code.mkString(",")}]")
    }
  }

  "Call with :: syntax and reserved keyword" in {
    val cpg = code("""
        |batch::retry!()
        |""".stripMargin)

    inside(cpg.call.name(".*retry!").l) {
      case batchCall :: Nil =>
        batchCall.name shouldBe "retry!"
        batchCall.code shouldBe "(<tmp-0> = batch)::retry!()"

        inside(batchCall.receiver.l) {
          case (receiverCall: Call) :: Nil =>
            receiverCall.name shouldBe Operators.fieldAccess
            receiverCall.code shouldBe "(<tmp-0> = batch).retry!"

            val selfBatch = receiverCall.argument(1).asInstanceOf[Call]
            selfBatch.code shouldBe "<tmp-0> = batch"

            val retry = receiverCall.argument(2).asInstanceOf[FieldIdentifier]
            retry.code shouldBe "retry!"

          case xs => fail(s"Expected one receiver for call, got [${xs.code.mkString(",")}]")
        }

      case xs => fail(s"Expected one method for batch.retry, got [${xs.code.mkString(",")}]")
    }
  }

  "Call with reserved keyword as base and call name using . notation" in {
    val cpg = code("""
        |retry.retry!()
        |""".stripMargin)

    inside(cpg.call.name(".*retry!").l) {
      case batchCall :: Nil =>
        batchCall.name shouldBe "retry!"
        batchCall.code shouldBe "(<tmp-0> = retry).retry!()"

        inside(batchCall.receiver.l) {
          case (receiverCall: Call) :: Nil =>
            receiverCall.name shouldBe Operators.fieldAccess
            receiverCall.code shouldBe "(<tmp-0> = retry).retry!"

            val selfBatch = receiverCall.argument(1).asInstanceOf[Call]
            selfBatch.code shouldBe "<tmp-0> = retry"

            val retry = receiverCall.argument(2).asInstanceOf[FieldIdentifier]
            retry.code shouldBe "retry!"

          case xs => fail(s"Expected one receiver for call, got [${xs.code.mkString(",")}]")
        }

      case xs => fail(s"Expected one method for batch.retry, got [${xs.code.mkString(",")}]")
    }
  }

  "Call with reserved keyword as base and call name" in {
    val cpg = code("""
        |retry::retry!()
        |""".stripMargin)

    inside(cpg.call.name(".*retry!").l) {
      case batchCall :: Nil =>
        batchCall.name shouldBe "retry!"
        batchCall.code shouldBe "(<tmp-0> = retry)::retry!()"

        inside(batchCall.receiver.l) {
          case (receiverCall: Call) :: Nil =>
            receiverCall.name shouldBe Operators.fieldAccess
            receiverCall.code shouldBe "(<tmp-0> = retry).retry!"

            val selfBatch = receiverCall.argument(1).asInstanceOf[Call]
            selfBatch.code shouldBe "<tmp-0> = retry"

            val retry = receiverCall.argument(2).asInstanceOf[FieldIdentifier]
            retry.code shouldBe "retry!"

          case xs => fail(s"Expected one receiver for call, got [${xs.code.mkString(",")}]")
        }

      case xs => fail(s"Expected one method for batch.retry, got [${xs.code.mkString(",")}]")
    }
  }

  "%x should be represented as a call to EXEC" in {
    val cpg = code("""
        |%x(ls -l)
        |""".stripMargin)

    inside(cpg.call.name(RubyOperators.backticks).l) {
      case execCall :: Nil =>
        execCall.name shouldBe RubyOperators.backticks
        inside(execCall.argument.l) {
          case selfArg :: lsArg :: Nil =>
            selfArg.code shouldBe "self"
            lsArg.code shouldBe "ls -l"
          case xs => fail(s"expected 2 arguments, got [${xs.code.mkString(",")}]")
        }
      case xs => fail(s"Expected one call to exec, got [${xs.code.mkString(",")}]")
    }
  }

  "MemberAccessCommand with two parameters" in {
    val cpg = code("foo&.bar 1,2")

    inside(cpg.call.name("bar").l) {
      case barCall :: Nil =>
        inside(barCall.argument.l) {
          case _ :: (arg1: Literal) :: (arg2: Literal) :: Nil =>
            arg1.code shouldBe "1"
            arg1.typeFullName shouldBe RDefines.prefixAsCoreType(RDefines.Integer)

            arg2.code shouldBe "2"
            arg2.typeFullName shouldBe RDefines.prefixAsCoreType(RDefines.Integer)
          case xs => fail(s"Expected three args, got [${xs.code.mkString(",")}]")
        }

      case xs => fail(s"Expected one call, got [${xs.code.mkString(",")}]")
    }
  }

  "Method def in class defined in a namespace" in {
    val cpg = code("""
        |class Api::V1::MobileController
        |  def show
        |  end
        |end
        |""".stripMargin)

    inside(cpg.method.name("show").l) {
      case showMethod :: Nil =>
        showMethod.astParentFullName shouldBe "Test0.rb:<main>.Api.V1.MobileController"
        showMethod.astParentType shouldBe NodeTypes.TYPE_DECL
      case xs => fail(s"Expected one methood, got ${xs.name.mkString(",")}")
    }
  }

  "Method def with mandatory arg after splat arg" in {
    val cpg = code("""
        |def foo(a=1, *b, c)
        |end
        |""".stripMargin)

    inside(cpg.method.name("foo").parameter.l) {
      case _ :: aParam :: bParam :: cParam :: Nil =>
        aParam.code shouldBe "a=1"
        bParam.code shouldBe "*b"
        cParam.code shouldBe "c"
      case xs => fail(s"Expected 4 params, got ${xs.code.mkString(",")}")
    }
  }

  "Unnamed proc parameters" should {
    val cpg = code("""
        |def outer_method(&)
        |  puts "In outer_method"
        |  inner_method(&)
        |end
        |
        |def inner_method(&)
        |  puts "In inner_method"
        |  yield if block_given?
        |end
        |
        |outer_method do
        |  puts "Hello from the block!"
        |end
        |""".stripMargin)

    "generate and reference proc param" in {
      inside(cpg.method.name("outer_method").l) {
        case outerMethod :: Nil =>
          val List(_, procParam) = outerMethod.parameter.l
          procParam.name shouldBe "<proc-param-0>"

          inside(outerMethod.call.name("inner_method").argument.l) {
            case _ :: procParamArg :: Nil =>
              procParamArg.code shouldBe "<proc-param-0>"
            case xs => fail(s"Expected two arguments, got [${xs.code.mkString(",")}]")
          }

        case xs => fail(s"Expected one method def, got [${xs.name.mkString(",")}]")
      }
    }

    "call correct proc param in `yield`" in {
      inside(cpg.method.name("inner_method").l) {
        case innerMethod :: Nil =>
          val List(_, procParam) = innerMethod.parameter.l
          procParam.name shouldBe "<proc-param-1>"

          innerMethod.call.nameExact("call").argument.isIdentifier.name.l shouldBe List("<proc-param-1>")

        case xs => fail(s"Expected one method def, got [${xs.name.mkString(",")}]")
      }
    }
  }

  "lambdas as arguments to a long chained call" should {
    val cpg = code("""
        |def foo(xs, total_ys, hex_values)
        |  xs.map.with_index { |f, i| [f / total_ys, hex_values[i]] }                       # 1
        |    .sort_by { |r| -r[0] }                                                         # 2
        |    .reject { |r| r[1].size == 8 && r[1].end_with?('00') }                         # 3
        |    .map { |r| Foo::Bar::Baz.new(*r[1][0..5].scan(/../).map { |c| c.to_i(16) }) }  # 4 & 5
        |    .slice(0, quantity)
        |  end
        |""".stripMargin)

    "not write lambda nodes that are already assigned to some temp variable" in {
      cpg.typeRef.typeFullName(".*Proc").size shouldBe 5
      cpg.typeRef.whereNot(_.astParent).size shouldBe 0
    }

    "resolve cached lambdas correctly" in {
      def getLineNumberOfLambdaForCall(callName: String) =
        cpg.call.nameExact(callName).argument.isTypeRef.typ.referencedTypeDecl.lineNumber.head

      getLineNumberOfLambdaForCall("with_index") shouldBe 3
      getLineNumberOfLambdaForCall("sort_by") shouldBe 4
      getLineNumberOfLambdaForCall("reject") shouldBe 5
      getLineNumberOfLambdaForCall("map") shouldBe 6
    }
  }

  "Forwarded args from method to call" should {
    val cpg = code("""
        |def foo(...)
        |   bar('foo', ...)
        |end
        |
        |""".stripMargin)

    "create a '...' parameter node" in {
      inside(cpg.method.nameExact("foo").parameter.l) { case _ :: forwardArgs :: Nil =>
        forwardArgs.name shouldBe "..."
        forwardArgs.code shouldBe "(...)"
      }
    }

    "create a '...' identifier node as a call argument" in {
      inside(cpg.call("bar").argument.isIdentifier.l) { case _ :: forwardedArgs :: Nil =>
        forwardedArgs.name shouldBe "..."
        forwardedArgs.code shouldBe "..."
        forwardedArgs.argumentIndex shouldBe 2
      }
    }
  }

  "Implicit return of range expression" in {
    val cpg = code("""
                     |def size_range
                     |    1..MAX_FILE_SIZE
                     |end""".stripMargin)

    inside(cpg.method.name("size_range").methodReturn.toReturn.l) {
      case rangeReturn :: Nil =>
        rangeReturn.code shouldBe "1..MAX_FILE_SIZE"

        val List(rangeOp) = rangeReturn.astChildren.isCall.l
        rangeOp.methodFullName shouldBe Operators.range

        val List(lhs: Literal, rhs: Call) = rangeOp.argument.l: @unchecked
        lhs.code shouldBe "1"

        rhs.code shouldBe "self.MAX_FILE_SIZE"
      case xs => fail(s"Expected one return, got [${xs.code.mkString(",")}]")
    }
  }

  "Method call with same name as reserved keyword" in {
    val cpg = code("""
        |    def public
        |      list.sort_by(&:position).filter_map { |category| category.slug if category.visible_to_public? }
        |    end
        |
        |    def notifiable
        |      public
        |    end
        |
        |    def not_notifiable
        |       public
        |       puts 1
        |       puts 2
        |    end
        |""".stripMargin)

    inside(cpg.method.name("notifiable").body.astChildren.isReturn.astChildren.isCall.name("public").l) {
      case publicCall :: Nil =>
        publicCall.code shouldBe "public"

        val List(selfArg) = publicCall.argument.l
      case xs => fail(s"Expected one call, got ${xs.code.mkString(",")}")
    }

    inside(cpg.method.name("not_notifiable").body.astChildren.isCall.name("public").l) {
      case publicCall :: Nil =>
        publicCall.code shouldBe "public"

        val List(selfArg) = publicCall.argument.l
      case xs => fail(s"Expected one call, got ${xs.code.mkString(",")}")
    }
  }
}
