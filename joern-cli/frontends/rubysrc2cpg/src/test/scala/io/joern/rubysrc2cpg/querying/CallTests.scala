package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.passes.{GlobalTypes, Defines as RubyDefines}
import io.joern.rubysrc2cpg.passes.Defines.{Main, RubyOperators}
import io.joern.rubysrc2cpg.passes.GlobalTypes.kernelPrefix
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, NodeTypes, Operators}
import io.shiftleft.semanticcpg.language.*

class CallTests extends RubyCode2CpgFixture(withPostProcessing = true) {

  "`puts 'hello'` is represented by a CALL node" in {
    val cpg = code("""
                     |puts 'hello'
                     |""".stripMargin)

    val List(puts) = cpg.call.name("puts").l
    puts.lineNumber shouldBe Some(2)
    puts.code shouldBe "puts 'hello'"
    puts.methodFullName shouldBe s"$kernelPrefix.puts"
    puts.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

    val List(selfReceiver: Identifier, hello: Literal) = puts.argument.l: @unchecked
    selfReceiver.argumentIndex shouldBe 0
    selfReceiver.name shouldBe RubyDefines.Self
    selfReceiver.code shouldBe RubyDefines.Self

    hello.argumentIndex shouldBe 1
    hello.code shouldBe "'hello'"
    hello.lineNumber shouldBe Some(2)

    val List(callBase: Call) = puts.receiver.l: @unchecked
    callBase.argumentIndex shouldBe -1
    callBase.methodFullName shouldBe Operators.fieldAccess
    callBase.name shouldBe Operators.fieldAccess
    callBase.code shouldBe "self.puts"

    val List(baseSelf: Identifier, baseProperty: FieldIdentifier) = callBase.argument.l: @unchecked
    baseSelf.argumentIndex shouldBe 1
    baseSelf.name shouldBe RubyDefines.Self
    baseProperty.argumentIndex shouldBe 2
    baseProperty.canonicalName shouldBe "puts"
  }

  "a `Kernel` or bundled class function call in a fully qualified way should have a type ref receiver" in {
    val cpg = code("""
        |Kernel.puts 'hello'
        |Math.atan2(1, 1)
        |""".stripMargin)

    val List(puts) = cpg.call.name("puts").l
    puts.lineNumber shouldBe Some(2)
    puts.code shouldBe "Kernel.puts 'hello'"
    puts.methodFullName shouldBe s"$kernelPrefix.puts"
    puts.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH

    val List(kernelRec: Call) = puts.receiver.l: @unchecked
    kernelRec.argumentIndex shouldBe -1
    kernelRec.typeFullName shouldBe Defines.Any
    kernelRec.code shouldBe "Kernel.puts"

    kernelRec.argument(1).asInstanceOf[TypeRef].typeFullName shouldBe kernelPrefix
    kernelRec.argument(2).asInstanceOf[FieldIdentifier].canonicalName shouldBe "puts"

    val kernelBase = puts.argument(0).asInstanceOf[TypeRef]
    kernelBase.typeFullName shouldBe kernelPrefix
    kernelBase.code shouldBe "Kernel"

    val List(atan2) = cpg.call.name("atan2").l
    atan2.lineNumber shouldBe Some(3)
    atan2.code shouldBe "Math.atan2(1, 1)"
    atan2.methodFullName shouldBe s"${GlobalTypes.builtinPrefix}.Math.atan2"
    atan2.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH

    val List(mathRec: Call) = atan2.receiver.l: @unchecked
    mathRec.argumentIndex shouldBe -1
    mathRec.typeFullName shouldBe Defines.Any
    mathRec.code shouldBe s"Math.atan2"

    mathRec.argument(1).asInstanceOf[TypeRef].typeFullName shouldBe s"${GlobalTypes.builtinPrefix}.Math"
    mathRec.argument(2).asInstanceOf[FieldIdentifier].canonicalName shouldBe "atan2"
  }

  "`foo(1,2)` is represented by a CALL node" in {
    val cpg = code("""
        |foo(1,2)
        |""".stripMargin)

    val List(foo) = cpg.call.name("foo").l
    foo.code shouldBe "foo(1,2)"
    foo.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
    foo.lineNumber shouldBe Some(2)

    val one = foo.argument(1)
    one.code shouldBe "1"

    val two = foo.argument(2)
    two.code shouldBe "2"
  }

  "`x.y(1)` is represented by a `y` CALL with argument `1` and receiver `x.y`" ignore {
    val cpg = code("""
                     |x.y(1)
                     |""".stripMargin)

    val List(fieldAccess) = cpg.fieldAccess.l

    fieldAccess.code shouldBe "x.y"
    fieldAccess.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
    fieldAccess.lineNumber shouldBe Some(2)
    fieldAccess.fieldIdentifier.code.l shouldBe List("y")

    val List(call: Call) = fieldAccess.astParent.toList: @unchecked

    call.code shouldBe "x.y(1)"
    call.name shouldBe "y"
    call.receiver.l shouldBe List(fieldAccess)
    call.lineNumber shouldBe Some(2)

    val List(one) = call.argument.l
    one.code shouldBe "1"
    one.lineNumber shouldBe Some(2)
  }

  "a method and call referencing to a keyword argument" should {

    val cpg = code("""
        |def foo(a, bar: "default")
        |  puts(bar)
        |end
        |
        |foo("hello", bar: "baz")
        |""".stripMargin)

    "contain they keyword in the argumentName property" in {
      inside(cpg.call.nameExact("foo").argument.l) {
        case (self: Identifier) :: (hello: Literal) :: (baz: Literal) :: Nil =>
          self.name shouldBe RubyDefines.Self
          self.argumentIndex shouldBe 0

          hello.code shouldBe "\"hello\""
          hello.argumentIndex shouldBe 1
          hello.argumentName shouldBe None

          baz.code shouldBe "\"baz\""
          baz.argumentIndex shouldBe 2
          baz.argumentName shouldBe Option("bar")
        case xs => fail(s"Invalid call arguments! Got [${xs.code.mkString(", ")}]")
      }
    }
  }

  "a simple object instantiation" should {

    val cpg = code("""class A
        |   def initialize(a, b)
        |   end
        |end
        |
        |a = A.new 1, 2
        |""".stripMargin)

    "create an assignment from `a` to an alloc lowering invocation block" in {
      inside(cpg.method.isModule.assignment.and(_.target.isIdentifier.name("a"), _.source.isBlock).l) {
        case assignment :: Nil =>
          assignment.code shouldBe "a = A.new"
          inside(assignment.argument.l) {
            case (a: Identifier) :: (_: Block) :: Nil =>
              a.name shouldBe "a"
              a.dynamicTypeHintFullName should contain(s"Test0.rb:$Main.A")
            case xs => fail(s"Expected one identifier and one call argument, got [${xs.code.mkString(",")}]")
          }
        case xs => fail(s"Expected a single assignment, got [${xs.code.mkString(",")}]")
      }
    }

    "create an assignment from a temp variable to the alloc call" in {
      inside(cpg.method.isModule.assignment.where(_.target.isIdentifier.name("<tmp-1>")).l) {
        case assignment :: Nil =>
          inside(assignment.argument.l) {
            case (a: Identifier) :: (alloc: Call) :: Nil =>
              a.name shouldBe "<tmp-1>"

              alloc.name shouldBe Operators.alloc
              alloc.methodFullName shouldBe Operators.alloc
              alloc.code shouldBe "A.new"
              alloc.argument.size shouldBe 0
            case xs => fail(s"Expected one identifier and one call argument, got [${xs.code.mkString(",")}]")
          }
        case xs => fail(s"Expected a single assignment, got [${xs.code.mkString(",")}]")
      }
    }

    "create a call to the object's constructor, with the temp variable receiver" in {
      inside(cpg.call.nameExact(RubyDefines.Initialize).l) {
        case constructor :: Nil =>
          inside(constructor.argument.l) {
            case (a: Identifier) :: (one: Literal) :: (two: Literal) :: Nil =>
              a.name shouldBe "<tmp-1>"
              a.typeFullName shouldBe s"Test0.rb:$Main.A"
              a.argumentIndex shouldBe 0

              one.code shouldBe "1"
              two.code shouldBe "2"
            case xs => fail(s"Expected one identifier and one call argument, got [${xs.code.mkString(",")}]")
          }

          val recv = constructor.receiver.head.asInstanceOf[Call]
          recv.methodFullName shouldBe Operators.fieldAccess
          recv.name shouldBe Operators.fieldAccess
          recv.code shouldBe s"A.${RubyDefines.Initialize}"

          recv.argument(1).label shouldBe NodeTypes.CALL
          recv.argument(1).code shouldBe "self.A"
          recv.argument(2).label shouldBe NodeTypes.FIELD_IDENTIFIER
          recv.argument(2).code shouldBe RubyDefines.Initialize
        case xs => fail(s"Expected a single alloc, got [${xs.code.mkString(",")}]")
      }
    }
  }

  "an object instantiation from some expression" should {
    val cpg = code("""def foo
        | params[:type].constantize.new(path)
        |end
        |""".stripMargin)

    "create a call node on the receiver end of the constructor lowering" in {
      inside(cpg.call.nameExact(RubyDefines.Initialize).l) {
        case constructor :: Nil =>
          inside(constructor.argument.l) {
            case (a: Identifier) :: (selfPath: Call) :: Nil =>
              a.name shouldBe "<tmp-0>"
              a.typeFullName shouldBe Defines.Any
              a.argumentIndex shouldBe 0

              selfPath.code shouldBe "self.path"
            case xs => fail(s"Expected one identifier and one call argument, got [${xs.code.mkString(",")}]")
          }

          val recv = constructor.receiver.head.asInstanceOf[Call]
          recv.methodFullName shouldBe Operators.fieldAccess
          recv.name shouldBe Operators.fieldAccess
          recv.code shouldBe s"(<tmp-2> = params[:type].constantize).${RubyDefines.Initialize}"

          recv.argument(2).asInstanceOf[FieldIdentifier].canonicalName shouldBe RubyDefines.Initialize

          inside(recv.argument(1).start.isCall.argument(2).isCall.argument.l) {
            case (paramsAssign: Call) :: (constantize: FieldIdentifier) :: Nil =>
              paramsAssign.code shouldBe "<tmp-1> = params[:type]"
              inside(paramsAssign.argument.l) { case (tmpIdent: Identifier) :: (indexAccess: Call) :: Nil =>
                tmpIdent.name shouldBe "<tmp-1>"

                indexAccess.name shouldBe Operators.indexAccess
                indexAccess.code shouldBe "params[:type]"
              }

              constantize.canonicalName shouldBe "constantize"
          }
        case xs => fail(s"Expected a single alloc, got [${xs.code.mkString(",")}]")
      }
    }
  }

  "a parenthesis-less call" should {
    val cpg = code("""
        |def src = 1
        |def f(p)
        | p += src
        | p
        |end
        |""".stripMargin)

    "correctly create a `src` call instead of identifier" in {
      inside(cpg.call("src").l) {
        case src :: Nil =>
          src.name shouldBe "src"
          src.methodFullName shouldBe s"Test0.rb:$Main.src"
        case xs => fail(s"Expected exactly one `src` call, instead got [${xs.code.mkString(",")}]")
      }
    }
  }

  "a parenthesis-less call as the base of a member access" should {
    val cpg = code("""
        |def f(p)
        | src.join(",")
        |end
        |
        |def src = [1, 2]
        |""".stripMargin)

    "correctly create a `src` call instead of identifier" in {
      inside(cpg.call("src").l) {
        case src :: Nil =>
          src.name shouldBe "src"
          src.methodFullName shouldBe s"Test0.rb:$Main.src"
        case xs => fail(s"Expected exactly one `src` call, instead got [${xs.code.mkString(",")}]")
      }
    }
  }

  "an identifier sharing the name of a previously defined method" should {
    val cpg = code("""
        |def foo()
        |end
        |
        |foo = 1
        |foo
        |""".stripMargin)

    "get precedence over the method" in {
      cpg.call("foo").size shouldBe 0
    }
  }

  "splatting argument for a call should be a single argument" in {
    val cpg = code("""
        |args = [1, 2]
        |foo(*args)
        |""".stripMargin)

    inside(cpg.call("foo").argument.l) {
      case _ :: (args: Call) :: Nil =>
        args.methodFullName shouldBe RubyOperators.splat
        args.code shouldBe "*args"
        args.lineNumber shouldBe Some(3)
      case xs => fail(s"Expected a single `*args` argument under `foo`, got [${xs.code.mkString(",")}]")
    }
  }

  "named parameters in parenthesis-less call to a symbol value should create a correctly named argument" in {
    val cpg            = code("on in: :sequence")
    val List(_, inArg) = cpg.call.nameExact("on").argument.l: @unchecked
    inArg.code shouldBe ":sequence"
    inArg.argumentName shouldBe Option("in")
  }

  "Calls with named arguments using symbols and hash rocket syntax" in {
    val cpg                      = code("render :foo => \"bar\"")
    val List(_, barArg: Literal) = cpg.call.nameExact("render").argument.l: @unchecked
    barArg.code shouldBe "\"bar\""
    barArg.argumentName shouldBe Option("foo")
  }

  "named parameters in parenthesis-less call with a known keyword as the association key should shadow the keyword" in {
    val cpg = code("""
        |foo retry: 3
        |""".stripMargin)
    val List(_, retry) = cpg.call.nameExact("foo").argument.l: @unchecked
    retry.code shouldBe "3"
    retry.argumentName shouldBe Some("retry")
  }

  "a call with a quoted regex literal should have a literal receiver" in {
    val cpg          = code("%r{^/}.freeze()")
    val regexLiteral = cpg.call.nameExact("freeze").receiver.fieldAccess.argument(1).head.asInstanceOf[Literal]
    regexLiteral.typeFullName shouldBe s"$kernelPrefix.Regexp"
    regexLiteral.code shouldBe "%r{^/}"
  }

  "a call with a double colon receiver" in {
    val cpg          = code("::Augeas.open { |aug| aug.get('/augeas/version') }")
    val augeasReceiv = cpg.call.nameExact("open").receiver.head.asInstanceOf[Call]
    augeasReceiv.methodFullName shouldBe Operators.fieldAccess
    augeasReceiv.code shouldBe "(<tmp-0> = ::Augeas).open"

    val selfAugeas = augeasReceiv.argument(1).asInstanceOf[Call]

    selfAugeas.argument(1).asInstanceOf[Identifier].name shouldBe "<tmp-0>"
    selfAugeas.argument(2).asInstanceOf[Call].code shouldBe "self::Augeas"

    augeasReceiv.argument(2).asInstanceOf[FieldIdentifier].canonicalName shouldBe "open"
  }

  "`nil` keyword as a member access should be a literal" in {
    val cpg    = code("nil.to_json")
    val toJson = cpg.fieldAccess.codeExact("nil.to_json").head
    val nilRec = toJson.argument(1).asInstanceOf[Literal]

    nilRec.code shouldBe "nil"
    nilRec.lineNumber shouldBe Option(1)
  }

  "Object initialize calls should be DynamicUnknown" in {
    val cpg = code("""Date.new(2013, 19, 20)""")

    inside(cpg.call.name(RubyDefines.Initialize).l) {
      case initCall :: Nil =>
        initCall.methodFullName shouldBe Defines.DynamicCallUnknownFullName
      case xs => fail(s"Expected one call to initialize, got ${xs.code.mkString}")
    }
  }

  "Member calls where the LHS is a call" should {

    "assign the first call to a temp variable to avoid a second invocation at arg 0" in {
      val cpg = code("a().b()")

      val bCall = cpg.call("b").head
      bCall.code shouldBe "(<tmp-0> = a()).b()"

      // Check receiver
      val bAccess = bCall.receiver.isCall.head
      bAccess.name shouldBe Operators.fieldAccess
      bAccess.methodFullName shouldBe Operators.fieldAccess
      bAccess.code shouldBe "(<tmp-0> = a()).b"

      bAccess.argument(2).asInstanceOf[FieldIdentifier].canonicalName shouldBe "b"

      val aAssign = bAccess.argument(1).asInstanceOf[Call]
      aAssign.name shouldBe Operators.assignment
      aAssign.methodFullName shouldBe Operators.assignment
      aAssign.code shouldBe "<tmp-0> = a()"

      aAssign.argument(1).asInstanceOf[Identifier].name shouldBe "<tmp-0>"
      aAssign.argument(2).asInstanceOf[Call].name shouldBe "a"

      // Check (cached) base
      val base = bCall.argument(0).asInstanceOf[Identifier]
      base.name shouldBe "<tmp-0>"
    }
  }

  "Call with Array Argument" in {
    val cpg = code("""
        |def foo(a)
        |  puts a
        |end
        |
        |foo([:b, :c => 1])
        |""".stripMargin)

    inside(cpg.call.name("foo").l) {
      case fooCall :: Nil =>
        inside(fooCall.argument.l) {
          case _ :: (arrayArg: Call) :: Nil =>
            arrayArg.code shouldBe "[:b, :c => 1]"
            arrayArg.methodFullName shouldBe Operators.arrayInitializer

            inside(arrayArg.argument.l) {
              case (elem1: Literal) :: (elem2: Call) :: Nil =>
                elem1.code shouldBe ":b"
                elem2.code shouldBe ":c => 1"

                elem2.methodFullName shouldBe RubyDefines.RubyOperators.association
              case xs => fail(s"Expected two args for elements, got ${xs.code.mkString(",")}")
            }
          case xs => fail(s"Expected two args, got ${xs.code.mkString(",")}")
        }
      case xs => fail(s"Expected one call for foo, got ${xs.code.mkString}")
    }
  }

  "Calls separated by `tmp` should render correct `code` properties" in {
    val cpg = code("""
        |User.find_by(auth_token: cookies[:auth_token].to_s)
        |""".stripMargin)

    cpg.call("find_by").code.head shouldBe "(<tmp-0> = User).find_by(auth_token: cookies[:auth_token].to_s)"
    cpg.call(Operators.indexAccess).code.head shouldBe "cookies[:auth_token]"
    cpg.fieldAccess
      .where(_.fieldIdentifier.canonicalNameExact("@to_s"))
      .code
      .head shouldBe "(<tmp-1> = cookies[:auth_token]).to_s"
  }

  "Calls with multiple splat args" in {
    val cpg = code("""
        |    doorkeeper_application&.includes_scope?(
        |      *::Gitlab::Auth::API_SCOPE, *::Gitlab::Auth::READ_API_SCOPE,
        |      *::Gitlab::Auth::ADMIN_SCOPES, *::Gitlab::Auth::REPOSITORY_SCOPES,
        |      *::Gitlab::Auth::REGISTRY_SCOPES
        |    )
        |""".stripMargin)

    inside(cpg.call.name("includes_scope\\?").argument.l) {
      case _ :: (apiScopeSplat: Call) :: (readScopeSplat: Call) :: (adminScopeSplat: Call) :: (repoScopeSplat: Call) :: (registryScopeSplat: Call) :: Nil =>
        apiScopeSplat.code shouldBe "*::Gitlab::Auth::API_SCOPE"
        apiScopeSplat.methodFullName shouldBe RubyOperators.splat

        readScopeSplat.code shouldBe "*::Gitlab::Auth::READ_API_SCOPE"
        readScopeSplat.methodFullName shouldBe RubyOperators.splat

        adminScopeSplat.code shouldBe "*::Gitlab::Auth::ADMIN_SCOPES"
        adminScopeSplat.methodFullName shouldBe RubyOperators.splat

        repoScopeSplat.code shouldBe "*::Gitlab::Auth::REPOSITORY_SCOPES"
        repoScopeSplat.methodFullName shouldBe RubyOperators.splat

        registryScopeSplat.code shouldBe "*::Gitlab::Auth::REGISTRY_SCOPES"
        registryScopeSplat.methodFullName shouldBe RubyOperators.splat

      case xs => fail(s"Expected 5 arguments for call, got [${xs.code.mkString(",")}]")
    }
  }
}
