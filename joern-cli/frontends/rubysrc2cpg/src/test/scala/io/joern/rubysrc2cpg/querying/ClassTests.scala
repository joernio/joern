package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.passes.Defines.{Initialize, Main, TypeDeclBody}
import io.joern.rubysrc2cpg.passes.{GlobalTypes, Defines as RubyDefines}
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, NodeTypes, Operators}
import io.shiftleft.semanticcpg.language.*

class ClassTests extends RubyCode2CpgFixture {

  "`class C ; end` is represented by an empty TYPE_DECL node" in {
    val cpg = code("""
                     |class C ; end
                     |""".stripMargin)

    val List(classC) = cpg.typeDecl.name("C").l

    classC.inheritsFromTypeFullName shouldBe List()
    classC.fullName shouldBe s"Test0.rb:$Main.C"
    classC.lineNumber shouldBe Some(2)
    classC.baseType.l shouldBe List()
    classC.member.name.l shouldBe List(RubyDefines.TypeDeclBody, RubyDefines.Initialize)
    classC.method.name.l shouldBe List(RubyDefines.TypeDeclBody, RubyDefines.Initialize)

    val List(singletonC) = cpg.typeDecl.nameExact("C<class>").l
    singletonC.inheritsFromTypeFullName shouldBe List()
    singletonC.fullName shouldBe s"Test0.rb:$Main.C<class>"
    singletonC.lineNumber shouldBe Some(2)
    singletonC.baseType.l shouldBe List()
    singletonC.member.name.l shouldBe List()
    singletonC.method.name.l shouldBe List()
  }

  "`class C < D` is represented by a TYPE_DECL node inheriting from `D`" in {
    val cpg = code("""
                     |class C < D
                     |
                     |end
                     |""".stripMargin)

    val List(classC) = cpg.typeDecl.name("C").l

    classC.inheritsFromTypeFullName shouldBe List("D")
    classC.fullName shouldBe s"Test0.rb:$Main.C"
    classC.lineNumber shouldBe Some(2)
    classC.member.name.l shouldBe List(RubyDefines.TypeDeclBody, RubyDefines.Initialize)
    classC.method.name.l shouldBe List(RubyDefines.TypeDeclBody, RubyDefines.Initialize)

    val List(typeD) = classC.baseType.l
    typeD.name shouldBe "D"

    val List(singletonC) = cpg.typeDecl.nameExact("C<class>").l

    singletonC.inheritsFromTypeFullName shouldBe List("D<class>")
    singletonC.fullName shouldBe s"Test0.rb:$Main.C<class>"
    singletonC.lineNumber shouldBe Some(2)
    singletonC.member.name.l shouldBe List()
    singletonC.method.name.l shouldBe List()
  }

  "`attr_reader :a` is represented by a `@a` MEMBER node" in {
    val cpg = code("""
                     |class C
                     | attr_reader :a
                     |end
                     |""".stripMargin)

    val List(classC)  = cpg.typeDecl.name("C").l
    val List(aMember) = classC.member.nameExact("@a").l

    aMember.code shouldBe "attr_reader :a"
    aMember.lineNumber shouldBe Some(3)

    val List(singletonC) = cpg.typeDecl.name("C<class>").l
    singletonC.member.nameExact("@a").isEmpty shouldBe true

    val List(aGetterMember) = classC.member.nameExact("a").l
    aGetterMember.dynamicTypeHintFullName should contain("Test0.rb:<main>.C.a")
  }

  "`attr_reader :'abc'` is represented by a `@abc` MEMBER node" in {
    val cpg = code("""
                     |class C
                     | attr_reader :'abc'
                     |end
                     |""".stripMargin)

    val List(classC)    = cpg.typeDecl.name("C").l
    val List(abcMember) = classC.member.name("@abc").l

    abcMember.code shouldBe "attr_reader :'abc'"
    abcMember.lineNumber shouldBe Some(3)

    val List(aMember) = classC.member.nameExact("abc").l
    aMember.dynamicTypeHintFullName should contain("Test0.rb:<main>.C.abc")
  }

  "`attr_reader :'abc' creates an `abc` METHOD node" in {
    val cpg = code("""
                     |class C
                     | attr_reader :'abc'
                     |end
                     |""".stripMargin)

    val List(classC)    = cpg.typeDecl.name("C").l
    val List(methodAbc) = classC.method.name("abc").l

    methodAbc.code shouldBe "def abc (...)"
    methodAbc.lineNumber shouldBe Some(3)
    methodAbc.parameter.indexGt(0).isEmpty shouldBe true
    methodAbc.fullName shouldBe s"Test0.rb:$Main.C.abc"

    val List(ret: Return)          = methodAbc.methodReturn.cfgIn.l: @unchecked
    val List(abcFieldAccess: Call) = ret.astChildren.l: @unchecked
    ret.code shouldBe "@abc"
    abcFieldAccess.name shouldBe Operators.fieldAccess
    abcFieldAccess.code shouldBe "self.@abc"

    val List(aMember) = classC.member.nameExact("abc").l
    aMember.dynamicTypeHintFullName should contain("Test0.rb:<main>.C.abc")
  }

  "`attr_reader :a, :b` is represented by `@a`, `@b` MEMBER nodes" in {
    val cpg = code("""
                     |class C
                     | attr_reader :a, :b
                     |end""".stripMargin)

    val List(classC)  = cpg.typeDecl.name("C").l
    val List(aMember) = classC.member.name("@a").l
    val List(bMember) = classC.member.name("@b").l

    aMember.code shouldBe bMember.code
    aMember.lineNumber shouldBe bMember.lineNumber
  }

  "`attr_writer :a` is represented by a `@a` MEMBER node" in {
    val cpg = code("""
                     |class C
                     | attr_writer :a
                     |end
                     |""".stripMargin)

    val List(classC)  = cpg.typeDecl.name("C").l
    val List(aMember) = classC.member.name("@a").l

    aMember.code shouldBe "attr_writer :a"
    aMember.lineNumber shouldBe Some(3)
  }

  "`attr_writer :a` creates an 'a=' METHOD node" in {
    val cpg = code("""
                     |class C
                     | attr_writer :a
                     |end
                     |""".stripMargin)

    val List(classC)  = cpg.typeDecl.name("C").l
    val List(methodA) = classC.method.name("a=").l

    methodA.code shouldBe "def a= (...)"
    methodA.lineNumber shouldBe Some(3)
    methodA.fullName shouldBe s"Test0.rb:$Main.C.a="

    // TODO: there's probably a better way for testing this
    val List(_, param)                   = methodA.parameter.l
    val List(assignment)                 = methodA.assignment.l
    val List(lhs: Call, rhs: Identifier) = assignment.argument.l: @unchecked

    param.name shouldBe rhs.name
    lhs.name shouldBe Operators.fieldAccess
    lhs.code shouldBe "self.@a"

    val List(aMember) = classC.member.nameExact("a=").l
    aMember.dynamicTypeHintFullName should contain("Test0.rb:<main>.C.a=")
  }

  "`attr_accessor :a` is represented by a `@a` MEMBER node" in {
    val cpg = code("""
                     |class C
                     | attr_accessor :a
                     |end
                     |""".stripMargin)

    val List(classC)  = cpg.typeDecl.name("C").l
    val List(aMember) = classC.member.name("@a").l

    aMember.code shouldBe "attr_accessor :a"
    aMember.lineNumber shouldBe Some(3)
  }

  "`def f(x) ... end` is represented by a METHOD inside the TYPE_DECL node" in {
    val cpg = code("""
                     |class C
                     | def f(x)
                     |   x + 1
                     | end
                     |end
                     |""".stripMargin)

    val List(classC)  = cpg.typeDecl.name("C").l
    val List(methodF) = classC.method.name("f").l

    methodF.fullName shouldBe s"Test0.rb:$Main.C.f"

    val List(memberF) = classC.member.nameExact("f").l
    memberF.dynamicTypeHintFullName.toSet should contain(methodF.fullName)
  }

  "`M.method` in a module `M` should have a method bound to a member under the module's singleton type declaration" in {
    val cpg = code("""
        |module M
        | def M.method(x)
        |   x
        | end
        |end
        |def main(p)
        | M::method(p)
        |end
        |""".stripMargin)

    // Obtain the nodes first
    val regularTypeDecl   = cpg.typeDecl.nameExact("M").head
    val singletonTypeDecl = cpg.typeDecl.nameExact("M<class>").head
    val method            = regularTypeDecl.method.nameExact("method").head
    val methodTypeDecl    = cpg.typeDecl.fullNameExact(method.fullName).head
    val methodMember      = singletonTypeDecl.member.nameExact("method").head
    // Now determine the properties and potential edges
    methodMember.dynamicTypeHintFullName.toSet should contain(method.fullName)
    methodTypeDecl.methodBinding.flatMap(_.boundMethod).head shouldBe method
  }

  "a method in a nested module should have the nested module's member-type and nested types' method" in {
    val cpg = code("""
        |module MMM
        | module Nested
        |   def self.method(x)
        |     x
        |   end
        | end
        |end
        |def outer(aaa)
        | MMM::Nested::method(aaa)
        |end
        |""".stripMargin)

    val nestedTypeDecl       = cpg.typeDecl("Nested").head
    val nestedSingleton      = cpg.typeDecl("Nested<class>").head
    val nestedTypeDeclMember = cpg.member("Nested").head
    val singletonTypeDecl    = cpg.typeDecl.nameExact("Nested<class>").head

    val method         = nestedTypeDecl.method.nameExact("method").head
    val methodTypeDecl = cpg.typeDecl.fullNameExact(method.fullName).head
    val methodMember   = singletonTypeDecl.member.nameExact("method").head

    nestedTypeDeclMember.typeDecl.name shouldBe "MMM<class>"
    nestedTypeDeclMember.dynamicTypeHintFullName.toSet should contain(nestedSingleton.fullName)

    singletonTypeDecl.astParent.asInstanceOf[TypeDecl].name shouldBe "MMM"

    methodMember.dynamicTypeHintFullName.toSet should contain(method.fullName)
    methodTypeDecl.methodBinding.flatMap(_.boundMethod).head shouldBe method
  }

  "`def initialize() ... end` directly inside a class has the constructor modifier" in {
    val cpg = code("""
                     |class C
                     | def initialize()
                     | end
                     |end
                     |""".stripMargin)

    val List(classC)     = cpg.typeDecl.name("C").l
    val List(methodInit) = classC.method.name(RubyDefines.Initialize).l

    methodInit.fullName shouldBe s"Test0.rb:$Main.C.${RubyDefines.Initialize}"
    methodInit.isConstructor.isEmpty shouldBe false
  }

  "`class C end` has default constructor" in {
    val cpg = code("""
                     |class C
                     |end
                     |""".stripMargin)

    val List(classC)     = cpg.typeDecl.name("C").l
    val List(methodInit) = classC.method.name(RubyDefines.Initialize).l

    methodInit.fullName shouldBe s"Test0.rb:$Main.C.${RubyDefines.Initialize}"
  }

  "only `def initialize() ... end` directly under class has the constructor modifier" in {
    val cpg = code("""
                     |def initialize()
                     |  1
                     |end
                     |
                     |class C
                     | def f()
                     |   2
                     | end
                     |end
                     |
                     |class D
                     | def f()
                     |   def initialize()
                     |     3
                     |   end
                     | end
                     |end
                     |""".stripMargin)

    cpg.method.nameExact(RubyDefines.Initialize).where(_.isConstructor).literal.code.l should be(empty)
  }

  "Constants should be defined under the respective singleton" in {
    val cpg = code("""
        |module MMM
        | MConst = 2
        | module Nested
        |   NConst = 4
        | end
        |end
        |
        |""".stripMargin)

    cpg.member("MConst").typeDecl.fullName.head shouldBe s"Test0.rb:$Main.MMM<class>"
    cpg.member("NConst").typeDecl.fullName.head shouldBe s"Test0.rb:$Main.MMM.Nested<class>"
  }

  "a basic anonymous class" should {
    val cpg = code("""
        |a = Class.new do
        |  def hello
        |    puts "Hello world!"
        |  end
        |end
        |""".stripMargin)

    "generate a type decl with the associated members" in {
      inside(cpg.typeDecl.nameExact("<anon-class-0>").l) {
        case anonClass :: Nil =>
          anonClass.name shouldBe "<anon-class-0>"
          anonClass.fullName shouldBe s"Test0.rb:$Main.<anon-class-0>"
          inside(anonClass.method.l) {
            case hello :: defaultConstructor :: Nil =>
              defaultConstructor.name shouldBe RubyDefines.Initialize
              defaultConstructor.fullName shouldBe s"Test0.rb:$Main.<anon-class-0>.${RubyDefines.Initialize}"

              hello.name shouldBe "hello"
              hello.fullName shouldBe s"Test0.rb:$Main.<anon-class-0>.hello"
            case xs => fail(s"Expected a single method, but got [${xs.map(x => x.label -> x.code).mkString(",")}]")
          }
        case xs => fail(s"Expected a single anonymous class, but got [${xs.map(x => x.label -> x.code).mkString(",")}]")
      }
    }

    "generate an assignment to the variable `a` with the source being a constructor invocation of the class" in {
      inside(cpg.method.isModule.assignment.l) {
        case aAssignment :: tmpAssign :: Nil =>
          aAssignment.target.code shouldBe "a"
          aAssignment.source.code shouldBe "(<tmp-0> = Class.new <anon-class-0> (...)).new"

          tmpAssign.target.code shouldBe "<tmp-0>"
          tmpAssign.source.code shouldBe "self.Class.new <anon-class-0> (...)"
        case xs => fail(s"Expected a single assignment, but got [${xs.map(x => x.label -> x.code).mkString(",")}]")
      }
    }

  }

  "a basic singleton class extending an object instance" should {
    val cpg = code("""class Animal; end
        |animal = Animal.new
        |
        |class << animal
        |  def bark
        |    'Woof'
        |  end
        |
        |  def legs
        |     4
        |  end
        |end
        |
        |animal.bark # => 'Woof'
        |""".stripMargin)

    "Create assignments to method refs for methods on singleton object" in {
      inside(cpg.method.isModule.block.assignment.l) {
        case _ :: _ :: _ :: barkAssignment :: legsAssignment :: Nil =>
          inside(barkAssignment.argument.l) {
            case (lhs: Call) :: (rhs: TypeRef) :: Nil =>
              val List(identifier, fieldIdentifier) = lhs.argument.l: @unchecked
              identifier.code shouldBe "animal"
              fieldIdentifier.code shouldBe "bark"

              rhs.typeFullName shouldBe s"Test0.rb:$Main.class<<animal.bark"
            case xs => fail(s"Expected two arguments for assignment, got [${xs.code.mkString(",")}]")
          }

          inside(legsAssignment.argument.l) {
            case (lhs: Call) :: (rhs: TypeRef) :: Nil =>
              val List(identifier, fieldIdentifier) = lhs.argument.l: @unchecked
              identifier.code shouldBe "animal"
              fieldIdentifier.code shouldBe "legs"

              rhs.typeFullName shouldBe s"Test0.rb:$Main.class<<animal.legs"
            case xs => fail(s"Expected two arguments for assignment, got [${xs.code.mkString(",")}]")
          }
        case xs => fail(s"Expected five assignments, got [${xs.code.mkString(",")}]")
      }
    }

    "Create TYPE_DECL nodes for two singleton methods" in {
      inside(cpg.typeDecl.name("(bark|legs)").l) {
        case barkTypeDecl :: legsTypeDecl :: Nil =>
          barkTypeDecl.fullName shouldBe s"Test0.rb:$Main.class<<animal.bark"
          legsTypeDecl.fullName shouldBe s"Test0.rb:$Main.class<<animal.legs"
        case xs => fail(s"Expected two type_decls, got [${xs.code.mkString(",")}]")
      }
    }
  }

  "if: <val> as function param" should {
    "Be treated as an SimpleIdentifier" in {
      val cpg = code("""
          | class User < ApplicationRecord
          |   validates :password, presence: true,
          |                        confirmation: true,
          |                        length: {within: 6..40},
          |                        on: :create,
          |                        if: :password
          |  end
          |""".stripMargin)
      inside(cpg.typeDecl.name("User").l) {
        case userType :: Nil =>
          inside(userType.method.name(RubyDefines.TypeDeclBody).l) {
            case constructor :: Nil =>
              inside(constructor.astChildren.isBlock.l) {
                case methodBlock :: Nil =>
                  val List(validateCall: Call) = methodBlock.astChildren.isCall.l: @unchecked

                  inside(validateCall.argument.l) {
                    case (identArg: Identifier) :: (passwordArg: Literal) :: (presenceArg: Literal) :: (confirmationArg: Literal) :: (_: Block) :: (onArg: Literal) :: (ifArg: Literal) :: Nil =>
                      passwordArg.code shouldBe ":password"
                      presenceArg.code shouldBe "true"
                      confirmationArg.code shouldBe "true"
                      onArg.code shouldBe ":create"
                      ifArg.code shouldBe ":password"
                    case xs => fail(s"Expected 7 arguments, got ${xs.code.mkString(", ")} instead")
                  }
                case xs => fail(s"Expected one block for method body, got ${xs.code.mkString(", ")} instead")
              }
            case xs => fail(s"Expected one constructor method, got ${xs.name.mkString(", ")} instead")
          }
        case _ => fail("Expected typeDecl for user, none found instead")
      }
    }

    "Be treated as a SimpleIdentifier 2" in {
      val cpg = code("""
          | class AdminController < ApplicationController
          |   before_action :administrative, if: :admin_param, except: [:get_user]
          |     skip_before_action :has_info
          |     layout false, only: [:get_all_users, :get_user]
          | end
          |""".stripMargin)

      inside(cpg.typeDecl.name("AdminController").l) {
        case adminTypeDecl :: Nil =>
          inside(adminTypeDecl.method.name(RubyDefines.TypeDeclBody).l) {
            case constructor :: Nil =>
              inside(constructor.astChildren.isBlock.l) {
                case methodBlock :: Nil =>
                  inside(methodBlock.astChildren.isCall.l) {
                    case beforeActionCall :: skipBeforeActionCall :: layoutCall :: Nil =>
                      inside(beforeActionCall.argument.l) {
                        case identArg :: adminArg :: ifArg :: exceptArg :: Nil =>
                          adminArg.code shouldBe ":administrative"
                          ifArg.code shouldBe ":admin_param"
                          exceptArg.code shouldBe "[:get_user]"
                        case xs => fail(s"Expected 4 args, instead found ${xs.code.mkString(", ")}")
                      }
                    case xs => fail(s"Expected 3 calls, instead found ${xs.code.mkString(", ")}")
                  }
                case xs => fail(s"Expected one block for method body, got ${xs.code.mkString(", ")} instead")
              }
            case xs => fail(s"Expected one constructor method, got ${xs.name.mkString(", ")} instead")
          }

        case _ => fail("Expected one typeDecl for AdminController")
      }
    }
  }

  "base types names extending a class in the definition" should {

    val cpg = code("""require "rails/all"
        |
        |module Bar
        | module Baz
        |   class Boz
        |   end
        | end
        |end
        |
        |module Railsgoat
        |  class Application < Rails::Application
        |  end
        |
        |  class Foo < Bar::Baz::Boz
        |  end
        |end
        |""".stripMargin)

    "handle a qualified base type from an external type correctly" in {
      inside(cpg.typeDecl("Application").headOption) {
        case Some(app) =>
          app.inheritsFromTypeFullName.head shouldBe "Rails.Application"
        case None => fail("Expected a type decl for 'Application', instead got nothing")
      }
    }

    "handle a deeply qualified internal base type correctly" in {
      inside(cpg.typeDecl("Foo").headOption) {
        case Some(app) =>
          app.inheritsFromTypeFullName.head shouldBe "Bar.Baz.Boz"
        case None => fail("Expected a type decl for 'Foo', instead got nothing")
      }
    }
  }

  "Instance variables in a class and method defs" should {
    val cpg = code("""
        |class Foo
        | @a
        |
        | def foo
        |   @b = 10
        | end
        |
        | def foobar
        |   @c = 20
        |   @d = 40
        | end
        |
        | def barfoo
        |   puts @a
        |   puts @c
        |   @o = "a"
        | end
        |end
        |""".stripMargin)

    "create respective member nodes" in {
      inside(cpg.typeDecl.name("Foo").l) {
        case fooType :: Nil =>
          inside(fooType.member.name("@.*").l) {
            case aMember :: bMember :: cMember :: dMember :: oMember :: Nil =>
              // Test that all members in class are present
              aMember.code shouldBe "@a"
              bMember.code shouldBe "@b"
              cMember.code shouldBe "@c"
              dMember.code shouldBe "@d"
              oMember.code shouldBe "@o"
            case _ => fail("Expected 5 members")
          }
        case xs => fail(s"Expected TypeDecl for Foo, instead got ${xs.name.mkString(", ")}")
      }
    }

    "create nil assignments under the class initializer" in {
      inside(cpg.typeDecl.name("Foo").l) {
        case fooType :: Nil =>
          inside(fooType.method.name(RubyDefines.TypeDeclBody).l) {
            case initMethod :: Nil =>
              inside(initMethod.block.astChildren.isCall.name(Operators.assignment).l) {
                case aAssignment :: bAssignment :: cAssignment :: dAssignment :: oAssignment :: Nil =>
                  aAssignment.code shouldBe "@a = nil"

                  bAssignment.code shouldBe "@b = nil"
                  cAssignment.code shouldBe "@c = nil"
                  dAssignment.code shouldBe "@d = nil"
                  oAssignment.code shouldBe "@o = nil"

                  inside(aAssignment.argument.l) {
                    case (lhs: Call) :: (rhs: Literal) :: Nil =>
                      lhs.code shouldBe s"${RubyDefines.Self}.@a"
                      lhs.methodFullName shouldBe Operators.fieldAccess

                      inside(lhs.argument.l) {
                        case (identifier: Identifier) :: (fieldIdentifier: FieldIdentifier) :: Nil =>
                          identifier.code shouldBe RubyDefines.Self
                          fieldIdentifier.code shouldBe "@a"
                        case _ => fail("Expected identifier and fieldIdentifier for fieldAccess")
                      }

                      rhs.code shouldBe "nil"
                    case _ => fail("Expected only LHS and RHS for assignment call")
                  }
                case xs => fail(s"Expected assignments, got [${xs.code.mkString}]")
              }
            case xs => fail(s"Expected one method for init, instead got ${xs.name.mkString(", ")}")
          }
        case xs => fail(s"Expected TypeDecl for Foo, instead got ${xs.name.mkString(", ")}")
      }
    }

    "call the body method" in {
      inside(cpg.call.nameExact(RubyDefines.TypeDeclBody).headOption) {
        case Some(bodyCall) =>
          bodyCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
          bodyCall.methodFullName shouldBe s"Test0.rb:$Main.Foo.${RubyDefines.TypeDeclBody}"
          bodyCall.code shouldBe "(<tmp-0> = self::Foo)::<body>()"
          bodyCall.receiver.isEmpty shouldBe true
          bodyCall.argument(0).code shouldBe "<tmp-0>"
        case None => fail("Expected <body> call")
      }
    }
  }

  "Class Variables in Class and Methods" should {
    val cpg = code("""
        |class Foo
        | @@a
        |
        | def foo
        |   @@b = 10
        | end
        |
        | def foobar
        |   @@c = 20
        |   @@d = 40
        | end
        |
        | def barfoo
        |   puts @@a
        |   puts @@c
        |   @@o = "a"
        | end
        |end
        |""".stripMargin)

    "create respective member nodes" in {
      inside(cpg.typeDecl.nameExact("Foo<class>").l) {
        case fooType :: Nil =>
          inside(fooType.member.name("@.*").l) {
            case aMember :: bMember :: cMember :: dMember :: oMember :: Nil =>
              // Test that all members in class are present
              aMember.code shouldBe "@@a"
              bMember.code shouldBe "@@b"
              cMember.code shouldBe "@@c"
              dMember.code shouldBe "@@d"
              oMember.code shouldBe "@@o"
            case xs => fail(s"Expected 5 members, instead got ${xs.size}: [${xs.code.mkString(",")}]")
          }
        case xs => fail(s"Expected TypeDecl for Foo, instead got ${xs.name.mkString(", ")}")
      }
    }

    "create nil assignments under the class initializer" in {
      inside(cpg.typeDecl.name("Foo").l) {
        case fooType :: Nil =>
          inside(fooType.method.name(RubyDefines.TypeDeclBody).l) {
            case clinitMethod :: Nil =>
              inside(clinitMethod.block.astChildren.isCall.name(Operators.assignment).l) {
                case aAssignment :: bAssignment :: cAssignment :: dAssignment :: oAssignment :: Nil =>
                  aAssignment.code shouldBe "@@a = nil"
                  bAssignment.code shouldBe "@@b = nil"
                  cAssignment.code shouldBe "@@c = nil"
                  dAssignment.code shouldBe "@@d = nil"
                  oAssignment.code shouldBe "@@o = nil"

                  inside(aAssignment.argument.l) {
                    case (lhs: Call) :: (rhs: Literal) :: Nil =>
                      lhs.code shouldBe s"${RubyDefines.Self}.@@a"
                      lhs.methodFullName shouldBe Operators.fieldAccess

                      inside(lhs.argument.l) {
                        case (identifier: Identifier) :: (fieldIdentifier: FieldIdentifier) :: Nil =>
                          identifier.code shouldBe RubyDefines.Self
                          fieldIdentifier.code shouldBe "@@a"
                        case _ => fail("Expected identifier and fieldIdentifier for fieldAccess")
                      }

                      rhs.code shouldBe "nil"
                    case _ => fail("Expected only LHS and RHS for assignment call")
                  }
                case xs =>
                  fail(s"Expected 5 fields initializers, got ${xs.size} instead ${xs.code.mkString(", ")}")
              }
            case xs => fail(s"Expected one method for <body>, instead got ${xs.name.mkString(", ")}")
          }
        case xs => fail(s"Expected TypeDecl for Foo, instead got ${xs.name.mkString(", ")}")
      }
    }
  }

  "Bodies that aren't StatementList" should {
    val cpg = code("""
        |  class EventWebhook
        |    ERRORS = [CustomErrorA, CustomErrorB]
        |
        |    def verify_signature(public_key, payload, signature, timestamp)
        |      verify_engine
        |      timestamped_payload = "#{timestamp}#{payload}"
        |      payload_digest = Digest::SHA256.digest(timestamped_payload)
        |      decoded_signature = Base64.decode64(signature)
        |      public_key.dsa_verify_asn1(payload_digest, decoded_signature)
        |    rescue *ERRORS => splat_errors
        |      false
        |    rescue StandardError => some_variable
        |      false
        |    end
        |  end
        |""".stripMargin)

    "successfully parse and create the method" in {
      cpg.method.nameExact("verify_signature").nonEmpty shouldBe true
    }

    "create the `StandardError` local variable" in {
      cpg.local.nameExact("some_variable").dynamicTypeHintFullName.toList shouldBe List(
        s"${GlobalTypes.builtinPrefix}.StandardError"
      )
    }

    "create the splatted error local variable" in {
      cpg.local.nameExact("splat_errors").size shouldBe 1
    }
  }

  "Scope call under TYPE DECL" should {
    val cpg = code("""
        |class Foo
        | scope :published, -> { where(status: "Published") }
        |  def bar
        |    puts 1
        |  end
        |end
        |""".stripMargin)

    "be moved to <init> constructor method" in {
      inside(cpg.typeDecl.name("Foo").l) {
        case fooClass :: Nil =>
          inside(fooClass.method.name(RubyDefines.TypeDeclBody).l) {
            case initMethod :: Nil =>
              inside(initMethod.astChildren.isBlock.astChildren.isCall.l) {
                case scopeCall :: Nil =>
                  scopeCall.code shouldBe "scope :published, -> { where(status: \"Published\") }"

                  inside(scopeCall.argument.l) {
                    case (self: Identifier) :: (literalArg: Literal) :: unknownArg :: Nil =>
                      self.code shouldBe "self"
                      literalArg.code shouldBe ":published"
                    case xs => fail(s"Expected three arguments, got ${xs.code.mkString(", ")} instead")
                  }
                case xs => fail(s"Expected one call under constructor, got ${xs.code.mkString(", ")} instead")
              }
            case xs => fail(s"Expected one init method, got ${xs.code.mkString(", ")} instead")
          }
        case xs => fail(s"Expected one class, got ${xs.code.mkString(", ")} instead")
      }
    }
  }

  "Scope call with Lambda Expression" should {
    val cpg = code("""
        |class Foo
        |  scope :hits_by_ip, ->(ip, col = "*") { select("#{col}").where(ip_address: ip).order("id DESC") }
        |  def bar
        |    puts 1
        |  end
        |end
        |""".stripMargin)

    "correct method full name for method ref under call" in {
      inside(cpg.typeDecl.name("Foo").l) {
        case fooClass :: Nil =>
          inside(fooClass.method.name(RubyDefines.TypeDeclBody).l) {
            case initMethod :: Nil =>
              initMethod.code shouldBe "def <body>; (...); end"
              inside(initMethod.astChildren.isBlock.l) {
                case methodBlock :: Nil =>
                  inside(methodBlock.astChildren.l) {
                    case methodCall :: Nil =>
                      inside(methodCall.astChildren.l) {
                        case (base: Call) :: (self: Identifier) :: (literal: Literal) :: (typeRef: TypeRef) :: Nil =>
                          base.code shouldBe "self.scope"
                          self.name shouldBe "self"
                          literal.code shouldBe ":hits_by_ip"
                          typeRef.typeFullName shouldBe s"Test0.rb:$Main.Foo.${RubyDefines.TypeDeclBody}.<lambda>0&Proc"
                          cpg.method
                            .fullNameExact(
                              typeRef.typ.referencedTypeDecl.member.name("call").dynamicTypeHintFullName.toSeq*
                            )
                            .parameter
                            .indexGt(0)
                            .name
                            .l shouldBe List("ip", "col")
                        case xs => fail(s"Expected three children, got ${xs.code.mkString(", ")} instead")
                      }
                    case xs => fail(s"Expected one call, got ${xs.code.mkString(", ")} instead")
                  }
                case xs => fail(s"Expected one block under method, got ${xs.code.mkString(", ")} instead")
              }
            case xs => fail(s"Expected one init method, got ${xs.code.mkString(", ")} instead")
          }
        case xs => fail(s"Expected one class, got ${xs.code.mkString(", ")} instead")
      }
    }
  }

  // TODO: Fix when implementing calls vs field accesses, currently handled as a MemberAccess where the target becomes "Encoding"
  //  which is resolved as `<__builtin.Encoding>`, and then adds the `Converter` as a function call, so type ends up being
  //  `<__builtin.Encoding>:Converter`
  "GlobalTypes::BundledClasses" ignore {
    val cpg = code("""
        |a = Encoding::Converter.asciicompat_encoding("abc")
        |""".stripMargin)

    "resolve call type" in {
      inside(cpg.call.nameExact(Operators.assignment).l) {
        case assignCall :: Nil =>
          inside(assignCall.argument.l) {
            case lhs :: (rhs: Call) :: Nil =>
              rhs.typeFullName shouldBe "__builtin.Encoding.Converter.asciicompat_encoding"
            case xs => fail(s"Expected lhs and rhs for assignment call, got [${xs.code.mkString(",")}]")
          }
        case xs => fail(s"Expected one call for assignment, got [${xs.code.mkString(",")}]")
      }
    }
  }

  "Class definition on one line" should {
    val cpg = code("""
        |class X 1 end
        |""".stripMargin)

    "create TYPE_DECL" in {
      inside(cpg.typeDecl.name("X").l) {
        case xClass :: Nil =>
          inside(xClass.astChildren.isMethod.l) {
            case bodyMethod :: initMethod :: Nil =>
              inside(bodyMethod.block.astChildren.l) {
                case (literal: Literal) :: Nil =>
                  literal.code shouldBe "1"
                case xs => fail(s"Expected literal for body method, got [${xs.code.mkString(",")}]")
              }
            case xs => fail(s"Expected body and init method, got [${xs.code.mkString(",")}]")
          }
        case xs => fail(s"Expected one class, got [${xs.code.mkString(",")}]")
      }
    }
  }

  "A call to super" should {
    val cpg = code("""
        |class A
        |  def foo(a)
        |  end
        |end
        |class B < A
        |  def foo(a)
        |    super(a)
        |  end
        |end
        |""".stripMargin)

    "create a simple call" in {
      val superCall = cpg.call.nameExact("super").head
      superCall.code shouldBe "super(a)"
      superCall.name shouldBe "super"
      superCall.methodFullName shouldBe Defines.DynamicCallUnknownFullName
    }
  }

  "a class that is redefined should have a counter suffixed to ensure uniqueness" in {
    val cpg = code("""
        |class Foo
        | def foo;end
        |end
        |class Bar;end
        |class Foo
        | def foo;end
        |end
        |class Foo;end
        |""".stripMargin)

    cpg.typeDecl.name("(Foo|Bar).*").filterNot(_.name.endsWith("<class>")).name.l shouldBe List(
      "Foo",
      "Bar",
      "Foo",
      "Foo"
    )
    cpg.typeDecl.name("(Foo|Bar).*").filterNot(_.name.endsWith("<class>")).fullName.l shouldBe List(
      s"Test0.rb:$Main.Foo",
      s"Test0.rb:$Main.Bar",
      s"Test0.rb:$Main.Foo0",
      s"Test0.rb:$Main.Foo1"
    )

    cpg.method.nameExact("foo").fullName.l shouldBe List(s"Test0.rb:$Main.Foo.foo", s"Test0.rb:$Main.Foo0.foo")

  }

  "Class with nonAllowedTypeDeclChildren and explicit init" should {
    val cpg = code("""
        |class Foo
        | 1
        | def initialize(bar)
        |   puts bar
        | end
        |end
        |""".stripMargin)

    "have an explicit init method" in {
      inside(cpg.typeDecl.nameExact("Foo").method.l) {
        case bodyMethod :: initMethod :: Nil =>
          bodyMethod.name shouldBe TypeDeclBody

          initMethod.name shouldBe Initialize
          inside(initMethod.parameter.l) {
            case selfParam :: barParam :: Nil =>
              selfParam.name shouldBe "self"
              barParam.name shouldBe "bar"
            case xs => fail(s"Expected two params, got [${xs.code.mkString(",")}]")
          }

          inside(initMethod.block.astChildren.l) {
            case (putsCall: Call) :: Nil =>
              putsCall.name shouldBe "puts"
            case xs => fail(s"Expected one call, got [${xs.code.mkString(",")}]")
          }

          inside(bodyMethod.block.astChildren.l) {
            case (one: Literal) :: Nil =>
              one.code shouldBe "1"
              one.typeFullName shouldBe s"${GlobalTypes.kernelPrefix}.Integer"
            case xs => fail(s"Expected one literal, got [${xs.code.mkString(",")}]")
          }

        case xs => fail(s"Expected body method and init method, got [${xs.code.mkString(",")}]")
      }
    }
  }

  "Class defined in Namespace" in {
    val cpg = code("""
        |class Api::V1::MobileController
        |end
        |""".stripMargin)

    inside(cpg.namespaceBlock.fullNameExact("Api.V1").typeDecl.l) {
      case mobileNamespace :: mobileClassNamespace :: Nil =>
        mobileNamespace.name shouldBe "MobileController"
        mobileNamespace.fullName shouldBe "Test0.rb:<main>.Api.V1.MobileController"

        mobileClassNamespace.name shouldBe "MobileController<class>"
        mobileClassNamespace.fullName shouldBe "Test0.rb:<main>.Api.V1.MobileController<class>"
      case xs => fail(s"Expected two namespace blocks, got ${xs.code.mkString(",")}")
    }

    inside(cpg.typeDecl.name("MobileController").l) {
      case mobileTypeDecl :: Nil =>
        mobileTypeDecl.name shouldBe "MobileController"
        mobileTypeDecl.fullName shouldBe "Test0.rb:<main>.Api.V1.MobileController"
        mobileTypeDecl.astParentFullName shouldBe "Api.V1"
        mobileTypeDecl.astParentType shouldBe NodeTypes.NAMESPACE_BLOCK

        mobileTypeDecl.astParent.isNamespaceBlock shouldBe true

        val namespaceDecl = mobileTypeDecl.astParent.asInstanceOf[NamespaceBlock]
        namespaceDecl.name shouldBe "Api.V1"
        namespaceDecl.filename shouldBe "Test0.rb"

        namespaceDecl.astParent.isFile shouldBe true
        val parentFileDecl = namespaceDecl.astParent.asInstanceOf[File]
        parentFileDecl.name shouldBe "Test0.rb"

      case xs => fail(s"Expected one class decl, got [${xs.code.mkString(",")}]")
    }
  }

  "Namespace scope is popping properly" in {
    val cpg = code("""
        |class Foo::Bar
        |end
        |
        |class Baz
        |end
        |""".stripMargin)

    inside(cpg.typeDecl.name("Baz").l) {
      case bazTypeDecl :: Nil =>
        bazTypeDecl.fullName shouldBe "Test0.rb:<main>.Baz"
      case xs => fail(s"Expected one type decl, got [${xs.code.mkString(",")}]")
    }
  }

  "Self param in static method" in {
    val cpg = code("""
        |class Benefits < ApplicationRecord
        |def self.save(file, backup = false)
        |    data_path = Rails.root.join("public", "data")
        |    full_file_name = "#{data_path}/#{file.original_filename}"
        |    f = File.open(full_file_name, "wb+")
        |    f.write file.read
        |    f.close
        |    make_backup(file, data_path, full_file_name) if backup == "true"
        |end
        |end
        |""".stripMargin)
  }

  "Splat Field Declaration" in {
    val cpg = code("""
        |  class EpisodeRssItem
        |    FOUND = %i[title itunes_subtitle].freeze
        |    attr_reader(*FOUND)
        |    attr_reader(*NOT_FOUND)
        |   end
        |
        |""".stripMargin)

    val List(titleMethod)  = cpg.method.name("title").l
    val List(itunesMethod) = cpg.method.name("itunes_subtitle").l
    val List(bodyMethod)   = cpg.method.name("<body>").l

    inside(titleMethod.methodReturn.toReturn.l) {
      case methodReturn :: Nil =>
        methodReturn.code shouldBe "@title"
      case xs => fail(s"Expected one return, got [${xs.code.mkString(",")}]")
    }

    inside(itunesMethod.methodReturn.toReturn.l) {
      case methodReturn :: Nil =>
        methodReturn.code shouldBe "@itunes_subtitle"
      case xs => fail(s"Expected one return, got [${xs.code.mkString(",")}]")
    }

    inside(bodyMethod.call.name("attr_reader").l) {
      case notFoundCall :: Nil =>
        notFoundCall.code shouldBe "attr_reader(*NOT_FOUND)"
        inside(notFoundCall.argument.l) {
          case _ :: splatArg :: Nil =>
            splatArg.code shouldBe "*NOT_FOUND"
          case xs => fail(s"Expected two args, got ${xs.size}: [${xs.code.mkString(",")}]")
        }
      case xs => fail(s"Expected one call, got [${xs.code.mkString(",")}]")
    }
  }

  "Unknown Splat Field Declaration" in {
    val cpg = code("""
                     |  class EpisodeRssItem
                     |    attr_reader(*NOT_FOUND)
                     |   end
                     |""".stripMargin)

    val List(bodyMethod) = cpg.method.name("<body>").l

    inside(bodyMethod.call.name("attr_reader").l) {
      case notFoundCall :: Nil =>
        notFoundCall.code shouldBe "attr_reader(*NOT_FOUND)"
        inside(notFoundCall.argument.l) {
          case _ :: splatArg :: Nil =>
            splatArg.code shouldBe "*NOT_FOUND"
          case xs => fail(s"Expected two args, got ${xs.size}: [${xs.code.mkString(",")}]")
        }
      case xs => fail(s"Expected one call, got [${xs.code.mkString(",")}]")
    }
  }
}
