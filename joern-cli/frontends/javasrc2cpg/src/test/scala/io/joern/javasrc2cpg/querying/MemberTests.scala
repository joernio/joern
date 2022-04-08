package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCodeToCpgFixture
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, ModifierTypes, Operators, PropertyNames}
import io.shiftleft.codepropertygraph.generated.nodes.{Call, FieldIdentifier, Identifier, Member}
import io.shiftleft.semanticcpg.language._
import org.scalatest.Ignore

class MemberTests extends JavaSrcCodeToCpgFixture {

  override val code: String =
    """
      |class Foo {
      |  int x;
      |}
      |
      |class Bar {
      |    int nonStatic;
      |
      |    static int isStatic = 0;
      |    static int isAlsoStatic;
      |
      |    static Bar isStaticObject = new Bar();
      |
      |    Bar anotherNonStatic = new Bar();
      |
      |    static {
      |        isAlsoStatic = 1;
      |    }
      |
      |    static {
      |        System.out.println("Second static block");
      |    }
      |}
      |""".stripMargin

  private def hasStaticMod(member: Member): Boolean = {
    member.modifier.exists(_.modifierType == ModifierTypes.STATIC)
  }

  "should contain MEMBER node with correct properties" in {
    val List(x) = cpg.member("x").l
    x.name shouldBe "x"
    x.code shouldBe "int x"
    x.typeFullName shouldBe "int"
    x.order shouldBe 1
  }

  "should allow traversing from MEMBER to TYPE_DECL" in {
    val List(x) = cpg.member("x").typeDecl.l
    x.name shouldBe "Foo"
  }

  "should not create a <clinit> method for classes without static members" in {
    cpg.typeDecl.nameExact("Foo").method.nameExact("<clinit>").size shouldBe 0
  }

  "should create correct static and non-static members for class with both" in {
    cpg.typeDecl.name("Bar").member.l match {
      case List(nonStatic, isStatic, isAlsoStatic, isStaticObject, anotherNonStatic) =>
        nonStatic.name shouldBe "nonStatic"
        nonStatic.typeFullName shouldBe "int"
        hasStaticMod(nonStatic) shouldBe false

        isStatic.name shouldBe "isStatic"
        isStatic.typeFullName shouldBe "int"
        hasStaticMod(isStatic) shouldBe true

        isAlsoStatic.name shouldBe "isAlsoStatic"
        isAlsoStatic.typeFullName shouldBe "int"
        hasStaticMod(isAlsoStatic) shouldBe true

        isStaticObject.name shouldBe "isStaticObject"
        isStaticObject.typeFullName shouldBe "Bar"
        hasStaticMod(isStaticObject) shouldBe true

        anotherNonStatic.name shouldBe "anotherNonStatic"
        anotherNonStatic.typeFullName shouldBe "Bar"
        hasStaticMod(anotherNonStatic) shouldBe false

      case res => fail(s"Expected 4 members but found $res")
    }
  }

  "should create a single <clinit> method for classes with static members" in {
    pendingUntilFixed {

      cpg.typeDecl.nameExact("Bar").method.nameExact("<clinit>").size shouldBe 1

      val clinit = cpg.typeDecl.nameExact("Bar").method.nameExact("<clinit>").head
      clinit.signature shouldBe "void()"
      clinit.fullName shouldBe "Bar.<clinit>:void()"

      clinit.body.astChildren.l match {
        case List(
              isStaticInit: Call,
              isStaticObjectAssign: Call,
              isStaticObjectInit: Call,
              isAlsoStaticInit: Call,
              printStmt: Call
            ) =>
          isStaticInit.methodFullName shouldBe Operators.assignment
          isStaticInit.argument.size shouldBe 2
          isStaticInit.argument.headOption match {
            case Some(fieldAccess: Call) =>
              fieldAccess.argument.size shouldBe 2
              fieldAccess.code shouldBe "Bar.isStatic"
              fieldAccess.typeFullName shouldBe "int"
              fieldAccess.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

              val List(identifier: Identifier, field: FieldIdentifier) = fieldAccess.argument.l
              identifier.name shouldBe "Bar"
              identifier.typeFullName shouldBe "Bar"
              identifier.order shouldBe 1
              identifier.argumentIndex shouldBe 1

              field.canonicalName shouldBe "isStatic"
              field.code shouldBe "isStatic"

            case res =>
              fail(s"Expected member field access but got ${res}")
          }

          isStaticObjectInit.methodFullName shouldBe "Bar.<init>:void()"

          isAlsoStaticInit.methodFullName shouldBe Operators.assignment

          printStmt.methodFullName shouldBe "java.io.PrintStream.println:void(java.lang.String)"

        case res => fail(s"Expected 4 calls in <clinit> body but found $res")
      }
    }
  }
}
