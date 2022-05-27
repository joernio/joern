package io.joern.c2cpg

import io.joern.c2cpg.fixtures.CompleteCpgFixture
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.{Call, FieldIdentifier, Identifier}
import io.shiftleft.semanticcpg.language._
import org.scalatest.Inside
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class EnumTests extends AnyWordSpec with Matchers with Inside with CompleteCpgFixture {

  "Enums" should {

    "be correct for simple enum" in CompleteCpgFixture("""
        |enum color
        |{
        |    red,
        |    yellow,
        |    green = 20,
        |    blue
        |};""".stripMargin) { cpg =>
      inside(cpg.typeDecl.nameNot("<global>").internal.l) { case List(color) =>
        color.name shouldBe "color"
        color.code should startWith("enum color")
        inside(color.member.l) { case List(red, yellow, green, blue) =>
          red.name shouldBe "red"
          yellow.name shouldBe "yellow"
          green.name shouldBe "green"
          blue.name shouldBe "blue"
        }
        inside(color.astChildren.isMethod.l) { case List(sinit) =>
          sinit.name shouldBe "<sinit>"
          sinit.fullName shouldBe "color:<sinit>"
          sinit.code shouldBe "green = 20"
          inside(sinit.ast.isCall.l) { case List(greenInit) =>
            greenInit.code shouldBe "green = 20"
          }
        }
      }
    }

    "be correct for simple enum typedef" in CompleteCpgFixture("""
        |typedef enum color
        |{
        |    red,
        |    yellow,
        |    green = 20,
        |    blue
        |} C;""".stripMargin) { cpg =>
      inside(cpg.typeDecl.nameNot("<global>").filter(x => !x.isExternal).l) { case List(color, c) =>
        color.name shouldBe "color"
        color.code should startWith("typedef enum color")
        color.aliasTypeFullName shouldBe None
        c.name shouldBe "C"
        c.aliasTypeFullName shouldBe Some("color")
        inside(color.astChildren.isMember.l) { case List(red, yellow, green, blue) =>
          red.name shouldBe "red"
          yellow.name shouldBe "yellow"
          green.name shouldBe "green"
          blue.name shouldBe "blue"
        }
        inside(color.astChildren.isMethod.l) { case List(sinit) =>
          sinit.name shouldBe "<sinit>"
          sinit.fullName shouldBe "color:<sinit>"
          sinit.code shouldBe "green = 20"
          inside(sinit.ast.isCall.l) { case List(greenInit) =>
            greenInit.code shouldBe "green = 20"
          }
        }
      }
    }

    "be correct for simple enum class" in CompleteCpgFixture("""
        |enum class altitude: char
        |{ 
        |     high='h',
        |     low='l', // C++11 allows the extra comma
        |}; """.stripMargin) { cpg =>
      inside(cpg.typeDecl.nameNot("<global>").internal.l) { case List(altitude) =>
        altitude.name shouldBe "altitude"
        altitude.code should startWith("enum class altitude")
        inside(altitude.member.l) { case List(high, low) =>
          high.name shouldBe "high"
          high.typeFullName shouldBe "char"
          low.name shouldBe "low"
          low.typeFullName shouldBe "char"
        }
        inside(altitude.astChildren.isMethod.l) { case List(sinit) =>
          sinit.name shouldBe "<sinit>"
          sinit.fullName shouldBe "altitude:<sinit>"
          sinit.code shouldBe "high='h',low='l'"
          inside(sinit.ast.isCall.code.l) { case List(highInit, lowInit) =>
            highInit shouldBe "high='h'"
            lowInit shouldBe "low='l'"
          }
        }
      }
    }

    "be correct for simple enum with type" in CompleteCpgFixture("""
        |enum smallenum: int
        |{
        |    a,
        |    b,
        |    c
        |};""".stripMargin) { cpg =>
      inside(cpg.typeDecl.nameNot("<global>").internal.l) { case List(smallenum) =>
        smallenum.name shouldBe "smallenum"
        smallenum.code should startWith("enum smallenum")
        inside(smallenum.member.l) { case List(a, b, c) =>
          a.name shouldBe "a"
          a.typeFullName shouldBe "int"
          b.name shouldBe "b"
          b.typeFullName shouldBe "int"
          c.name shouldBe "c"
          c.typeFullName shouldBe "int"
        }
        smallenum.astChildren.isMethod shouldBe empty
      }
    }

    "be correct for anonymous enum" in CompleteCpgFixture("""
         |enum
         |{
         |    d,
         |    e,
         |    f
         |};""".stripMargin) { cpg =>
      inside(cpg.typeDecl.nameNot("<global>").internal.l) { case List(anon) =>
        anon.name shouldBe "anonymous_enum_0"
        anon.code should startWith("enum")
        inside(anon.member.l) { case List(d, e, f) =>
          d.name shouldBe "d"
          e.name shouldBe "e"
          f.name shouldBe "f"
        }
        anon.astChildren.isMethod shouldBe empty
      }
    }

    "be correct for enum access" in CompleteCpgFixture("""
       |enum X: int
       |{
       |    a,
       |    b
       |};
       |int x = X::a;
       |""".stripMargin) { cpg =>
      inside(cpg.typeDecl.nameNot("<global>").internal.l) { case List(x) =>
        x.name shouldBe "X"
        x.code should startWith("enum X")
        inside(x.member.l) { case List(a, b) =>
          a.name shouldBe "a"
          a.typeFullName shouldBe "int"
          b.name shouldBe "b"
          b.typeFullName shouldBe "int"
        }
        x.astChildren.isMethod shouldBe empty
        inside(cpg.call.l) { case List(assign, ma) =>
          assign.code shouldBe "x = X::a"
          ma.code shouldBe "X::a"
          inside(ma.ast.l) { case List(call: Call, x: Identifier, a: FieldIdentifier) =>
            call.name shouldBe Operators.fieldAccess
            x.order shouldBe 1
            a.order shouldBe 2
          }
        }
      }
    }

  }

}
