package io.joern.c2cpg.passes.types

import io.joern.c2cpg.parser.FileDefaults
import io.joern.c2cpg.testfixtures.CCodeToCpgSuite
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.codepropertygraph.generated.nodes.FieldIdentifier
import io.shiftleft.codepropertygraph.generated.nodes.Identifier
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

class EnumTypeTests extends CCodeToCpgSuite(fileSuffix = FileDefaults.CPP_EXT) {

  "Enums" should {

    "be correct for simple enum" in {
      val cpg = code("""
        |enum color
        |{
        |    red,
        |    yellow,
        |    green = 20,
        |    blue
        |};""".stripMargin)
      inside(cpg.typeDecl.nameNot(NamespaceTraversal.globalNamespaceName).internal.l) { case List(color) =>
        color.name shouldBe "color"
        color.code should startWith("enum color")
        inside(color.member.l) { case List(red, yellow, green, blue) =>
          red.name shouldBe "red"
          yellow.name shouldBe "yellow"
          green.name shouldBe "green"
          blue.name shouldBe "blue"
        }
        inside(color.astChildren.isMethod.l) { case List(clinit) =>
          clinit.name shouldBe io.joern.x2cpg.Defines.StaticInitMethodName
          clinit.fullName shouldBe s"color:${io.joern.x2cpg.Defines.StaticInitMethodName}"
          clinit.lineNumber shouldBe Some(2)
          clinit.columnNumber shouldBe Some(1)
          clinit.filename shouldBe "Test0.cpp"
          inside(clinit.ast.isCall.l) { case List(greenInit) =>
            greenInit.code shouldBe "green = 20"
          }
        }
      }
    }

    "be correct for simple enum typedef" in {
      val cpg = code("""
        |typedef enum color
        |{
        |    red,
        |    yellow,
        |    green = 20,
        |    blue
        |} C;""".stripMargin)
      inside(cpg.typeDecl.nameNot(NamespaceTraversal.globalNamespaceName).filter(x => !x.isExternal).l) {
        case List(color, c) =>
          color.name shouldBe "color"
          color.code should startWith("typedef enum color")
          color.aliasTypeFullName shouldBe Some("C")
          c.name shouldBe "C"
          c.aliasTypeFullName shouldBe Some("color")
          inside(color.astChildren.isMember.l) { case List(red, yellow, green, blue) =>
            red.name shouldBe "red"
            yellow.name shouldBe "yellow"
            green.name shouldBe "green"
            blue.name shouldBe "blue"
          }
          inside(color.astChildren.isMethod.l) { case List(clinit) =>
            clinit.name shouldBe io.joern.x2cpg.Defines.StaticInitMethodName
            clinit.fullName shouldBe s"color:${io.joern.x2cpg.Defines.StaticInitMethodName}"
            inside(clinit.ast.isCall.l) { case List(greenInit) =>
              greenInit.code shouldBe "green = 20"
            }
          }
      }
    }

    "be correct for simple enum class" in {
      val cpg = code("""
        |enum class altitude: char
        |{ 
        |     high='h',
        |     low='l', // C++11 allows the extra comma
        |}; """.stripMargin)
      inside(cpg.typeDecl.nameNot(NamespaceTraversal.globalNamespaceName).internal.l) { case List(altitude) =>
        altitude.name shouldBe "altitude"
        altitude.code should startWith("enum class altitude")
        inside(altitude.member.l) { case List(high, low) =>
          high.name shouldBe "high"
          high.typeFullName shouldBe "char"
          low.name shouldBe "low"
          low.typeFullName shouldBe "char"
        }
        inside(altitude.astChildren.isMethod.l) { case List(clinit) =>
          clinit.name shouldBe io.joern.x2cpg.Defines.StaticInitMethodName
          clinit.fullName shouldBe s"altitude:${io.joern.x2cpg.Defines.StaticInitMethodName}"
          inside(clinit.ast.isCall.code.l) { case List(highInit, lowInit) =>
            highInit shouldBe "high='h'"
            lowInit shouldBe "low='l'"
          }
        }
      }
    }

    "be correct for simple enum with type" in {
      val cpg = code("""
        |enum smallenum: int
        |{
        |    a,
        |    b,
        |    c
        |};""".stripMargin)
      inside(cpg.typeDecl.nameNot(NamespaceTraversal.globalNamespaceName).internal.l) { case List(smallenum) =>
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

    "be correct for anonymous enum" in {
      val cpg = code("""
         |enum
         |{
         |    d,
         |    e,
         |    f
         |};""".stripMargin)
      inside(cpg.typeDecl.nameNot(NamespaceTraversal.globalNamespaceName).internal.l) { case List(anon) =>
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

    "be correct for anonymous enum with alias" in {
      val cpg = code("""
          |enum
          |{
          |    d,
          |    e,
          |    f
          |} testing;""".stripMargin)
      val List(anon) = cpg.typeDecl.name("testing").l
      inside(anon.member.l) { case List(d, e, f) =>
        d.name shouldBe "d"
        e.name shouldBe "e"
        f.name shouldBe "f"
      }
      anon.aliasTypeFullName shouldBe None
    }

    "be correct for enum access" in {
      val cpg = code("""
       |enum X: int
       |{
       |    a,
       |    b
       |};
       |int x = X::a;
       |""".stripMargin)
      inside(cpg.typeDecl.nameNot(NamespaceTraversal.globalNamespaceName).internal.l) { case List(x) =>
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
