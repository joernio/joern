// This test file has been translated from swift/test/Parse/escaped_identifiers.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.SwiftSrc2CpgSuite
import io.shiftleft.semanticcpg.language.*

class EscapedIdentifiersTests extends SwiftSrc2CpgSuite {

  "EscapedIdentifiersTests" should {

    "testEscapedIdentifiers1" in {
      val cpg = code("""
      func `protocol`() {}
      """)
      val List(protocolFunc) = cpg.method.nameExact("`protocol`").l
      protocolFunc.fullName shouldBe "Test0.swift:<global>.`protocol`:()->ANY"
    }

    "testEscapedIdentifiers2" in {
      val cpg = code("""
      `protocol`()
      """)
      val List(protocolCall) = cpg.call.nameExact("`protocol`").l
      protocolCall.code shouldBe "`protocol`()"
    }

    "testEscapedIdentifiers3" in {
      val cpg = code("""
      class `Type` {}
      """)
      val List(typeDecl) = cpg.typeDecl.nameExact("`Type`").l
      typeDecl.fullName shouldBe "Test0.swift:<global>.`Type`"
    }

    "testEscapedIdentifiers4" in {
      val cpg = code("""
      var `class` = `Type`.self
      """)
      val List(globalBlock) = cpg.method.nameExact("<global>").block.l
      val List(classL)      = globalBlock.local.nameExact("`class`").l
      classL.name shouldBe "`class`"
      val List(assign) = cpg.call.nameExact("<operator>.assignment").l
      assign.code shouldBe "var `class` = `Type`.self"
    }

    "testEscapedIdentifiers5" in {
      val cpg = code("""
      func foo() {}
      """)
      val List(fooFunc) = cpg.method.nameExact("foo").l
      fooFunc.fullName shouldBe "Test0.swift:<global>.foo:()->ANY"
    }

    "testEscapedIdentifiers6" in {
      val cpg = code("""
      `foo`()
      """)
      val List(fooCall) = cpg.call.nameExact("`foo`").l
      fooCall.code shouldBe "`foo`()"
    }

    "testEscapedIdentifiers7" in {
      val cpg = code("""
      // Escaping suppresses identifier contextualization.
      var get: (() -> ()) -> () = { $0() }
      """)
      val List(getLocal) = cpg.method.nameExact("<global>").block.local.nameExact("get").l
      getLocal.typeFullName should include("Swift.Function")
      cpg.method.nameExact("<lambda>0").fullName.headOption shouldBe defined
    }

    "testEscapedIdentifiers8" in {
      val cpg = code("""
      var applyGet: Int {
        `get` { }
        return 0
      }
      """)
      val List(applyGetMethod) = cpg.method.nameExact("applyGet").l
      applyGetMethod.fullName should startWith("Test0.swift:<global>.applyGet:")
      applyGetMethod.fullName should endWith("Swift.Int")
      val List(getCall) = applyGetMethod.call.nameExact("`get`").l
      getCall.code shouldBe "`get` { }"
    }

    "testEscapedIdentifiers9" in {
      val cpg = code("""
      enum `switch` {}
      """)
      val List(typeDecl) = cpg.typeDecl.nameExact("`switch`").l
      typeDecl.fullName shouldBe "Test0.swift:<global>.`switch`"
    }

    "testEscapedIdentifiers10" in {
      val cpg = code("""
      typealias `Self` = Int
      """)
      val List(typeDecl) = cpg.typeDecl.nameExact("`Self`").l
      typeDecl.fullName shouldBe "Test0.swift:<global>.`Self`"
      typeDecl.aliasTypeFullName shouldBe Some("Swift.Int")
    }

  }

}
