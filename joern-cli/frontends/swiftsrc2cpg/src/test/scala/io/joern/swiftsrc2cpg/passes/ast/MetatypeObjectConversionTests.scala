

// This test file has been translated from swift/test/Parse/metatype_object_conversion.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class MetatypeObjectConversionTests extends AbstractPassTest {
  "testMetatypeObjectConversion1" ignore AstFixture("") { cpg =>
    assertParse(
      """
      class C {}
      struct S {}
      """
    )
  }

  "testMetatypeObjectConversion2" ignore AstFixture("") { cpg =>
    assertParse(
      """
      protocol NonClassProto {}
      protocol ClassConstrainedProto : class {}
      """
    )
  }

  "testMetatypeObjectConversion3" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func takesAnyObject(_ x: AnyObject) {}
      """
    )
  }

  "testMetatypeObjectConversion4" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func concreteTypes() {
        takesAnyObject(C.self)
        takesAnyObject(S.self)
        takesAnyObject(ClassConstrainedProto.self)
      }
      """
    )
  }

  "testMetatypeObjectConversion5" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func existentialMetatypes(nonClass: NonClassProto.Type,
                                classConstrained: ClassConstrainedProto.Type,
                                compo: (NonClassProto & ClassConstrainedProto).Type) {
        takesAnyObject(nonClass)
        takesAnyObject(classConstrained)
        takesAnyObject(compo)
      }
      """
    )
  }

}
