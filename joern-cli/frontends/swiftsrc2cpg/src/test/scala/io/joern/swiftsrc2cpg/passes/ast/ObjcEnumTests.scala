

// This test file has been translated from swift/test/Parse/objc_enum.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class ObjcEnumTests extends AbstractPassTest {
  "testObjcEnum1" ignore AstFixture("") { cpg =>
    assertParse(
      """
      @objc enum Foo: Int32 {
        case Zim, Zang, Zung
      }
      """
    )
  }

  "testObjcEnum2" ignore AstFixture("") { cpg =>
    assertParse(
      """
      @objc enum Generic<T>: Int32 {
        case Zim, Zang, Zung
      }
      """
    )
  }

  "testObjcEnum3" ignore AstFixture("") { cpg =>
    assertParse(
      """
      @objc(EnumRuntimeName) enum RuntimeNamed: Int32 {
        case Zim, Zang, Zung
      }
      """
    )
  }

  "testObjcEnum4" ignore AstFixture("") { cpg =>
    assertParse(
      """
      @objc enum NoRawType {
        case Zim, Zang, Zung
      }
      """
    )
  }

  "testObjcEnum5" ignore AstFixture("") { cpg =>
    assertParse(
      """
      @objc enum NonIntegerRawType: Float {
        case Zim = 1.0, Zang = 1.5, Zung = 2.0
      }
      """
    )
  }

  "testObjcEnum6" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum NonObjCEnum: Int {
        case Zim, Zang, Zung
      }
      """
    )
  }

  "testObjcEnum7" ignore AstFixture("") { cpg =>
    assertParse(
      """
      class Bar {
        @objc func foo(x: Foo) {}
        @objc func nonObjC(x: NonObjCEnum) {}
      }
      """
    )
  }

  "testObjcEnum8" ignore AstFixture("") { cpg =>
    assertParse(
      """
      // <rdar://problem/23681566> @objc enums with payloads rejected with no source location info
      @objc enum r23681566 : Int32 {
        case Foo(progress: Int)
      }
      """
    )
  }

}
