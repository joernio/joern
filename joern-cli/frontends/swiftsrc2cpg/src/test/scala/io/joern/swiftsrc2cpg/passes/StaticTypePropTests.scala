package io.joern.swiftsrc2cpg.passes

import io.shiftleft.semanticcpg.language.*
import io.joern.swiftsrc2cpg.testfixtures.SwiftSrc2CpgSuite

class StaticTypePropTests extends SwiftSrc2CpgSuite(withPostProcessing = true) {
  "basic test 1" in {
    val cpg = code(
      """
        |func foo(x: Int) -> Double {
        |  let y = x
        |  let z = staticCall(y)
        |  return z
        |}
        |""".stripMargin)
      .addSymbol("staticCall, Double, Int")

    cpg.method.l
  }

  "basic test 2" in {
    val cpg = code(
      """
        |func foo(x: Int) -> Double {
        |  let y = x
        |  let z = base.foo(y)
        |  return z
        |}
        |""".stripMargin)

    cpg.method.l
  }
}
