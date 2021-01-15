package io.shiftleft.py2cpg

import io.shiftleft.codepropertygraph.Cpg

class Fixture(code: String) {
  val cpg = new Cpg()
  val py2Cpg = new Py2Cpg(
    Iterable.single(() => Py2Cpg.InputPair("test.py", code)),
    cpg
  )
  py2Cpg.buildCpg()
}
