package io.joern.abap2cpg.passes

import io.joern.x2cpg.passes.base.NamespaceCreator
import io.shiftleft.codepropertygraph.generated.Cpg

/** Creates NAMESPACE nodes and connects NAMESPACE_BLOCKs via REF edges.
  *
  * Thin wrapper around NamespaceCreator following the abap2cpg Pass naming convention.
  */
class NamespacePass(cpg: Cpg) extends NamespaceCreator(cpg)
