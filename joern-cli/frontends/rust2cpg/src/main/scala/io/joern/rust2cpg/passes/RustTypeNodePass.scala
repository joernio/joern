package io.joern.rust2cpg.passes

import io.joern.x2cpg.passes.frontend.TypeNodePass
import io.shiftleft.codepropertygraph.generated.Cpg
import io.joern.rust2cpg.astcreation.RustFullNames

class RustTypeNodePass(cpg: Cpg) extends TypeNodePass(Set.empty, cpg, getTypesFromCpg = true) {

  override def fullToShortName(typeName: String): String = RustFullNames.shortName(typeName)
}
