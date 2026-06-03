package io.joern.rust2cpg.passes

import io.joern.x2cpg.passes.frontend.TypeNodePass
import io.shiftleft.codepropertygraph.generated.Cpg
import io.joern.rust2cpg.astcreation.RustFullNames

object RustTypeNodePass {
  def withTypesFromCpg(cpg: Cpg): TypeNodePass = {
    new TypeNodePass(Set.empty, cpg, getTypesFromCpg = true) {

      override def fullToShortName(typeName: String): String = {
        // A path e.g. `x::...::z<...>` or `z<...>` has shortname `z`, without generics.
        // Any other type shape has matching name/fullName.
        val isPathLike = typeName.headOption.exists(_.isLetter)
        if (isPathLike) {
          typeName.takeWhile(_ != '<').split(RustFullNames.PathSep).lastOption.getOrElse(typeName)
        } else {
          typeName
        }
      }
    }
  }
}
