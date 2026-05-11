package io.joern.rust2cpg.astcreation

import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.PropertyNames

// Computes rust-style full names, e.g. `crate::module::item`.
// ast_gen provided methodFullName/typeFullName are always preferred.
// The fallback is to rely on the current enclosing scope.
trait RustFullNames { this: AstCreator =>

  protected def crateName: String = parseResult.crateName.getOrElse(Defines.Unknown)

  // `crate::module` when it's a submodule, or `crate` when the file is the crate root.
  protected def namespaceFullName: String = parseResult.modulePath match {
    case Some(path) if path.nonEmpty => s"$crateName::$path"
    case _                           => crateName
  }

  protected def composeFullName(name: String): String = {
    val parentFullName = methodAstParentStack.head.properties(PropertyNames.FullName).toString
    s"$parentFullName::$name"
  }
}
