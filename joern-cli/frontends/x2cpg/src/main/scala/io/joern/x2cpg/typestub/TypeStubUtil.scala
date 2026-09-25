package io.joern.x2cpg.typestub

import io.shiftleft.semanticcpg.utils.ExternalCommand

import java.net.URI
import java.nio.file.{Path, Paths}

object TypeStubUtil {

  /** Obtains the type stub dir for this frontend.
    * @return
    *   the directory where type stubs are.
    */
  def typeStubDir(codeSourceLocation: String): Path = {
    // Parse as a URI rather than stripping the scheme manually: on Windows the code source URL is
    // `file:/D:/...`, and only URI-aware conversion turns that into a valid `D:\...` path.
    // The location string may contain unescaped spaces (e.g. `file:/Users/John Doe/...`),
    // which `URI.create` rejects -- escape them first.
    val path = Paths.get(URI.create(codeSourceLocation.replace(" ", "%20"))).toAbsolutePath
    ExternalCommand.executableDir(path).getParent.resolve("type_stubs")
  }

}
