package io.joern.console

import scala.util.control.NoStackTrace

/** If you need to throw an Exception that renders ok-ish to the user. e.g. `cpg.help` when no cpg is loaded:
  * ```
  * joern> cpg.help
  * io.joern.console.Error: No CPG loaded for project c - try e.g. `help|importCode|importCpg|open`
  * ```
  *
  * In comparison, with a regular Exception the user sees
  * ```
  * joern> cpg.help
  * java.lang.RuntimeException: No CPG loaded for project console1665115232529348622
  * - try e.g. `help|importCode|importCpg|open`
  *   at io.joern.console.workspacehandling.WorkspaceManager.cpg(WorkspaceManager.scala:218)
  *   at io.joern.console.Console.cpg(Console.scala:137)
  *   ... 39 elided
  * ```
  */
class Error(message: String) extends RuntimeException(message) with NoStackTrace
