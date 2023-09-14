package io.shiftleft.semanticcpg.language.modulevariable

import io.shiftleft.codepropertygraph.generated.v2.nodes.{Local, StaticType}

trait ModuleVariableT
object OpNodes {

  /** Represents a module-level global variable. This kind of node behaves like both a local variable and a field access
    * and is common in languages such as Python/JavaScript.
    */
  type ModuleVariable = Local with StaticType[ModuleVariableT]

}
