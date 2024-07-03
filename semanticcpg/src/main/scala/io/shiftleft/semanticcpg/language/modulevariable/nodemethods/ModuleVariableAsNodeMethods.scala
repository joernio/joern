package io.shiftleft.semanticcpg.language.modulevariable.nodemethods

import io.shiftleft.codepropertygraph.generated.nodes.{Identifier, Local, Member}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.codepropertygraph.generated.help.Doc

class ModuleVariableAsLocalMethods(node: Local) extends AnyVal {

  @Doc(info = "If this local is declared on the module-defining method level")
  def isModuleVariable: Boolean = node.method.isModule.nonEmpty

}
