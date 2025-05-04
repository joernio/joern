package io.joern.c2cpg.astcreation

import io.joern.x2cpg.datastructures.VariableScopeManager
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

class CVariableScopeManager extends VariableScopeManager {

  /** Overrides the parent class implementation to handle C-specific scope path computation. Since for c2cpg we do not
    * handle include directives (like we might for other frontends), we need to strip the global namespace prefix from
    * scope paths to ensure correct fullname resolution across translation units.
    */
  override def computeScopePath: String = {
    getEnclosingMethodScopeElement(stack) match {
      case Some(methodScope) if methodScope.methodName.startsWith(io.joern.x2cpg.Defines.ClosurePrefix) =>
        val offset = NamespaceTraversal.globalNamespaceName.length + 1
        val index  = methodScope.methodFullName.indexOf(NamespaceTraversal.globalNamespaceName) + offset
        methodScope.methodFullName.substring(index).takeWhile(_ != ':')
      case Some(methodScope) if methodScope.methodName != NamespaceTraversal.globalNamespaceName =>
        methodScope.methodFullName.takeWhile(_ != ':')
      case _ => ""
    }
  }

}
