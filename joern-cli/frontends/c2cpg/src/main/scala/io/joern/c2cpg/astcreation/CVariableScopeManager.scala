package io.joern.c2cpg.astcreation

import io.joern.x2cpg.datastructures.VariableScopeManager
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

class CVariableScopeManager extends VariableScopeManager {

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
