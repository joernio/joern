package io.joern.pysrc2cpg.memop

import io.joern.pythonparser.ast

import scala.collection.mutable

/** Source identifier names grouped by the lexical AST scope that contains them. */
class ScopeNameCollection {
  private val namesByScope = new java.util.IdentityHashMap[ast.iast, mutable.Set[String]]()
  private val scopeStack   = mutable.Stack.empty[ast.iast]

  private[memop] def pushScope(scope: ast.iast): Unit = {
    scopeStack.push(scope)
    namesByScope.put(scope, mutable.Set.empty)
  }

  private[memop] def popScope(): Unit = {
    scopeStack.pop()
  }

  private[memop] def addName(name: String): Unit = {
    namesByScope.get(scopeStack.head).add(name)
  }

  def namesInScope(scope: ast.iast): collection.Set[String] = {
    Option(namesByScope.get(scope)).map(_.toSet).getOrElse(Set.empty)
  }
}
