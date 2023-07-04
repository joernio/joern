package io.joern.pysrc2cpg.memop

import io.joern.pythonparser.ast

import scala.collection.mutable

class AstNodeToMemoryOperationMap {
  private class IdentityHashWrapper(private val astNode: ast.iast) {
    override def equals(o: Any): Boolean = {
      o match {
        case wrapper: IdentityHashWrapper =>
          astNode eq wrapper.astNode
        case _ =>
          false
      }
    }

    override def hashCode(): Int = {
      System.identityHashCode(astNode)
    }

    override def toString: String = astNode.toString
  }

  private val astNodeToMemOp = mutable.HashMap.empty[IdentityHashWrapper, MemoryOperation]

  def put(astNode: ast.iast, memOp: MemoryOperation): Unit = {
    astNodeToMemOp.put(new IdentityHashWrapper(astNode), memOp)
  }

  def get(astNode: ast.iast): Option[MemoryOperation] = {
    astNodeToMemOp.get(new IdentityHashWrapper(astNode))
  }

}
