package io.shiftleft.py2cpg.memop

import io.shiftleft.pythonparser.ast

import scala.collection.mutable

class AstNodeToMemoryOperationMap {
  private class IdentityHashWrapper(astNode: ast.iast) {
    override def equals(o: Any): Boolean = {
      o match {
        case anyRef: AnyRef =>
          astNode eq anyRef
        case _ =>
          false
      }
    }

    override def hashCode(): Int = {
      System.identityHashCode(astNode)
    }
  }

  private val astNodeToMemOp = mutable.HashMap.empty[IdentityHashWrapper, MemoryOperation]

  def put(astNode: ast.iast, memOp: MemoryOperation): Unit = {
    astNodeToMemOp.put(new IdentityHashWrapper(astNode), memOp)
  }

  def get(astNode: ast.iast): Option[MemoryOperation] = {
    astNodeToMemOp.get(new IdentityHashWrapper(astNode))
  }

}

