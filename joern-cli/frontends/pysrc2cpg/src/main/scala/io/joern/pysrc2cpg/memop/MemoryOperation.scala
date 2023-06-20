package io.joern.pysrc2cpg.memop

sealed trait MemoryOperation {
  override def toString: String = getClass.getSimpleName
}
object Store extends MemoryOperation
object Load  extends MemoryOperation
object Del   extends MemoryOperation
