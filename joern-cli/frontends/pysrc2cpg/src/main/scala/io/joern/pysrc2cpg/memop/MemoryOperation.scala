package io.joern.pysrc2cpg.memop

sealed trait MemoryOperation
object Store extends MemoryOperation
object Load  extends MemoryOperation
object Del   extends MemoryOperation
