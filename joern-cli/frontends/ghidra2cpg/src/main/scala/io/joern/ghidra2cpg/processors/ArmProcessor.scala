package io.joern.ghidra2cpg.processors

import scala.collection.mutable

object ArmProcessor extends Processor {
  override val getInstructions: mutable.HashMap[String, String] = mutable.HashMap(
    "add"     -> "<operator>.incBy",
    "adr"     -> "<operator>.addressOf",
    "adrp"    -> "<operator>.addressOf",
    "asr"     -> "<operator>.arithmeticShiftRight",
    "b"       -> "<operator>.goto",
    "b.eq"    -> "<operator>.goto",
    "b.ne"    -> "<operator>.goto",
    "bne"     -> "<operator>.goto",
    "bl"      -> "CALL",
    "blr"     -> "CALL",
    "br"      -> "<operator>.goto",
    "bti"     -> "<operator>.goto",
    "bx"      -> "<operator>.goto",
    "blx"     -> "CALL",
    "bxeq"    -> "<operator>.goto",
    "cbnz"    -> "<operator>.goto",
    "cbz"     -> "<operator>.goto",
    "cmp"     -> "<operator>.compare",
    "cpy"     -> "<operator>.assignment",
    "ldp"     -> "<operator>.assignment",
    "ldr"     -> "<operator>.assignment",
    "ldrb"    -> "<operator>.assignment",
    "ldmia"   -> "<operator>.assignment",
    "ldmiaeq" -> "<operator>.assignment",
    "ldmiane" -> "<operator>.assignment",
    "lsl"     -> "<operator>.logicalShiftLeft",
    "lsr"     -> "<operator>.logicalShiftRight",
    "mov"     -> "<operator>.assignment",
    "movk"    -> "<operator>.assignment",
    "movs"    -> "<operator>.assignment",
    "nop"     -> "<operator>.NOP",
    "ret"     -> "RET",
    "stmdb"   -> "<operator>.assignment",
    "stp"     -> "<operator>.assignment",
    "str"     -> "<operator>.assignment",
    "strb"    -> "<operator>.assignment",
    "sub"     -> "<operator>.subtraction"
  )
}
