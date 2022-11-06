package io.joern.ghidra2cpg.processors

import scala.collection.mutable

object ArmProcessor extends Processor {
  override val getInstructions: mutable.HashMap[String, String] = mutable.HashMap(
    "add"     -> "<operator>.incBy",
    "adr"     -> "<operator>.assignment",
    "adrp"    -> "TODO",
    "asr"     -> "TODO",
    "b"       -> "<operator>.goto",
    "b.eq"    -> "<operator>.goto",
    "b.ne"    -> "<operator>.goto",
    "bne"     -> "<operator>.goto",
    "bl"      -> "CALL",
    "blr"     -> "<operator>.goto",
    "br"      -> "<operator>.goto",
    "bti"     -> "<operator>.goto",
    "bx"      -> "<operator>.goto",
    "blx"     -> "<operator>.goto",
    "bxeq"    -> "<operator>.goto",
    "cbnz"    -> "<operator>.goto",
    "cbz"     -> "<operator>.goto",
    "cmp"     -> "<operator>.compare",
    "cpy"     -> "<operator>.assignment",
    "ldp"     -> "<operator>.addressOf",
    "ldr"     -> "<operator>.addressOf",
    "ldrb"    -> "<operator>.addressOf",
    "ldmia"   -> "<operator>.addressOf",
    "ldmiaeq" -> "<operator>.addressOf",
    "ldmiane" -> "<operator>.addressOf",
    "lsl"     -> "<operator>.addressOf",
    "lsr"     -> "<operator>.addressOf",
    "mov"     -> "<operator>.assignment",
    "movk"    -> "<operator>.assignment",
    "movs"    -> "<operator>.assignment",
    "nop"     -> "<operator>.NOP",
    "ret"     -> "RET",
    "stmdb"   -> "<operator>.assignment",
    "stp"     -> "<operator>.assignment",
    "str"     -> "<operator>.addition",
    "strb"    -> "<operator>.assignment",
    "sub"     -> "<operator>.subtraction"
  )
}
