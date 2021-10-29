package io.joern.ghidra2cpg.processors
import scala.collection.immutable._

class Arm extends Processor {
  override def getInstructions: HashMap[String, String] =
    HashMap(
      "add" -> "<operator>.incBy",
      "adrp" -> "TODO",
      "asr" -> "TODO",
      "b" -> "<operator>.goto",
      "b.eq" -> "<operator>.goto",
      "b.ne" -> "<operator>.goto",
      "bl" -> "CALL",
      "blr" -> "<operator>.goto",
      "br" -> "<operator>.goto",
      "bti" -> "<operator>.goto",
      "cbnz" -> "<operator>.goto",
      "cbz" -> "<operator>.goto",
      "cmp" -> "<operator>.compare",
      "ldp" -> "<operator>.addressOf",
      "ldr" -> "<operator>.addressOf",
      "ldrb" -> "<operator>.addressOf",
      "lsl" -> "<operator>.addressOf",
      "lsr" -> "<operator>.addressOf",
      "mov" -> "<operator>.assignment",
      "movk" -> "<operator>.assignment",
      "nop" -> "<operator>.NOP",
      "ret" -> "RETURN",
      "stp" -> "<operator>.assignment",
      "str" -> "<operator>.addition",
      "strb" -> "<operator>.assignment",
      "sub" -> "<operator>.subtraction"
    )
}
