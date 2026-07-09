package io.joern.lua2cpg.bytecode

final case class LuaRegisterEvent(kind: String, prototypeId: String, pc: Int, slot: Int, valueRef: String)

final case class LuaSemanticStep(sourceRef: String, destRef: String, kind: String)

final case class LuaClosureValue(slot: Int, valueRef: String, targetPrototypeId: String, provenance: String)

final case class LuaCallSite(
  callsiteId: String,
  prototypeId: String,
  pc: Int,
  opcode: String,
  targetValueRef: String,
  firstArgSlot: Option[Int],
  argCount: Option[Int],
  firstReturnSlot: Option[Int],
  returnCount: Option[Int]
)

final case class LuaLocalFlow(sourceRef: String, sinkRef: String, edgeKind: String, provenance: String)

final case class LuaTableFieldFlow(
  tableRef: String,
  keyRef: String,
  writeRef: String,
  readRef: String,
  provenance: String
)

final case class LuaGlobalFlow(
  globalName: String,
  writeRef: String,
  readRef: String,
  valueRef: String,
  provenance: String
)

final case class LuaUpvalueFlow(
  upvalueId: String,
  captureRef: String,
  readRef: String,
  writeRef: String,
  provenance: String
)

final case class LuaCallTargetCandidate(callsiteId: String, targetRef: String, confidence: String, provenance: String)

final case class LuaUnresolvedCall(callsiteId: String, unresolvedReason: String, provenance: String)

final case class LuaKillOverwrite(
  killId: String,
  prototypeId: String,
  firstWrite: String,
  laterWrite: String,
  readRef: String,
  sinkRef: String,
  reason: String
)

final case class LuaNegativeExpectation(
  negativeId: String,
  sourceRef: String,
  sinkRef: String,
  kind: String,
  reason: String
)

final case class LuaPrototypeSemantics(
  registerEvents: Vector[LuaRegisterEvent],
  semanticSteps: Vector[LuaSemanticStep],
  closureValues: Vector[LuaClosureValue],
  callSites: Vector[LuaCallSite],
  localFlows: Vector[LuaLocalFlow],
  tableFieldFlows: Vector[LuaTableFieldFlow],
  globalFlows: Vector[LuaGlobalFlow],
  upvalueFlows: Vector[LuaUpvalueFlow],
  callTargetCandidates: Vector[LuaCallTargetCandidate],
  unresolvedCalls: Vector[LuaUnresolvedCall],
  killOverwrites: Vector[LuaKillOverwrite],
  negativeExpectations: Vector[LuaNegativeExpectation]
)

object LuaInstructionSemantics {
  private val RkConstantBase       = 256
  private val BytecodeProvenance   = "bytecode-only"
  private val BoundaryProvenance   = "bytecode-boundary"
  private val ParamDerivedReason   = "param-derived"
  private val MutationBoundary     = "upvalue-mutation-boundary"
  private val UpvalueStaleBoundary = "no-stale-upvalue-reuse-after-setupval"

  def normalize(prototype: LuaPrototype): LuaPrototypeSemantics = {
    val builder = Vector.newBuilder[LuaPrototypeSemantics]
    builder += normalizeOne(prototype)
    prototype.nested.foreach(child => builder += normalize(child))
    combine(builder.result())
  }

  def normalizePrototype(prototype: LuaPrototype): LuaPrototypeSemantics = normalizeOne(prototype)

  private def normalizeOne(prototype: LuaPrototype): LuaPrototypeSemantics = {
    val state = new SemanticState(prototype)
    prototype.instructions.sortBy(_.pc).foreach(state.visit)
    state.result()
  }

  private def combine(items: Vector[LuaPrototypeSemantics]): LuaPrototypeSemantics =
    LuaPrototypeSemantics(
      registerEvents = items.flatMap(_.registerEvents),
      semanticSteps = items.flatMap(_.semanticSteps),
      closureValues = items.flatMap(_.closureValues),
      callSites = items.flatMap(_.callSites),
      localFlows = items.flatMap(_.localFlows),
      tableFieldFlows = items.flatMap(_.tableFieldFlows),
      globalFlows = items.flatMap(_.globalFlows),
      upvalueFlows = items.flatMap(_.upvalueFlows),
      callTargetCandidates = items.flatMap(_.callTargetCandidates),
      unresolvedCalls = items.flatMap(_.unresolvedCalls),
      killOverwrites = items.flatMap(_.killOverwrites),
      negativeExpectations = items.flatMap(_.negativeExpectations)
    )

  private final class SemanticState(prototype: LuaPrototype) {
    private val registerEvents       = Vector.newBuilder[LuaRegisterEvent]
    private val semanticSteps        = Vector.newBuilder[LuaSemanticStep]
    private val closureValues        = Vector.newBuilder[LuaClosureValue]
    private val callSites            = Vector.newBuilder[LuaCallSite]
    private val localFlows           = Vector.newBuilder[LuaLocalFlow]
    private val tableFieldFlows      = Vector.newBuilder[LuaTableFieldFlow]
    private val globalFlows          = Vector.newBuilder[LuaGlobalFlow]
    private val upvalueFlows         = Vector.newBuilder[LuaUpvalueFlow]
    private val callTargetCandidates = Vector.newBuilder[LuaCallTargetCandidate]
    private val unresolvedCalls      = Vector.newBuilder[LuaUnresolvedCall]
    private val killOverwrites       = Vector.newBuilder[LuaKillOverwrite]
    private val negativeExpectations = Vector.newBuilder[LuaNegativeExpectation]

    private var reaching        = (0 until prototype.numParams).map(slot => slot -> Set(staticSlotRef(slot))).toMap
    private var closuresBySlot  = Map.empty[Int, LuaClosureValue]
    private var tableWrites     = Map.empty[(Int, String), Set[String]]
    private var globalWrites    = Map.empty[String, Set[String]]
    private var mutatedUpvalues = Set.empty[Int]

    def visit(instruction: LuaInstruction): Unit = {
      instruction.opcode match {
        case LuaOpcode.Move =>
          val source = readSlot(instruction, instruction.b)
          writeSlot(instruction, instruction.a, Set(source), "move")
          closuresBySlot.get(instruction.b).foreach { closure =>
            val moved = closure.copy(slot = instruction.a, valueRef = slotRef(instruction.pc, instruction.a))
            closuresBySlot += instruction.a -> moved
            closureValues += moved
          }
        case LuaOpcode.LoadK =>
          writeSlot(instruction, instruction.a, Set(constantRef(instruction.b)), "loadk")
        case LuaOpcode.LoadBool | LuaOpcode.LoadNil | LuaOpcode.NewTable | LuaOpcode.Vararg =>
          writeSlot(
            instruction,
            instruction.a,
            Set(slotRef(instruction.pc, instruction.a)),
            instruction.opcode.mnemonic.toLowerCase
          )
        case LuaOpcode.Closure =>
          val target = nestedPrototypeId(instruction.b)
          val value  = slotRef(instruction.pc, instruction.a)
          writeSlot(instruction, instruction.a, Set(value), "closure")
          val closure = LuaClosureValue(instruction.a, value, target, BytecodeProvenance)
          closuresBySlot += instruction.a -> closure
          closureValues += closure
        case LuaOpcode.Call | LuaOpcode.TailCall =>
          handleCall(instruction)
        case LuaOpcode.Concat =>
          handleConcat(instruction)
        case LuaOpcode.GetUpval =>
          val read = slotRef(instruction.pc, instruction.a)
          writeSlot(instruction, instruction.a, Set(read), "getupval")
          upvalueFlows += LuaUpvalueFlow(upvalueRef(instruction.b), read, read, read, BytecodeProvenance)
          if (mutatedUpvalues(instruction.b)) {
            addBoundary(UpvalueStaleBoundary, read, read, "upvalue mutation invalidates earlier read")
          }
        case LuaOpcode.SetUpval =>
          val read = readSlot(instruction, instruction.a)
          mutatedUpvalues += instruction.b
          addBoundary(
            MutationBoundary,
            read,
            upvalueRef(instruction.b),
            "SETUPVAL introduces an explicit mutation boundary"
          )
        case LuaOpcode.GetGlobal =>
          val write = slotRef(instruction.pc, instruction.a)
          writeSlot(instruction, instruction.a, Set(write), "getglobal")
          stringConstant(instruction.b).foreach { name =>
            globalWrites.get(name).foreach { sources =>
              sources.foreach { source =>
                globalFlows += LuaGlobalFlow(name, source, write, source, BytecodeProvenance)
              }
            }
          }
        case LuaOpcode.SetGlobal =>
          val value = readSlot(instruction, instruction.a)
          stringConstant(instruction.b).foreach(name => globalWrites += name -> Set(value))
        case LuaOpcode.GetTable =>
          handleGetTable(instruction)
        case LuaOpcode.SetTable =>
          handleSetTable(instruction)
        case LuaOpcode.Self =>
          handleSelf(instruction)
        case LuaOpcode.Return =>
          readReturnSlots(instruction).foreach(readSlot(instruction, _))
        case LuaOpcode.Eq | LuaOpcode.Lt | LuaOpcode.Le =>
          rkRegister(instruction.b).foreach(readSlot(instruction, _))
          instruction.c.flatMap(rkRegister).foreach(readSlot(instruction, _))
        case _ =>
          instruction.c.foreach { c =>
            rkRegister(instruction.b).foreach(readSlot(instruction, _))
            rkRegister(c).foreach(readSlot(instruction, _))
          }
      }
    }

    def result(): LuaPrototypeSemantics = {
      LuaPrototypeSemantics(
        registerEvents = registerEvents.result(),
        semanticSteps = semanticSteps.result(),
        closureValues = closureValues.result(),
        callSites = callSites.result(),
        localFlows = localFlows.result(),
        tableFieldFlows = tableFieldFlows.result(),
        globalFlows = globalFlows.result(),
        upvalueFlows = upvalueFlows.result(),
        callTargetCandidates = callTargetCandidates.result(),
        unresolvedCalls = unresolvedCalls.result(),
        killOverwrites = killOverwrites.result(),
        negativeExpectations = negativeExpectations.result()
      )
    }

    private def handleCall(instruction: LuaInstruction): Unit = {
      val targetClosure = closuresBySlot.get(instruction.a)
      val targetRead    = readSlot(instruction, instruction.a)
      val argSlots      = callArgumentSlots(instruction)
      argSlots.foreach(readSlot(instruction, _))
      val returnSlots = callReturnSlots(instruction)
      returnSlots.foreach { slot =>
        writeSlot(instruction, slot, Set(slotRef(instruction.pc, slot)), "call-return")
      }
      val callsite = LuaCallSite(
        callsiteId = instructionRef(instruction.pc),
        prototypeId = prototype.prototypeId,
        pc = instruction.pc,
        opcode = instruction.opcode.mnemonic,
        targetValueRef = targetRead,
        firstArgSlot = argSlots.headOption,
        argCount = Some(argSlots.size),
        firstReturnSlot = returnSlots.headOption,
        returnCount = Some(returnSlots.size)
      )
      callSites += callsite
      targetClosure match {
        case Some(closure) =>
          callTargetCandidates += LuaCallTargetCandidate(
            callsite.callsiteId,
            closure.targetPrototypeId,
            "candidate",
            BytecodeProvenance
          )
        case None =>
          if (isParamDerived(instruction.a)) {
            unresolvedCalls += LuaUnresolvedCall(callsite.callsiteId, ParamDerivedReason, BoundaryProvenance)
          }
      }
    }

    private def handleConcat(instruction: LuaInstruction): Unit = {
      val c       = requireC(instruction)
      val sources = (instruction.b to c).map(slot => readSlot(instruction, slot)).toSet
      writeSlot(instruction, instruction.a, sources, "concat")
    }

    private def handleGetTable(instruction: LuaInstruction): Unit = {
      val tableSlot = instruction.b
      readSlot(instruction, tableSlot)
      instruction.c.flatMap(rkRegister).foreach(readSlot(instruction, _))
      val write = slotRef(instruction.pc, instruction.a)
      writeSlot(instruction, instruction.a, Set(write), "gettable")
      instruction.c.flatMap(rkConstantRef).foreach { key =>
        tableWrites.get((tableSlot, key)).foreach { sources =>
          sources.foreach { source =>
            tableFieldFlows += LuaTableFieldFlow(
              slotRef(instruction.pc, tableSlot),
              key,
              source,
              write,
              BytecodeProvenance
            )
          }
        }
      }
      if (isGlobalEnvironmentTable(tableSlot)) {
        instruction.c.flatMap(rkConstantName).foreach { name =>
          globalWrites.get(name).foreach { sources =>
            sources.foreach { source =>
              globalFlows += LuaGlobalFlow(name, source, write, source, BytecodeProvenance)
            }
          }
        }
      }
    }

    private def handleSetTable(instruction: LuaInstruction): Unit = {
      val tableSlot = instruction.a
      readSlot(instruction, tableSlot)
      rkRegister(instruction.b).foreach(readSlot(instruction, _))
      val valueRefs = instruction.c.flatMap(rkRegister).map(readSlot(instruction, _)).toSet
      instruction.bOptionConstantString.foreach { key =>
        tableWrites += (tableSlot, key) -> valueRefs
      }
      if (isGlobalEnvironmentTable(tableSlot)) {
        instruction.bOptionConstantName.foreach { name =>
          if (valueRefs.nonEmpty) {
            globalWrites += name -> valueRefs
          }
        }
      }
    }

    private def handleSelf(instruction: LuaInstruction): Unit = {
      readSlot(instruction, instruction.b)
      instruction.c.flatMap(rkRegister).foreach(readSlot(instruction, _))
      writeSlot(instruction, instruction.a + 1, Set(slotRef(instruction.pc, instruction.b)), "self-base")
      writeSlot(instruction, instruction.a, Set(slotRef(instruction.pc, instruction.a)), "self-member")
    }

    private def readReturnSlots(instruction: LuaInstruction): Seq[Int] =
      instruction.b match {
        case 0 => Seq.empty
        case 1 => Seq.empty
        case n => instruction.a until (instruction.a + n - 1)
      }

    private def callArgumentSlots(instruction: LuaInstruction): Seq[Int] =
      instruction.b match {
        case 0 => (instruction.a + 1) until prototype.maxStack
        case 1 => Seq.empty
        case n => (instruction.a + 1) until (instruction.a + n)
      }

    private def callReturnSlots(instruction: LuaInstruction): Seq[Int] =
      instruction.c match {
        case Some(0) => Seq(instruction.a)
        case Some(1) => Seq.empty
        case Some(n) => instruction.a until (instruction.a + n - 1)
        case None    => Seq.empty
      }

    private def readSlot(instruction: LuaInstruction, slot: Int): String = {
      val read = slotRef(instruction.pc, slot)
      registerEvents += LuaRegisterEvent("read", prototype.prototypeId, instruction.pc, slot, read)
      reaching.get(slot).toSeq.flatten.foreach { source =>
        localFlows += LuaLocalFlow(source, read, "may-reaching-definition", BytecodeProvenance)
      }
      read
    }

    private def writeSlot(instruction: LuaInstruction, slot: Int, sources: Set[String], kind: String): Unit = {
      val write = slotRef(instruction.pc, slot)
      registerEvents += LuaRegisterEvent("write", prototype.prototypeId, instruction.pc, slot, write)
      sources.filterNot(_ == write).foreach { source =>
        localFlows += LuaLocalFlow(source, write, "same-instruction-dependence", BytecodeProvenance)
        semanticSteps += LuaSemanticStep(source, write, kind)
      }
      reaching.get(slot).foreach { prior =>
        if (prior.nonEmpty && !prior.contains(write)) {
          prior.foreach { first =>
            killOverwrites += LuaKillOverwrite(
              s"${prototype.prototypeId}:pc${instruction.pc}:r$slot:kills:$first",
              prototype.prototypeId,
              first,
              write,
              write,
              write,
              "same-slot-overwrite-kills-prior-definition"
            )
          }
        }
      }
    }
      reaching += slot -> Set(write)
      closuresBySlot -= slot

    private def isParamDerived(slot: Int): Boolean =
      reachesParameter(reaching.getOrElse(slot, Set.empty), Set.empty)

    private def reachesParameter(refs: Set[String], seen: Set[String]): Boolean = {
      val pending = refs.diff(seen)
      pending.exists(_.startsWith(s"${prototype.prototypeId}:r")) || {
        val parents = localFlows
          .result()
          .collect {
            case flow if pending(flow.sinkRef) => flow.sourceRef
          }
          .toSet
        parents.nonEmpty && reachesParameter(parents, seen ++ pending)
      }
    }

    private def isGlobalEnvironmentTable(slot: Int): Boolean =
      reaching.get(slot).toSeq.flatten.exists { ref =>
        val globalGet = prototype.instructions.exists { instruction =>
          slotRef(instruction.pc, instruction.a) == ref &&
          instruction.opcode == LuaOpcode.GetGlobal &&
          stringConstant(instruction.b).contains("_G")
        }
        globalGet
      }

    private def addBoundary(kind: String, sourceRef: String, sinkRef: String, reason: String): Unit =
      negativeExpectations += LuaNegativeExpectation(s"$kind:$sourceRef->$sinkRef", sourceRef, sinkRef, kind, reason)

    private def slotRef(pc: Int, slot: Int): String = s"${prototype.prototypeId}@pc$pc:r$slot"

    private def instructionRef(pc: Int): String = s"${prototype.prototypeId}@pc$pc"

    private def staticSlotRef(slot: Int): String = s"${prototype.prototypeId}:r$slot"

    private def constantRef(index: Int): String = s"${prototype.prototypeId}:k$index"

    private def upvalueRef(index: Int): String = s"${prototype.prototypeId}:u$index"

    private def nestedPrototypeId(ordinal: Int): String = s"${prototype.prototypeId}.$ordinal"

    private def stringConstant(index: Int): Option[String] =
      prototype.constants.collectFirst { case LuaConstant(`index`, "string", LuaConstantValue.StringValue(value)) =>
        value
      }

    private def rkRegister(value: Int): Option[Int] =
      if (value < RkConstantBase) Some(value) else None

    private def rkConstantRef(value: Int): Option[String] =
      if (value >= RkConstantBase) Some(constantRef(value - RkConstantBase)) else None

    private def rkConstantName(value: Int): Option[String] =
      if (value >= RkConstantBase) stringConstant(value - RkConstantBase) else None

    private def requireC(instruction: LuaInstruction): Int =
      instruction.c match {
        case Some(value) => value
        case None =>
          throw new IllegalArgumentException(
            s"${instruction.opcode.mnemonic} at ${prototype.prototypeId}@pc${instruction.pc} has no C operand"
          )
      }

    extension (instruction: LuaInstruction) {
      private def bOptionConstantString: Option[String] =
        if (instruction.b >= RkConstantBase) Some(constantRef(instruction.b - RkConstantBase)) else None

      private def bOptionConstantName: Option[String] =
        if (instruction.b >= RkConstantBase) stringConstant(instruction.b - RkConstantBase) else None
    }

  }
}
