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

  private final case class UpvalueClosureBindings(
    directTargets: Map[Int, String],
    tableTargets: Map[Int, Map[String, String]]
  ) {
    def nonEmpty: Boolean = directTargets.nonEmpty || tableTargets.nonEmpty
  }

  private object UpvalueClosureBindings {
    val Empty: UpvalueClosureBindings = UpvalueClosureBindings(Map.empty, Map.empty)
  }

  def normalize(prototype: LuaPrototype): LuaPrototypeSemantics = {
    val upvalueClosureBindings = closureBindingsByPrototypeAndUpvalue(prototype)
    val builder                = Vector.newBuilder[LuaPrototypeSemantics]
    def visit(current: LuaPrototype): Unit = {
      builder += normalizeOne(
        current,
        upvalueClosureBindings.getOrElse(current.prototypeId, UpvalueClosureBindings.Empty)
      )
      current.nested.foreach(visit)
    }
    visit(prototype)
    combine(builder.result())
  }

  def normalizePrototype(prototype: LuaPrototype): LuaPrototypeSemantics =
    normalizeOne(prototype, UpvalueClosureBindings.Empty)

  private def normalizeOne(
    prototype: LuaPrototype,
    upvalueClosureBindings: UpvalueClosureBindings
  ): LuaPrototypeSemantics = {
    val state = new SemanticState(prototype, upvalueClosureBindings)
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

  private def closureBindingsByPrototypeAndUpvalue(root: LuaPrototype): Map[String, UpvalueClosureBindings] = {
    def collect(
      parent: LuaPrototype,
      currentUpvalueBindings: UpvalueClosureBindings
    ): Map[String, UpvalueClosureBindings] = {
      val directTargetsBySlot = scala.collection.mutable.Map.empty[Int, String]
      val tableTargetsBySlot  = scala.collection.mutable.Map.empty[Int, Map[String, String]]
      val capturedByPrototype = scala.collection.mutable.Map.empty[String, UpvalueClosureBindings]
      val sorted              = parent.instructions.sortBy(_.pc)
      val bindingPcs          = closureBindingPcs(parent)

      def clearSlot(slot: Int): Unit = {
        directTargetsBySlot -= slot
        tableTargetsBySlot -= slot
      }

      def keyName(value: Int): Option[String] =
        if (value >= RkConstantBase) constantString(parent, value - RkConstantBase) else None

      def childUpvalueBindings(closure: LuaInstruction): UpvalueClosureBindings = {
        val childPrototypeId = s"${parent.prototypeId}.${closure.b}"
        parent.nested
          .find(_.prototypeId == childPrototypeId)
          .map { child =>
            val directTargets = sorted
              .dropWhile(_.pc <= closure.pc)
              .take(child.upvalueCount)
              .zipWithIndex
              .flatMap {
                case (binder, upvalueSlot) if binder.opcode == LuaOpcode.Move =>
                  directTargetsBySlot.get(binder.b).map(upvalueSlot -> _)
                case (binder, upvalueSlot) if binder.opcode == LuaOpcode.GetUpval =>
                  currentUpvalueBindings.directTargets.get(binder.b).map(upvalueSlot -> _)
                case _ => None
              }
              .toMap
            val tableTargets = sorted
              .dropWhile(_.pc <= closure.pc)
              .take(child.upvalueCount)
              .zipWithIndex
              .flatMap {
                case (binder, upvalueSlot) if binder.opcode == LuaOpcode.Move =>
                  tableTargetsBySlot.get(binder.b).map(upvalueSlot -> _)
                case (binder, upvalueSlot) if binder.opcode == LuaOpcode.GetUpval =>
                  currentUpvalueBindings.tableTargets.get(binder.b).map(upvalueSlot -> _)
                case _ => None
              }
              .toMap
            UpvalueClosureBindings(directTargets, tableTargets)
          }
          .getOrElse(UpvalueClosureBindings.Empty)
      }

      sorted.foreach {
        case instruction if bindingPcs(instruction.pc) =>
        case instruction if instruction.opcode == LuaOpcode.Closure =>
          clearSlot(instruction.a)
          val targetPrototypeId = s"${parent.prototypeId}.${instruction.b}"
          directTargetsBySlot += instruction.a -> targetPrototypeId
          val childBindings = childUpvalueBindings(instruction)
          if (childBindings.nonEmpty) {
            capturedByPrototype += targetPrototypeId -> childBindings
          }
          parent.nested
            .find(_.prototypeId == targetPrototypeId)
            .foreach(child => capturedByPrototype ++= collect(child, childBindings))
        case instruction if instruction.opcode == LuaOpcode.Move =>
          val movedDirect = directTargetsBySlot.get(instruction.b)
          val movedTable  = tableTargetsBySlot.get(instruction.b)
          clearSlot(instruction.a)
          movedDirect.foreach(target => directTargetsBySlot += instruction.a -> target)
          movedTable.foreach(targets => tableTargetsBySlot += instruction.a -> targets)
        case instruction if instruction.opcode == LuaOpcode.GetUpval =>
          val inheritedDirect = currentUpvalueBindings.directTargets.get(instruction.b)
          val inheritedTable  = currentUpvalueBindings.tableTargets.get(instruction.b)
          clearSlot(instruction.a)
          inheritedDirect.foreach(target => directTargetsBySlot += instruction.a -> target)
          inheritedTable.foreach(targets => tableTargetsBySlot += instruction.a -> targets)
        case instruction if instruction.opcode == LuaOpcode.GetTable =>
          val loaded = for {
            tableTargets <- tableTargetsBySlot.get(instruction.b)
            key          <- instruction.c.flatMap(keyName)
            target       <- tableTargets.get(key)
          } yield target
          clearSlot(instruction.a)
          loaded.foreach(target => directTargetsBySlot += instruction.a -> target)
        case instruction if instruction.opcode == LuaOpcode.SetTable =>
          for {
            key         <- keyName(instruction.b)
            valueSlot   <- instruction.c.flatMap(rkRegisterValue)
            valueTarget <- directTargetsBySlot.get(valueSlot)
          } {
            val currentTargets = tableTargetsBySlot.getOrElse(instruction.a, Map.empty)
            tableTargetsBySlot += instruction.a -> (currentTargets + (key -> valueTarget))
          }
        case instruction if instruction.opcode == LuaOpcode.Call || instruction.opcode == LuaOpcode.TailCall =>
          clearSlot(instruction.a)
        case instruction
            if instruction.opcode == LuaOpcode.LoadK || instruction.opcode == LuaOpcode.LoadBool ||
              instruction.opcode == LuaOpcode.LoadNil || instruction.opcode == LuaOpcode.GetGlobal ||
              instruction.opcode == LuaOpcode.NewTable ||
              instruction.opcode == LuaOpcode.Self || instruction.opcode == LuaOpcode.Vararg =>
          clearSlot(instruction.a)
        case _ =>
      }

      capturedByPrototype.toMap
    }

    collect(root, UpvalueClosureBindings.Empty)
  }

  private def constantString(prototype: LuaPrototype, index: Int): Option[String] =
    prototype.constants.collectFirst { case LuaConstant(`index`, "string", LuaConstantValue.StringValue(value)) =>
      value
    }

  private def rkRegisterValue(value: Int): Option[Int] =
    if (value < RkConstantBase) Some(value) else None

  private def closureBindingPcs(prototype: LuaPrototype): Set[Int] =
    prototype.instructions
      .sortBy(_.pc)
      .zipWithIndex
      .flatMap {
        case (closure, index) if closure.opcode == LuaOpcode.Closure =>
          prototype.nested.find(_.prototypeId == s"${prototype.prototypeId}.${closure.b}").toVector.flatMap { child =>
            (1 to child.upvalueCount)
              .takeWhile { offset =>
                prototype.instructions
                  .sortBy(_.pc)
                  .lift(index + offset)
                  .exists(binding =>
                    binding.pc == closure.pc + offset &&
                      (binding.opcode == LuaOpcode.Move || binding.opcode == LuaOpcode.GetUpval)
                  )
              }
              .flatMap(offset => prototype.instructions.sortBy(_.pc).lift(index + offset).map(_.pc))
          }
        case _ => Vector.empty
      }
      .toSet

  private final class SemanticState(prototype: LuaPrototype, upvalueClosureBindings: UpvalueClosureBindings) {
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

    private var reaching           = (0 until prototype.numParams).map(slot => slot -> Set(staticSlotRef(slot))).toMap
    private var closuresBySlot     = Map.empty[Int, LuaClosureValue]
    private var tableObjectsBySlot = (0 until prototype.numParams).map(slot => slot -> staticSlotRef(slot)).toMap
    private var tableWrites        = Map.empty[(String, String), Set[String]]
    private var closureTableWrites = Map.empty[(Int, String), LuaClosureValue]
    private var globalWrites       = Map.empty[String, Set[String]]
    private var mutatedUpvalues    = Set.empty[Int]
    private var conditionalWriteUntilPc = Option.empty[Int]

    def visit(instruction: LuaInstruction): Unit = {
      conditionalWriteUntilPc = conditionalWriteUntilPc.filter(instruction.pc < _)
      instruction.opcode match {
        case LuaOpcode.Move =>
          val source           = readSlot(instruction, instruction.b)
          val movedTableObject = tableObjectsBySlot.get(instruction.b)
          val movedTableClosures = closureTableWrites.collect {
            case ((tableSlot, key), closure) if tableSlot == instruction.b => key -> closure
          }
          writeSlot(instruction, instruction.a, Set(source), "move")
          movedTableObject.foreach(tableObject => tableObjectsBySlot += instruction.a -> tableObject)
          closuresBySlot.get(instruction.b).foreach { closure =>
            val moved = closure.copy(slot = instruction.a, valueRef = slotRef(instruction.pc, instruction.a))
            closuresBySlot += instruction.a -> moved
            closureValues += moved
          }
          movedTableClosures.foreach { case (key, closure) =>
            closureTableWrites += (instruction.a, key) -> closure.copy(
              slot = instruction.a,
              valueRef = slotRef(instruction.pc, instruction.a)
            )
          }
        case LuaOpcode.LoadK =>
          writeSlot(instruction, instruction.a, Set(constantRef(instruction.b)), "loadk")
        case LuaOpcode.NewTable =>
          writeSlot(instruction, instruction.a, Set(slotRef(instruction.pc, instruction.a)), "newtable")
          tableObjectsBySlot += instruction.a -> slotRef(instruction.pc, instruction.a)
        case LuaOpcode.LoadBool | LuaOpcode.LoadNil | LuaOpcode.Vararg =>
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
          upvalueClosureBindings.directTargets.get(instruction.b).foreach { targetPrototypeId =>
            val closure = LuaClosureValue(instruction.a, read, targetPrototypeId, BytecodeProvenance)
            closuresBySlot += instruction.a -> closure
            closureValues += closure
          }
          upvalueClosureBindings.tableTargets.get(instruction.b).foreach { targetsByKey =>
            targetsByKey.foreach { case (key, targetPrototypeId) =>
              closureTableWrites += (instruction.a, key) ->
                LuaClosureValue(instruction.a, read, targetPrototypeId, BytecodeProvenance)
            }
          }
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
            tableObjectsBySlot += instruction.a -> s"global:$name"
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
        case LuaOpcode.SetList =>
          handleSetList(instruction)
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
      forwardJumpTargetPc(instruction).foreach { target =>
        conditionalWriteUntilPc = Some(math.max(conditionalWriteUntilPc.getOrElse(target), target))
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
      val argumentReads = argSlots.map(readSlot(instruction, _))
      val returnSlots   = callReturnSlots(instruction)
      val callReads     = (targetRead +: argumentReads).toSet
      returnSlots.foreach { slot =>
        writeSlot(instruction, slot, callReads, "call-return")
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
      val tableSlot   = instruction.b
      val tableObject = tableObjectsBySlot.get(tableSlot)
      val tableRead   = readSlot(instruction, tableSlot)
      val keyReads    = instruction.c.flatMap(rkRegister).map(readSlot(instruction, _)).toSet
      val write       = slotRef(instruction.pc, instruction.a)
      val loadedClosure = instruction.c
        .flatMap(rkConstantName)
        .flatMap(key => closureTableWrites.get((tableSlot, key)))
      writeSlot(instruction, instruction.a, keyReads + tableRead, "gettable")
      instruction.c.flatMap(rkConstantRef).foreach { key =>
        tableObject.flatMap(identity => tableWrites.get((identity, key))).foreach { sources =>
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
      loadedClosure.foreach { closure =>
        val loaded = closure.copy(slot = instruction.a, valueRef = write)
        closuresBySlot += instruction.a -> loaded
        closureValues += loaded
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
      val valueSlot = instruction.c.flatMap(rkRegister)
      val valueRefs = valueSlot.map(readSlot(instruction, _)).toSet
      for {
        tableObject <- tableObjectsBySlot.get(tableSlot)
        key         <- instruction.bOptionConstantString
      } {
        tableWrites += (tableObject, key) -> valueRefs
      }
      instruction.bOptionConstantName.foreach { key =>
        valueSlot.flatMap(closuresBySlot.get).foreach { closure =>
          closureTableWrites += (tableSlot, key) -> closure
        }
      }
      if (isGlobalEnvironmentTable(tableSlot)) {
        instruction.bOptionConstantName.foreach { name =>
          if (valueRefs.nonEmpty) {
            globalWrites += name -> valueRefs
          }
        }
      }
    }

    private def handleSetList(instruction: LuaInstruction): Unit = {
      val tableSlot = instruction.a
      readSlot(instruction, tableSlot)
      if (instruction.b > 0 && tableObjectsBySlot.contains(tableSlot)) {
        val tableObject = tableObjectsBySlot(tableSlot)
        val valueRefs   = (1 to instruction.b).map(offset => readSlot(instruction, tableSlot + offset))
        instruction.c.foreach { block =>
          valueRefs.zipWithIndex.foreach { case (valueRef, index) =>
            tableWrites += (tableObject, setListElementKey(block, index)) -> Set(valueRef)
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
      if (conditionalWriteUntilPc.isDefined && isConditionalDefaultWrite(instruction)) {
        reaching += slot -> (reaching.getOrElse(slot, Set.empty) + write)
      } else {
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
        reaching += slot -> Set(write)
        closuresBySlot -= slot
        tableObjectsBySlot -= slot
        closureTableWrites = closureTableWrites.filterNot { case ((tableSlot, _), _) => tableSlot == slot }
      }
    }

    private def forwardJumpTargetPc(instruction: LuaInstruction): Option[Int] =
      if (instruction.opcode == LuaOpcode.Jmp) {
        val target = instruction.pc + 1 + instruction.b
        Option.when(target > instruction.pc + 1)(target)
      } else {
        None
      }

    private def isConditionalDefaultWrite(instruction: LuaInstruction): Boolean =
      instruction.opcode == LuaOpcode.LoadK || instruction.opcode == LuaOpcode.LoadBool ||
        instruction.opcode == LuaOpcode.LoadNil

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

    private def setListElementKey(block: Int, index: Int): String = s"${prototype.prototypeId}:setlist:$block:$index"

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
