package io.joern.lua2cpg.bytecode

import io.joern.lua2cpg.Config
import io.joern.lua2cpg.passes.LuaBytecodeModelPass.DecodedBytecode

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*

object LuaRealFirmwareEvidenceExporter {
  def write(config: Config, decoded: Vector[DecodedBytecode], semantics: LuaProgramSemantics): Unit =
    config.realFirmwareOutputDir.foreach { outputDir =>
      validateTaintPathEndpoints(semantics)
      val root       = Paths.get(outputDir)
      val stagingDir = root.resolve("staging")
      Files.createDirectories(stagingDir)

      val artifacts = decoded.map { item =>
        val staging = stagingFor(item, semantics)
        writeJson(stagingDir.resolve(s"${safeArtifactName(artifactIdFor(item.relativeName))}.json"), staging)
        artifactSummary(item)
      }

      writeJson(
        root.resolve("decoder-summary.json"),
        ujson.Obj(
          "run_id" -> "lua2cpg-upstream-export",
          "totals" -> ujson.Obj(
            "input_count"      -> decoded.size,
            "decoded_count"    -> decoded.count(_.result.artifact.accepted),
            "diagnostic_count" -> decoded.count(!_.result.artifact.accepted),
            "prototype_count"  -> decoded.flatMap(_.result.root.toVector.flatMap(allPrototypes)).size,
            "instruction_count" -> decoded
              .flatMap(_.result.root.toVector.flatMap(allPrototypes))
              .map(_.instructions.size)
              .sum,
            "callsite_count"  -> decoded.flatMap(_.result.root.toVector.flatMap(allPrototypes)).map(countCallsites).sum,
            "flow_edge_count" -> decoded.map(item => localSemantics(item.result).localFlows.size).sum
          ),
          "artifacts" -> artifacts
        )
      )
      writeJson(root.resolve("path-search-profile.json"), pathSearchProfile(semantics))
      writeJson(
        root.resolve("run-summary.json"),
        ujson.Obj(
          "run_id"                        -> "lua2cpg-upstream-export",
          "status"                        -> "completed",
          "native_status"                 -> "cpg-written",
          "d19_path_parity_status"        -> "not-run",
          "native_d19_path_parity_status" -> "not-run",
          "totals" -> ujson.Obj(
            "input_count"      -> decoded.size,
            "decoded_count"    -> decoded.count(_.result.artifact.accepted),
            "diagnostic_count" -> decoded.count(!_.result.artifact.accepted)
          )
        )
      )
      writeJson(root.resolve("run-errors.json"), ujson.Obj("errors" -> ujson.Arr()))
    }

  private def stagingFor(item: DecodedBytecode, semantics: LuaProgramSemantics): ujson.Obj = {
    val relativeName = item.relativeName
    val result       = item.result
    val artifactId   = artifactIdFor(relativeName)
    val accepted     = result.artifact.accepted
    val prototypes   = result.root.toVector.flatMap(allPrototypes)
    val local        = localSemantics(result)

    ujson.Obj(
      "artifact_id"          -> artifactId,
      "relative_path"        -> relativeName,
      "decoder_status"       -> (if (accepted) "accepted" else result.artifact.diagnostic.kind),
      "input_kind"           -> result.artifact.inputKind,
      "profile"              -> result.profile.map(profileJson).getOrElse(ujson.Null),
      "provenance"           -> "upstream-lua2cpg,bytecode-only",
      "nodes"                -> nodesFor(prototypes),
      "edges"                -> local.localFlows.map(localFlowJson),
      "call_name_resolution" -> callNameResolutionRows(relativeName, result, semantics),
      "unresolved_values"    -> ujson.Arr(),
      "upvalue_flows"        -> local.upvalueFlows.map(upvalueFlowJson),
      "defuse_paths"         -> local.localFlows.map(defusePathJson(relativeName)),
      "function_identity"    -> prototypes.map(functionIdentityJson(artifactId, relativeName)),
      "module_resolution" -> semantics.moduleResolutions
        .filter(_.fromModulePath == relativeName)
        .map(moduleResolutionJson),
      "module_return_table" -> semantics.moduleReturnTables
        .filter(_.modulePath == relativeName)
        .map(moduleReturnTableJson),
      "module_linkage"        -> moduleLinkageRows(relativeName, semantics),
      "call_target_candidate" -> callTargetCandidateRows(relativeName, local, semantics),
      "interproc_arg_flow" -> semantics.interproceduralArgFlows
        .filter(_.fromArgumentRef.startsWith(s"$relativeName:"))
        .map(interproceduralArgFlowJson),
      "interproc_return_flow" -> semantics.interproceduralReturnFlows
        .filter(_.callerResultRef.startsWith(s"$relativeName:"))
        .map(interproceduralReturnFlowJson),
      "source_endpoints" -> semantics.sourceEndpoints
        .filter(_.sourceRef.startsWith(s"$relativeName:"))
        .map(sourceEndpointJson),
      "sink_endpoints" -> semantics.sinkEndpoints.filter(_.sinkRef.startsWith(s"$relativeName:")).map(sinkEndpointJson),
      "path_evidence"  -> pathEvidenceRows(relativeName, semantics)
    )
  }

  private def artifactSummary(item: DecodedBytecode): ujson.Obj = {
    val prototypes = item.result.root.toVector.flatMap(allPrototypes)
    ujson.Obj(
      "artifact_id"        -> artifactIdFor(item.relativeName),
      "relative_path"      -> item.relativeName,
      "status"             -> (if (item.result.artifact.accepted) "accepted" else item.result.artifact.diagnostic.kind),
      "profile_id"         -> item.result.profile.map(_.profileId).getOrElse("unavailable"),
      "profile"            -> item.result.profile.map(profileJson).getOrElse(ujson.Null),
      "diagnostic_kind"    -> item.result.artifact.diagnostic.kind,
      "diagnostic_message" -> item.result.artifact.diagnostic.message,
      "prototype_count"    -> prototypes.size,
      "instruction_count"  -> prototypes.map(_.instructions.size).sum,
      "callsite_count"     -> prototypes.map(countCallsites).sum,
      "flow_edge_count"    -> localSemantics(item.result).localFlows.size,
      "staging_graph"      -> s"staging/${safeArtifactName(artifactIdFor(item.relativeName))}.json",
      "path_evidence_count" -> 0
    )
  }

  private def localSemantics(result: LuaBytecodeDecodeResult): LuaPrototypeSemantics =
    result.root
      .map(LuaInstructionSemantics.normalize)
      .getOrElse(
        LuaPrototypeSemantics(
          Vector.empty,
          Vector.empty,
          Vector.empty,
          Vector.empty,
          Vector.empty,
          Vector.empty,
          Vector.empty,
          Vector.empty,
          Vector.empty,
          Vector.empty,
          Vector.empty,
          Vector.empty
        )
      )

  private def callNameResolutionRows(
    relativeName: String,
    result: LuaBytecodeDecodeResult,
    semantics: LuaProgramSemantics
  ): Vector[ujson.Obj] = {
    val decodedRows = result.root.toVector.flatMap(allPrototypes).flatMap { prototype =>
      val callsByPrototype =
        LuaInstructionSemantics.normalizePrototype(prototype).callSites.map(row => row.callsiteId -> row).toMap
      resolvedCalls(prototype).map { call =>
        val callsite = callsByPrototype.get(call.callsiteId)
        val targetValueRef = callsite
          .map(_.targetValueRef)
          .getOrElse(
            throw new IllegalStateException(s"missing callsite semantics for ${relativeName}::${call.callsiteId}")
          )
        ujson.Obj(
          "resolution_id"    -> s"${relativeName}:${call.callsiteId}:${call.resolvedName.getOrElse("unresolved")}",
          "artifact_id"      -> artifactIdFor(relativeName),
          "artifact_role"    -> "main",
          "module_path"      -> relativeName,
          "prototype_id"     -> call.prototypeId,
          "pc"               -> call.pc,
          "callsite_id"      -> scopedCallsite(relativeName, call.callsiteId),
          "target_value_ref" -> targetValueRef,
          "bytecode_pattern" -> call.resolvedName.getOrElse("unresolved-target"),
          "resolved_name"    -> call.resolvedName.getOrElse(""),
          "name_components"  -> call.resolvedName.map(_.split('.').toVector).getOrElse(Vector.empty),
          "resolution_kind"  -> call.resolvedName.map(_ => "global-member-chain").getOrElse("unresolved"),
          "confidence"       -> call.resolvedName.map(_ => "bytecode-derived").getOrElse("unresolved"),
          "provenance"       -> "upstream-lua2cpg,bytecode-only",
          "unresolved_reason" -> call.resolvedName
            .map(_ => "none")
            .getOrElse("target-register-has-no-resolved-name-chain"),
          "argument_value_refs"         -> call.argumentRefs,
          "return_value_refs"           -> call.returnRefs,
          "argument_constants"          -> ujson.Arr(),
          "direct_target_prototype_ids" -> ujson.Arr()
        )
      }
    }
    val sanitizerRows = semantics.sanitizerCalls.flatMap { row =>
      val (modulePath, callsiteId) = splitQualifiedRef(row.callsiteId)
      if (modulePath == relativeName) {
        Vector(
          ujson.Obj(
            "resolution_id"               -> s"${row.callsiteId}:${row.sanitizerName}",
            "artifact_id"                 -> artifactIdFor(relativeName),
            "artifact_role"               -> "main",
            "module_path"                 -> modulePath,
            "prototype_id"                -> prototypeIdFromCallsiteId(callsiteId),
            "pc"                          -> pcFromCallsiteId(callsiteId),
            "callsite_id"                 -> toScopedStepRef(row.callsiteId),
            "target_value_ref"            -> splitQualifiedRef(row.sanitizedValueRef)._2,
            "bytecode_pattern"            -> s"sanitizer:${row.sanitizerName}",
            "resolved_name"               -> row.sanitizerName,
            "name_components"             -> row.sanitizerName.split('.').toVector,
            "resolution_kind"             -> "sanitizer-call",
            "confidence"                  -> "bytecode-derived",
            "provenance"                  -> row.provenance,
            "unresolved_reason"           -> "none",
            "argument_value_refs"         -> Vector(splitQualifiedRef(row.sanitizedValueRef)._2),
            "return_value_refs"           -> Vector(splitQualifiedRef(row.sanitizedValueRef)._2),
            "argument_constants"          -> ujson.Arr(),
            "direct_target_prototype_ids" -> ujson.Arr()
          )
        )
      } else Vector.empty
    }
    decodedRows ++ sanitizerRows
  }

  private def resolvedCalls(prototype: LuaPrototype): Vector[ResolvedCall] =
    prototype.instructions
      .filter(instruction => instruction.opcode == LuaOpcode.Call || instruction.opcode == LuaOpcode.TailCall)
      .map { instruction =>
        ResolvedCall(
          callsiteId = s"${prototype.prototypeId}@pc${instruction.pc}",
          prototypeId = prototype.prototypeId,
          pc = instruction.pc,
          resolvedName = callTargetNameAt(prototype, instruction.pc, instruction.a),
          argumentRefs = callArgumentRefs(prototype, instruction),
          returnRefs = callReturnRefs(prototype.prototypeId, instruction)
        )
      }

  private final case class ResolvedCall(
    callsiteId: String,
    prototypeId: String,
    pc: Int,
    resolvedName: Option[String],
    argumentRefs: Vector[String],
    returnRefs: Vector[String]
  )

  private def callTargetNameAt(prototype: LuaPrototype, pc: Int, slot: Int): Option[String] =
    prototype.instructions
      .filter(instruction => instruction.pc < pc && instruction.a == slot)
      .sortBy(_.pc)
      .lastOption
      .flatMap {
        case instruction if instruction.opcode == LuaOpcode.GetGlobal =>
          constantName(prototype.constants, instruction.b)
        case instruction if instruction.opcode == LuaOpcode.GetTable =>
          for {
            baseName <- callTargetNameAt(prototype, instruction.pc, instruction.b)
            field    <- instruction.c.filter(_ >= 256).flatMap(value => constantName(prototype.constants, value - 256))
          } yield s"$baseName.$field"
        case _ => None
      }

  private def callArgumentRefs(prototype: LuaPrototype, instruction: LuaInstruction): Vector[String] = {
    val slots = instruction.b match {
      case 0 => ((instruction.a + 1) until prototype.maxStack).toVector
      case 1 => Vector.empty
      case n => ((instruction.a + 1) until (instruction.a + n)).toVector
    }
    slots.map(slot => s"${prototype.prototypeId}@pc${instruction.pc}:r$slot")
  }

  private def callReturnRefs(prototypeId: String, instruction: LuaInstruction): Vector[String] = {
    val slots = instruction.c match {
      case Some(0) => Vector(instruction.a)
      case Some(1) => Vector.empty
      case Some(n) => (instruction.a until (instruction.a + n - 1)).toVector
      case None    => Vector.empty
    }
    slots.map(slot => s"$prototypeId@pc${instruction.pc}:r$slot")
  }

  private def nodesFor(prototypes: Vector[LuaPrototype]): Vector[ujson.Obj] =
    prototypes.flatMap { prototype =>
      prototype.instructions.map { instruction =>
        ujson.Obj(
          "type"         -> "LUA_BYTECODE_INSTRUCTION",
          "prototype_id" -> prototype.prototypeId,
          "pc"           -> instruction.pc,
          "opcode"       -> instruction.opcode.mnemonic,
          "callsite_id"  -> s"${prototype.prototypeId}@pc${instruction.pc}",
          "reads"        -> readRefs(prototype.prototypeId, instruction),
          "writes"       -> writeRefs(prototype.prototypeId, instruction)
        )
      }
    }

  private def readRefs(prototypeId: String, instruction: LuaInstruction): Vector[String] =
    instruction.opcode match {
      case LuaOpcode.Call | LuaOpcode.TailCall =>
        (Vector(instruction.a) ++ callArgumentSlots(instruction)).map(slot =>
          s"$prototypeId@pc${instruction.pc}:r$slot"
        )
      case LuaOpcode.Return =>
        returnSlots(instruction).map(slot => s"$prototypeId@pc${instruction.pc}:r$slot").toVector
      case _ =>
        (Vector(instruction.b) ++ instruction.c.toVector.filter(_ < 256)).map(slot =>
          s"$prototypeId@pc${instruction.pc}:r$slot"
        )
    }

  private def writeRefs(prototypeId: String, instruction: LuaInstruction): Vector[String] =
    instruction.opcode match {
      case LuaOpcode.Return => Vector.empty
      case _                => Vector(s"$prototypeId@pc${instruction.pc}:r${instruction.a}")
    }

  private def localFlowJson(row: LuaLocalFlow): ujson.Obj =
    ujson.Obj("type" -> "REACHING_DEF", "src" -> row.sourceRef, "dst" -> row.sinkRef, "kind" -> row.edgeKind)

  private def upvalueFlowJson(row: LuaUpvalueFlow): ujson.Obj =
    ujson.Obj(
      "flow_id"     -> s"upvalue:${row.upvalueId}:${row.captureRef}:${row.writeRef}",
      "upvalue_id"  -> row.upvalueId,
      "capture_ref" -> row.captureRef,
      "read_ref"    -> row.readRef,
      "write_ref"   -> row.writeRef,
      "provenance"  -> row.provenance
    )

  private def defusePathJson(relativeName: String)(row: LuaLocalFlow): ujson.Obj =
    ujson.Obj(
      "path_id"            -> s"$relativeName:${row.sourceRef}->${row.sinkRef}",
      "source_ref"         -> row.sourceRef,
      "sink_ref"           -> row.sinkRef,
      "path_steps"         -> Vector(qualify(relativeName, row.sourceRef), qualify(relativeName, row.sinkRef)),
      "first_missing_edge" -> "none",
      "provenance"         -> row.provenance
    )

  private def functionIdentityJson(artifactId: String, relativeName: String)(prototype: LuaPrototype): ujson.Obj =
    ujson.Obj(
      "identity_id"   -> s"$relativeName:${prototype.prototypeId}",
      "artifact_id"   -> artifactId,
      "artifact_role" -> "main",
      "module_path"   -> relativeName,
      "prototype_id"  -> prototype.prototypeId,
      "display_name"  -> prototype.prototypeId,
      "identity_kind" -> "bytecode-prototype-id",
      "provenance"    -> "upstream-lua2cpg,bytecode-only,prototype-identity"
    )

  private def moduleResolutionJson(row: LuaModuleResolution): ujson.Obj =
    ujson.Obj(
      "resolution_id"      -> s"${row.fromModulePath}:${row.requireCallsiteId}:${row.requireString}",
      "callsite_id"        -> scopedCallsite(row.fromModulePath, row.requireCallsiteId),
      "module_path"        -> row.fromModulePath,
      "require_string"     -> row.requireString,
      "resolution_status"  -> row.resolutionStatus,
      "target_module_path" -> row.targetModulePath.getOrElse(""),
      "unresolved_reason"  -> row.unresolvedReason.getOrElse("none"),
      "provenance"         -> row.provenance
    )

  private def moduleReturnTableJson(row: LuaModuleReturnTable): ujson.Obj =
    ujson.Obj(
      "return_table_id"     -> s"${row.modulePath}:${row.tableRef}:${row.fieldName}",
      "module_path"         -> row.modulePath,
      "table_ref"           -> row.tableRef,
      "field_name"          -> row.fieldName,
      "target_prototype_id" -> row.targetPrototypeId,
      "provenance"          -> row.provenance
    )

  private def moduleLinkageRows(relativeName: String, semantics: LuaProgramSemantics): Vector[ujson.Obj] =
    semantics.moduleFieldCallTargets.filter(_.fromModulePath == relativeName).map { row =>
      ujson.Obj(
        "linkage_id"  -> s"${row.fromModulePath}:${row.callsiteId}:${row.targetModulePath}:${row.targetPrototypeId}",
        "callsite_id" -> scopedCallsite(row.fromModulePath, row.callsiteId),
        "module_path" -> row.fromModulePath,
        "target_module_path"  -> row.targetModulePath,
        "target_prototype_id" -> row.targetPrototypeId,
        "field_name"          -> row.fieldName,
        "resolution_status"   -> "matched",
        "provenance"          -> row.provenance
      )
    }

  private def callTargetCandidateRows(
    relativeName: String,
    local: LuaPrototypeSemantics,
    semantics: LuaProgramSemantics
  ): Vector[ujson.Obj] = {
    val localRows = local.callTargetCandidates.map { row =>
      ujson.Obj(
        "candidate_id"      -> s"$relativeName:${row.callsiteId}:${row.targetRef}",
        "callsite_id"       -> scopedCallsite(relativeName, row.callsiteId),
        "module_path"       -> relativeName,
        "target_ref"        -> s"$relativeName::${row.targetRef}",
        "confidence"        -> row.confidence,
        "resolution_status" -> "matched",
        "unresolved_reason" -> "none",
        "provenance"        -> row.provenance
      )
    }
    val crossRows = semantics.crossBoundaryCallTargets.filter(_.fromModulePath == relativeName).map { row =>
      ujson.Obj(
        "candidate_id" -> s"${row.fromModulePath}:${row.callsiteId}:${row.targetModulePath}:${row.targetPrototypeId}",
        "callsite_id"  -> scopedCallsite(row.fromModulePath, row.callsiteId),
        "module_path"  -> row.fromModulePath,
        "target_module_path"  -> row.targetModulePath,
        "target_prototype_id" -> row.targetPrototypeId,
        "target_ref"          -> s"${row.targetModulePath}::${row.targetPrototypeId}",
        "confidence"          -> row.confidence,
        "resolution_status"   -> "matched",
        "unresolved_reason"   -> "none",
        "provenance"          -> row.provenance
      )
    }
    localRows ++ crossRows
  }

  private def interproceduralArgFlowJson(row: LuaInterproceduralArgFlow): ujson.Obj =
    ujson.Obj(
      "flow_id" -> s"arg:${qualifiedCallsite(row.callsiteId, row.fromArgumentRef)}:${row.fromArgumentRef}:${row.toParameterRef}",
      "callsite_id"         -> qualifiedCallsite(row.callsiteId, row.fromArgumentRef),
      "from_argument_ref"   -> row.fromArgumentRef,
      "argument_index"      -> row.argumentIndex,
      "to_parameter_ref"    -> row.toParameterRef,
      "target_module_path"  -> row.targetModulePath,
      "target_prototype_id" -> row.targetPrototypeId,
      "provenance"          -> row.provenance
    )

  private def interproceduralReturnFlowJson(row: LuaInterproceduralReturnFlow): ujson.Obj =
    ujson.Obj(
      "flow_id" -> s"return:${qualifiedCallsite(row.callsiteId, row.callerResultRef)}:${row.calleeReturnRef}:${row.callerResultRef}",
      "callsite_id"         -> qualifiedCallsite(row.callsiteId, row.callerResultRef),
      "callee_return_ref"   -> s"${row.targetModulePath}:${row.calleeReturnRef}",
      "caller_result_ref"   -> row.callerResultRef,
      "target_module_path"  -> row.targetModulePath,
      "target_prototype_id" -> row.targetPrototypeId,
      "provenance"          -> row.provenance
    )

  private def sourceEndpointJson(row: LuaSourceEndpoint): ujson.Obj = {
    val (modulePath, valueRef) = splitQualifiedRef(row.sourceRef)
    ujson.Obj(
      "module_path" -> modulePath,
      "value_ref"   -> valueRef,
      "callsite_id" -> toScopedStepRef(row.callsiteId),
      "trigger"     -> row.trigger,
      "provenance"  -> row.provenance
    )
  }

  private def sinkEndpointJson(row: LuaSinkEndpoint): ujson.Obj = {
    val (modulePath, valueRef) = splitQualifiedRef(row.sinkRef)
    ujson.Obj(
      "module_path" -> modulePath,
      "value_ref"   -> valueRef,
      "callsite_id" -> toScopedStepRef(row.callsiteId),
      "trigger"     -> row.trigger,
      "param_idx"   -> row.parameterIndex,
      "provenance"  -> row.provenance
    )
  }

  private def pathEvidenceRows(relativeName: String, semantics: LuaProgramSemantics): Vector[ujson.Obj] = {
    val exportedNamesByPrototype = semantics.moduleReturnTables
      .groupBy(item => item.modulePath -> item.targetPrototypeId)
      .view
      .mapValues(_.map(_.fieldName).distinct.sorted)
      .collect { case (key, Vector(singleName)) => key -> singleName }
      .toMap
    semantics.taintPaths.filter(_.sourceRef.startsWith(s"$relativeName:")).map { row =>
      val report =
        semantics.reportClassifications.find(item => item.sourceRef == row.sourceRef && item.sinkRef == row.sinkRef)
      val (sourceModule, sourceValue) = splitQualifiedRef(row.sourceRef)
      val (sinkModule, sinkValue)     = splitQualifiedRef(row.sinkRef)
      val sourceEndpoint = semantics.sourceEndpoints
        .find(_.sourceRef == row.sourceRef)
        .getOrElse(
          throw new IllegalStateException(s"taint path lacks source endpoint: ${row.sourceRef}->${row.sinkRef}")
        )
      val sinkEndpoint = semantics.sinkEndpoints
        .find(_.sinkRef == row.sinkRef)
        .getOrElse(throw new IllegalStateException(s"taint path lacks sink endpoint: ${row.sourceRef}->${row.sinkRef}"))
      val sourceCallsite     = unqualifyCallsite(sourceEndpoint.callsiteId)
      val sinkCallsite       = unqualifyCallsite(sinkEndpoint.callsiteId)
      val sourcePrototype    = prototypeIdFromCallsiteId(sourceCallsite)
      val sinkPrototype      = prototypeIdFromCallsiteId(sinkCallsite)
      val sourceFunctionName = displayFunctionName(exportedNamesByPrototype, sourceModule, sourcePrototype)
      val sinkFunctionName   = displayFunctionName(exportedNamesByPrototype, sinkModule, sinkPrototype)
      ujson.Obj(
        "path_id"              -> s"${row.sourceRef}->${row.sinkRef}",
        "baseline_report_id"   -> "",
        "source_module_path"   -> sourceModule,
        "source_function_name" -> sourceFunctionName,
        "source_pc"            -> pcFromCallsiteId(sourceCallsite),
        "source_trigger"       -> sourceEndpoint.trigger,
        "sink_module_path"     -> sinkModule,
        "sink_function_name"   -> sinkFunctionName,
        "sink_pc"              -> pcFromCallsiteId(sinkCallsite),
        "sink_trigger"         -> sinkEndpoint.trigger,
        "sink_param_idx"       -> sinkEndpoint.parameterIndex,
        "source"               -> ujson.Obj("module_path" -> sourceModule, "value_ref" -> sourceValue),
        "sink"                 -> ujson.Obj("module_path" -> sinkModule, "value_ref" -> sinkValue),
        "path_status"          -> "matched",
        "path_step_count"      -> row.pathSteps.size,
        "equivalence_level"    -> "upstream-lua2cpg-native-path",
        "path_steps"           -> row.pathSteps.map(toScopedStepRef),
        "sanitizer_hits"       -> sanitizerHitsFor(row, semantics),
        "classification"       -> report.map(_.classification).getOrElse(row.classification),
        "provenance"           -> row.provenance
      )
    }
  }

  private def validateTaintPathEndpoints(semantics: LuaProgramSemantics): Unit = {
    val sourceRefs = semantics.sourceEndpoints.map(_.sourceRef).toSet
    val sinkRefs   = semantics.sinkEndpoints.map(_.sinkRef).toSet
    semantics.taintPaths.foreach { path =>
      if (!sourceRefs(path.sourceRef)) {
        throw new IllegalStateException(s"taint path lacks source endpoint: ${path.sourceRef}->${path.sinkRef}")
      }
      if (!sinkRefs(path.sinkRef)) {
        throw new IllegalStateException(s"taint path lacks sink endpoint: ${path.sourceRef}->${path.sinkRef}")
      }
    }
  }

  private def displayFunctionName(
    exportedNamesByPrototype: Map[(String, String), String],
    modulePath: String,
    prototypeId: String
  ): String =
    exportedNamesByPrototype.getOrElse(modulePath -> prototypeId, prototypeId)

  private def sanitizerHitsFor(path: LuaTaintPath, semantics: LuaProgramSemantics): ujson.Arr =
    ujson.Arr.from(
      semantics.sanitizerClassifications
        .filter(row => row.sourceRef == path.sourceRef && row.sinkRef == path.sinkRef)
        .map { row =>
          ujson.Obj(
            "callsite_id"       -> toScopedStepRef(row.sanitizerCallsiteId),
            "sanitizer_name"    -> row.sanitizerName,
            "applies_to_sink"   -> row.appliesToSink,
            "on_dataflow_chain" -> row.onDataflowChain
          )
        }
    )

  private def pathSearchProfile(semantics: LuaProgramSemantics): ujson.Obj =
    ujson.Obj(
      "status"                                  -> "completed",
      "source_endpoint_count"                   -> semantics.sourceEndpoints.size,
      "sink_endpoint_count"                     -> semantics.sinkEndpoints.size,
      "taint_path_count"                        -> semantics.taintPaths.size,
      "sanitizer_classification_count"          -> semantics.sanitizerClassifications.size,
      "report_count"                            -> semantics.vulnerabilityReports.size,
      "local_path_graph_module_count"           -> semantics.pathSearchStats.localPathGraphModuleCount,
      "local_path_graph_build_count"            -> semantics.pathSearchStats.localPathGraphBuildCount,
      "local_path_search_count"                 -> semantics.pathSearchStats.localPathSearchCount,
      "distinct_local_path_query_count"         -> semantics.pathSearchStats.distinctLocalPathQueryCount,
      "source_sink_pair_count"                  -> semantics.pathSearchStats.sourceSinkPairCount,
      "qualified_source_sink_pair_count"        -> semantics.pathSearchStats.qualifiedSourceSinkPairCount,
      "prototype_pruned_source_sink_pair_count" -> semantics.pathSearchStats.prototypePrunedSourceSinkPairCount
    )

  private def profileJson(profile: LuaBytecodeProfile): ujson.Obj =
    ujson.Obj(
      "lua_version"      -> profile.luaVersion,
      "bytecode_version" -> profile.bytecodeVersion,
      "format"           -> profile.format,
      "endianness"       -> profile.endianness,
      "int_size"         -> profile.intSize,
      "size_t_size"      -> profile.sizeTSize,
      "instruction_size" -> profile.instructionSize,
      "lua_number_size"  -> profile.luaNumberSize,
      "number_mode"      -> profile.numberMode,
      "profile_id"       -> profile.profileId
    )

  private def allPrototypes(prototype: LuaPrototype): Vector[LuaPrototype] =
    prototype +: prototype.nested.flatMap(allPrototypes)

  private def countCallsites(prototype: LuaPrototype): Int =
    prototype.instructions.count(instruction =>
      instruction.opcode == LuaOpcode.Call || instruction.opcode == LuaOpcode.TailCall
    )

  private def callArgumentSlots(instruction: LuaInstruction): Vector[Int] =
    instruction.b match {
      case 0 => Vector(instruction.a + 1)
      case 1 => Vector.empty
      case n => ((instruction.a + 1) until (instruction.a + n)).toVector
    }

  private def returnSlots(instruction: LuaInstruction): Seq[Int] =
    instruction.b match {
      case 0 => Seq.empty
      case 1 => Seq.empty
      case n => instruction.a until (instruction.a + n - 1)
    }

  private def constantName(constants: Vector[LuaConstant], index: Int): Option[String] =
    constants.collectFirst { case LuaConstant(`index`, "string", LuaConstantValue.StringValue(value)) =>
      value
    }

  private def toScopedStepRef(valueRef: String): String = {
    val (modulePath, localRef) = splitQualifiedRef(valueRef)
    s"$modulePath::$localRef"
  }

  private def qualify(modulePath: String, ref: String): String = s"$modulePath:$ref"

  private def qualifiedCallsite(callsiteId: String, valueRef: String): String =
    s"${splitQualifiedRef(valueRef)._1}::$callsiteId"

  private def scopedCallsite(modulePath: String, callsiteId: String): String =
    s"$modulePath::$callsiteId"

  private def unqualifyCallsite(ref: String): String =
    ref.split(':').lastOption.getOrElse(ref)

  private def splitQualifiedRef(ref: String): (String, String) = {
    val splitAt = Vector(".luac:")
      .flatMap(marker => {
        val index = ref.indexOf(marker)
        if (index >= 0) Some(index + marker.length - 1) else None
      })
      .headOption
      .getOrElse(throw new IllegalArgumentException(s"Lua qualified ref is missing module path: $ref"))
    (ref.substring(0, splitAt), ref.substring(splitAt + 1))
  }

  private def prototypeIdFromValueRef(ref: String): String =
    ref.split("@pc", 2).headOption.getOrElse(ref)

  private def pcFromValueRef(ref: String): Int =
    ref.split("@pc", 2).lift(1).flatMap(_.split(":r", 2).headOption).flatMap(_.toIntOption).getOrElse(0)

  private def prototypeIdFromCallsiteId(ref: String): String =
    ref.split("@pc", 2).headOption.getOrElse(ref)

  private def pcFromCallsiteId(ref: String): Int =
    ref.split("@pc", 2).lift(1).flatMap(_.toIntOption).getOrElse(0)

  private def artifactIdFor(relativeName: String): String = s"sha256:${relativeName}"

  private def safeArtifactName(artifactId: String): String =
    artifactId.map {
      case character if character.isLetterOrDigit => character
      case _                                      => '_'
    }

  private def writeJson(path: Path, value: ujson.Value): Unit = {
    Files.createDirectories(path.getParent)
    Files.write(path, (ujson.write(value, indent = 2) + "\n").getBytes(StandardCharsets.UTF_8))
  }
}
