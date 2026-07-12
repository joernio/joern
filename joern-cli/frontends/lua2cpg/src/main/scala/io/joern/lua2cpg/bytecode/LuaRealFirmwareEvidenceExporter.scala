package io.joern.lua2cpg.bytecode

import io.joern.lua2cpg.Config
import io.joern.lua2cpg.passes.LuaBytecodeModelPass.DecodedBytecode

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*

object LuaRealFirmwareEvidenceExporter {
  private val MaxRetainedPairProfileBytes = 4096L

  def write(config: Config, decoded: Vector[DecodedBytecode], semantics: LuaProgramSemantics): Unit =
    config.realFirmwareOutputDir.foreach { outputDir =>
      validateTaintPathEndpoints(semantics)
      val profile    = pathSearchProfile(semantics)
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
      writeJson(root.resolve("path-search-profile.json"), profile)
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

    val exportedNamesByPrototype = exportedFunctionNamesByPrototype(semantics)
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
      "function_identity" -> prototypes.map(functionIdentityJson(artifactId, relativeName, exportedNamesByPrototype)),
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

  private def functionIdentityJson(
    artifactId: String,
    relativeName: String,
    exportedNamesByPrototype: Map[(String, String), String]
  )(prototype: LuaPrototype): ujson.Obj = {
    val identity = functionDisplayIdentity(exportedNamesByPrototype, relativeName, prototype.prototypeId)
    ujson.Obj(
      "identity_id"   -> s"$relativeName:${prototype.prototypeId}",
      "artifact_id"   -> artifactId,
      "artifact_role" -> "main",
      "module_path"   -> relativeName,
      "prototype_id"  -> prototype.prototypeId,
      "display_name"  -> identity.displayName,
      "identity_kind" -> identity.identityKind,
      "provenance"    -> identity.provenance
    )
  }

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
    val exportedNamesByPrototype = exportedFunctionNamesByPrototype(semantics)
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
    functionDisplayIdentity(exportedNamesByPrototype, modulePath, prototypeId).displayName

  private final case class FunctionDisplayIdentity(displayName: String, identityKind: String, provenance: String)

  private def exportedFunctionNamesByPrototype(semantics: LuaProgramSemantics): Map[(String, String), String] =
    semantics.moduleReturnTables
      .groupBy(item => item.modulePath -> item.targetPrototypeId)
      .view
      .mapValues(_.map(_.fieldName).distinct.sorted)
      .collect { case (key, Vector(singleName)) => key -> singleName }
      .toMap

  private def functionDisplayIdentity(
    exportedNamesByPrototype: Map[(String, String), String],
    modulePath: String,
    prototypeId: String
  ): FunctionDisplayIdentity =
    exportedNamesByPrototype.get(modulePath -> prototypeId) match {
      case Some(displayName) =>
        FunctionDisplayIdentity(displayName, "synthetic", "upstream-lua2cpg,bytecode-only,synthetic-name")
      case None =>
        syntheticPrototypeDisplayName(prototypeId) match {
          case Some(displayName) =>
            FunctionDisplayIdentity(displayName, "synthetic", "upstream-lua2cpg,bytecode-only,synthetic-name")
          case None =>
            FunctionDisplayIdentity(
              prototypeId,
              "bytecode-prototype-id",
              "upstream-lua2cpg,bytecode-only,prototype-identity"
            )
        }
    }

  private def syntheticPrototypeDisplayName(prototypeId: String): Option[String] =
    if (prototypeId == "root") None
    else {
      val prefix = "root."
      if (!prototypeId.startsWith(prefix)) {
        throw new IllegalArgumentException(
          s"unsupported Lua prototype id for synthetic function identity: $prototypeId"
        )
      }
      val ordinalPath = prototypeId.stripPrefix(prefix).split('.').toVector
      if (ordinalPath.exists(_.forall(_.isDigit) == false)) {
        throw new IllegalArgumentException(s"non-numeric Lua prototype ordinal path: $prototypeId")
      }
      Some(s"func_unknow_0_${ordinalPath.mkString("_")}")
    }

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

  private def pathSearchProfile(semantics: LuaProgramSemantics): ujson.Obj = {
    validatePerformanceAttribution(semantics)
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
      "prototype_pruned_source_sink_pair_count" -> semantics.pathSearchStats.prototypePrunedSourceSinkPairCount,
      "performance_attribution"                 -> performanceAttributionJson(semantics.performanceAttribution)
    )
  }

  private val PairCounterNames = Set(
    "source_reachability_check_count",
    "source_reachability_accepted_count",
    "prototype_unreachable_pair_count",
    "source_specific_provenance_pruned_pair_count",
    "parameter_position_check_count",
    "parameter_position_accepted_count",
    "parameter_position_pruned_count",
    "path_constructor_check_count",
    "path_constructor_accepted_count",
    "path_constructor_pruned_count",
    "bridge_argument_provenance_candidate_count",
    "bridge_candidate_pc_pruned_count",
    "bridge_candidate_reachability_pruned_count",
    "bridge_local_path_attempt_count",
    "bridge_local_path_success_count",
    "local_path_search_count",
    "distinct_local_path_query_count",
    "local_path_cache_hit_count",
    "local_path_cache_miss_count",
    "local_path_graph_build_count",
    "local_path_graph_cache_hit_count",
    "local_path_graph_cache_miss_count",
    "bridge_path_cache_hit_count",
    "bridge_path_cache_miss_count",
    "targeted_search_node_visit_count",
    "targeted_search_edge_visit_count",
    "early_candidate_short_circuit_count",
    "taint_path_count",
    "report_count"
  )

  private def performanceAttributionJson(attribution: LuaPerformanceAttribution): ujson.Obj = {
    val aggregate                                = attribution.aggregateCounters
    def aggregateNumber(name: String): ujson.Num = ujson.Num(aggregate(name).toDouble)
    val pairProfiles                             = pairProfilesJson(attribution.pairProfiles)
    ujson.Obj(
      "schema"                           -> "lua-r7-performance-attribution-v1",
      "unattributed_changed_family_work" -> ujson.Num(attribution.unattributedChangedFamilyWork.toDouble),
      "retained_pair_profile_count"      -> attribution.pairProfiles.size,
      "retained_pair_profile_bytes" -> ujson
        .write(pairProfiles)
        .getBytes(StandardCharsets.UTF_8)
        .length,
      "rows" -> ujson.Obj(
        "P1" -> ujson.Obj(
          "candidate_count" -> ujson.Num(attribution.p1CandidateCount.toDouble),
          "rejected_count"  -> ujson.Num(attribution.p1RejectedCount.toDouble),
          "accepted_count"  -> ujson.Num(attribution.p1AcceptedCount.toDouble)
        ),
        "P2" -> aggregateRow(
          aggregate,
          "parameter_position_check_count",
          "parameter_position_accepted_count",
          "parameter_position_pruned_count"
        ),
        "P3" -> ujson.Obj(
          "candidate_count"           -> aggregateNumber("source_reachability_check_count"),
          "accepted_count"            -> aggregateNumber("source_reachability_accepted_count"),
          "prototype_rejected_count"  -> aggregateNumber("prototype_unreachable_pair_count"),
          "provenance_rejected_count" -> aggregateNumber("source_specific_provenance_pruned_pair_count")
        ),
        "P4" -> ujson.Obj(
          "candidate_count"             -> aggregateNumber("bridge_argument_provenance_candidate_count"),
          "pc_rejected_count"           -> aggregateNumber("bridge_candidate_pc_pruned_count"),
          "reachability_rejected_count" -> aggregateNumber("bridge_candidate_reachability_pruned_count"),
          "continued_count" -> ujson.Num(
            (aggregate("bridge_argument_provenance_candidate_count") - aggregate(
              "bridge_candidate_pc_pruned_count"
            ) - aggregate("bridge_candidate_reachability_pruned_count")).toDouble
          ),
          "path_constructor_candidate_count" -> aggregateNumber("path_constructor_check_count"),
          "path_constructor_accepted_count"  -> aggregateNumber("path_constructor_accepted_count"),
          "path_constructor_rejected_count"  -> aggregateNumber("path_constructor_pruned_count")
        ),
        "P5" -> ujson.Obj(
          "node_visit_count" -> aggregateNumber("targeted_search_node_visit_count"),
          "edge_visit_count" -> aggregateNumber("targeted_search_edge_visit_count")
        ),
        "P6" -> ujson.Obj(
          "local_path_cache_hit_count"        -> aggregateNumber("local_path_cache_hit_count"),
          "local_path_cache_miss_count"       -> aggregateNumber("local_path_cache_miss_count"),
          "local_path_graph_cache_hit_count"  -> aggregateNumber("local_path_graph_cache_hit_count"),
          "local_path_graph_cache_miss_count" -> aggregateNumber("local_path_graph_cache_miss_count"),
          "bridge_path_cache_hit_count"       -> aggregateNumber("bridge_path_cache_hit_count"),
          "bridge_path_cache_miss_count"      -> aggregateNumber("bridge_path_cache_miss_count")
        ),
        "P7" -> ujson.Obj("status" -> "not-invoked-no-reuse", "invocation_count" -> 0, "reuse_count" -> 0),
        "early_short_circuit" -> ujson.Obj(
          "count"                       -> aggregateNumber("early_candidate_short_circuit_count"),
          "pc_rejected_count"           -> aggregateNumber("bridge_candidate_pc_pruned_count"),
          "reachability_rejected_count" -> aggregateNumber("bridge_candidate_reachability_pruned_count")
        )
      ),
      "pair_profiles" -> pairProfiles
    )
  }

  private def pairProfilesJson(rows: Vector[LuaPairPerformanceProfile]): ujson.Arr =
    ujson.Arr.from(rows.map { row =>
      ujson.Obj.from(
        Vector(
          "pair_id"            -> ujson.Str(row.pairId),
          "source_ref"         -> ujson.Str(row.sourceRef),
          "sink_ref"           -> ujson.Str(row.sinkRef),
          "source_callsite_id" -> ujson.Str(row.sourceCallsiteId),
          "sink_callsite_id"   -> ujson.Str(row.sinkCallsiteId),
          "source_trigger"     -> ujson.Str(row.sourceTrigger),
          "sink_trigger"       -> ujson.Str(row.sinkTrigger)
        ) ++ row.counters.toVector.sortBy(_._1).map { case (name, value) => name -> ujson.Num(value.toDouble) }
      )
    })

  private def aggregateRow(count: Map[String, Long], total: String, accepted: String, rejected: String): ujson.Obj =
    ujson.Obj(
      "candidate_count" -> ujson.Num(count(total).toDouble),
      "accepted_count"  -> ujson.Num(count(accepted).toDouble),
      "rejected_count"  -> ujson.Num(count(rejected).toDouble)
    )

  private def validatePerformanceAttribution(semantics: LuaProgramSemantics): Unit = {
    val attribution = semantics.performanceAttribution
    def requireCount(condition: Boolean, message: String): Unit =
      if (!condition) throw new IllegalStateException(s"invalid Lua performance attribution: $message")

    requireCount(
      attribution.p1CandidateCount == attribution.p1RejectedCount + attribution.p1AcceptedCount,
      "P1 candidate count does not partition into accepted and rejected counts"
    )
    requireCount(
      attribution.unattributedChangedFamilyWork == 0L,
      s"unattributed changed-family work is ${attribution.unattributedChangedFamilyWork}"
    )
    requireCount(
      attribution.aggregateCounters.keySet == PairCounterNames,
      "aggregate counter keys do not exactly match the required schema"
    )
    val continuedBridgeCandidates =
      attribution.aggregateCounters("bridge_argument_provenance_candidate_count") -
        attribution.aggregateCounters("early_candidate_short_circuit_count")
    val requiresPairProfiles = attribution.aggregateCounters("local_path_search_count") > 0L ||
      attribution.aggregateCounters("taint_path_count") > 0L || continuedBridgeCandidates > 0L
    requireCount(
      !requiresPairProfiles || attribution.pairProfiles.nonEmpty,
      "pair profiles are empty despite retained pair work"
    )
    validateCounterPartitions(attribution.aggregateCounters, "aggregate")
    requireCount(
      attribution.pairProfiles.map(_.pairId).distinct.size == attribution.pairProfiles.size,
      "pair identities are not unique"
    )
    requireCount(
      attribution.pairProfiles.size <= semantics.pathSearchStats.localPathSearchCount + semantics.taintPaths.size,
      "retained pair profile count exceeds local-search plus taint-path bound"
    )
    val retainedPairProfileBytes = ujson
      .write(pairProfilesJson(attribution.pairProfiles))
      .getBytes(StandardCharsets.UTF_8)
      .length
    requireCount(
      attribution.pairProfiles.isEmpty ||
        retainedPairProfileBytes <= attribution.pairProfiles.size.toLong * MaxRetainedPairProfileBytes,
      s"retained pair profile payload exceeds $MaxRetainedPairProfileBytes bytes per row"
    )
    requireCount(
      attribution.aggregateCounters("source_reachability_check_count") == semantics.pathSearchStats.sourceSinkPairCount,
      "aggregate source reachability count does not match legacy source-sink pair count"
    )
    requireCount(
      attribution.aggregateCounters(
        "parameter_position_accepted_count"
      ) == semantics.pathSearchStats.qualifiedSourceSinkPairCount,
      "aggregate parameter-position accepted count does not match legacy qualified pair count"
    )
    requireCount(
      attribution.aggregateCounters("local_path_search_count") == semantics.pathSearchStats.localPathSearchCount,
      "aggregate local path search count does not match legacy count"
    )
    requireCount(
      attribution.aggregateCounters(
        "distinct_local_path_query_count"
      ) == semantics.pathSearchStats.distinctLocalPathQueryCount,
      "aggregate distinct local path query count does not match legacy count"
    )
    requireCount(
      attribution.aggregateCounters(
        "local_path_graph_build_count"
      ) == semantics.pathSearchStats.localPathGraphBuildCount,
      "aggregate local path graph build count does not match legacy count"
    )
    attribution.pairProfiles.foreach { row =>
      val count = row.counters
      requireCount(
        count.keySet == PairCounterNames,
        s"counter keys do not exactly match the required schema for pair ${row.pairId}"
      )
      requireIdentity(row)
      requireCount(
        count("local_path_search_count") > 0L || count("taint_path_count") > 0L,
        s"retained pair has neither local-search nor taint-path work for pair ${row.pairId}"
      )
      requireCount(count.values.forall(_ >= 0L), s"negative counter for pair ${row.pairId}")
      requireCount(
        count("source_reachability_check_count") == count("source_reachability_accepted_count") +
          count("prototype_unreachable_pair_count") + count("source_specific_provenance_pruned_pair_count"),
        s"source reachability partition mismatch for pair ${row.pairId}"
      )
      requireCount(
        count("parameter_position_check_count") == count("parameter_position_accepted_count") +
          count("parameter_position_pruned_count"),
        s"parameter position partition mismatch for pair ${row.pairId}"
      )
      requireCount(
        count("path_constructor_check_count") == count("path_constructor_accepted_count") +
          count("path_constructor_pruned_count"),
        s"path constructor partition mismatch for pair ${row.pairId}"
      )
      requireCount(
        count("bridge_argument_provenance_candidate_count") >= count("bridge_candidate_pc_pruned_count") +
          count("bridge_candidate_reachability_pruned_count"),
        s"bridge candidate partition mismatch for pair ${row.pairId}"
      )
      requireCount(
        count("early_candidate_short_circuit_count") == count("bridge_candidate_pc_pruned_count") +
          count("bridge_candidate_reachability_pruned_count"),
        s"early short-circuit partition mismatch for pair ${row.pairId}"
      )
      requireCount(
        count("local_path_search_count") == count("local_path_cache_hit_count") +
          count("local_path_cache_miss_count"),
        s"local path cache partition mismatch for pair ${row.pairId}"
      )
      requireCount(
        count("distinct_local_path_query_count") == count("local_path_cache_miss_count"),
        s"distinct local path query mismatch for pair ${row.pairId}"
      )
      requireCount(
        count("local_path_graph_build_count") == count("local_path_graph_cache_miss_count"),
        s"local path graph build mismatch for pair ${row.pairId}"
      )
      requireCount(
        count("taint_path_count") == semantics.taintPaths.count(path =>
          path.sourceRef == row.sourceRef && path.sinkRef == row.sinkRef
        ),
        s"path reconciliation mismatch for pair ${row.pairId}"
      )
      requireCount(
        count("report_count") == semantics.reportClassifications.count(report =>
          report.sourceRef == row.sourceRef && report.sinkRef == row.sinkRef
        ),
        s"report reconciliation mismatch for pair ${row.pairId}"
      )
    }
  }

  private def validateCounterPartitions(count: Map[String, Long], identity: String): Unit = {
    def requireCount(condition: Boolean, message: String): Unit =
      if (!condition) throw new IllegalStateException(s"invalid Lua performance attribution: $message")
    requireCount(count.values.forall(_ >= 0L), s"negative counter for $identity")
    requireCount(
      count("source_reachability_check_count") == count("source_reachability_accepted_count") + count(
        "prototype_unreachable_pair_count"
      ) + count("source_specific_provenance_pruned_pair_count"),
      s"source reachability partition mismatch for $identity"
    )
    requireCount(
      count("parameter_position_check_count") == count("parameter_position_accepted_count") + count(
        "parameter_position_pruned_count"
      ),
      s"parameter position partition mismatch for $identity"
    )
    requireCount(
      count("path_constructor_check_count") == count("path_constructor_accepted_count") + count(
        "path_constructor_pruned_count"
      ),
      s"path constructor partition mismatch for $identity"
    )
    requireCount(
      count("bridge_argument_provenance_candidate_count") >= count("bridge_candidate_pc_pruned_count") + count(
        "bridge_candidate_reachability_pruned_count"
      ),
      s"bridge candidate partition mismatch for $identity"
    )
    requireCount(
      count("early_candidate_short_circuit_count") == count("bridge_candidate_pc_pruned_count") + count(
        "bridge_candidate_reachability_pruned_count"
      ),
      s"early short-circuit partition mismatch for $identity"
    )
    requireCount(
      count("local_path_search_count") == count("local_path_cache_hit_count") + count("local_path_cache_miss_count"),
      s"local path cache partition mismatch for $identity"
    )
    requireCount(
      count("distinct_local_path_query_count") == count("local_path_cache_miss_count"),
      s"distinct local path query mismatch for $identity"
    )
    requireCount(
      count("local_path_graph_build_count") == count("local_path_graph_cache_miss_count"),
      s"local path graph build mismatch for $identity"
    )
  }

  private def requireIdentity(row: LuaPairPerformanceProfile): Unit = {
    def invalid(message: String): Nothing =
      throw new IllegalStateException(s"invalid Lua performance attribution: $message for pair ${row.pairId}")
    val ValueRef = raw"(.+):([^:]+)@pc([0-9]+):r([0-9]+)".r
    val Callsite = raw"(.+)::([^:]+)@pc([0-9]+)".r
    def validate(ref: String, callsite: String, side: String): Unit = (ref, callsite) match {
      case (ValueRef(refModule, refPrototype, refPc, _), Callsite(callModule, callPrototype, callPc))
          if refModule == callModule && refPrototype == callPrototype && refPc == callPc =>
      case _ => invalid(s"malformed or mismatched $side identity")
    }
    if (row.sourceTrigger.isEmpty || row.sinkTrigger.isEmpty) invalid("empty trigger")
    validate(row.sourceRef, row.sourceCallsiteId, "source")
    validate(row.sinkRef, row.sinkCallsiteId, "sink")
  }

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
