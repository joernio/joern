package io.joern.lua2cpg.bytecode

final case class LuaModuleResolution(
  requireCallsiteId: String,
  requireString: String,
  resolutionStatus: String,
  fromModulePath: String,
  targetModulePath: Option[String],
  unresolvedReason: Option[String],
  provenance: String
)

final case class LuaModuleReturnTable(
  modulePath: String,
  tableRef: String,
  fieldName: String,
  targetPrototypeId: String,
  provenance: String
)

final case class LuaModuleFieldCallTarget(
  fromModulePath: String,
  callsiteId: String,
  fieldName: String,
  targetModulePath: String,
  targetPrototypeId: String,
  provenance: String
)

final case class LuaInterproceduralArgFlow(
  callsiteId: String,
  fromArgumentRef: String,
  argumentIndex: Int,
  targetModulePath: String,
  targetPrototypeId: String,
  toParameterRef: String,
  provenance: String
)

final case class LuaInterproceduralReturnFlow(
  callsiteId: String,
  targetModulePath: String,
  targetPrototypeId: String,
  calleeReturnRef: String,
  callerResultRef: String,
  provenance: String
)

final case class LuaCrossBoundaryCallTarget(
  fromModulePath: String,
  callsiteId: String,
  targetModulePath: String,
  targetPrototypeId: String,
  confidence: String,
  provenance: String
)

final case class LuaTaintPath(
  sourceRef: String,
  sinkRef: String,
  pathSteps: Vector[String],
  classification: String,
  provenance: String
)

final case class LuaE4Boundary(boundaryId: String, boundaryKind: String, reason: String)

final case class LuaRuleMatch(
  callsiteId: String,
  ruleKind: String,
  trigger: String,
  matchedName: String,
  parameterIndex: Option[Int],
  provenance: String
)

final case class LuaSourceEndpoint(sourceRef: String, callsiteId: String, trigger: String, provenance: String)

final case class LuaSinkEndpoint(
  sinkRef: String,
  callsiteId: String,
  trigger: String,
  parameterIndex: Int,
  provenance: String
)

final case class LuaSanitizerCall(
  callsiteId: String,
  sanitizerName: String,
  sanitizedValueRef: String,
  provenance: String
)

final case class LuaSanitizerClassification(
  sourceRef: String,
  sinkRef: String,
  sanitizerCallsiteId: String,
  sanitizerName: String,
  appliesToSink: Boolean,
  onDataflowChain: Boolean,
  classification: String
)

final case class LuaReportClassification(sourceRef: String, sinkRef: String, classification: String, reason: String)

final case class LuaVulnerabilityReport(
  sourceRef: String,
  sinkRef: String,
  pathStatus: String,
  classification: String,
  pathSteps: Vector[String],
  provenance: String
)

final case class LuaE5Boundary(boundaryId: String, boundaryKind: String, reason: String)

final case class LuaProgramSemantics(
  moduleResolutions: Vector[LuaModuleResolution],
  moduleReturnTables: Vector[LuaModuleReturnTable],
  moduleFieldCallTargets: Vector[LuaModuleFieldCallTarget],
  interproceduralArgFlows: Vector[LuaInterproceduralArgFlow],
  interproceduralReturnFlows: Vector[LuaInterproceduralReturnFlow],
  crossBoundaryCallTargets: Vector[LuaCrossBoundaryCallTarget],
  taintPaths: Vector[LuaTaintPath],
  boundaries: Vector[LuaE4Boundary],
  ruleMatches: Vector[LuaRuleMatch],
  sourceEndpoints: Vector[LuaSourceEndpoint],
  sinkEndpoints: Vector[LuaSinkEndpoint],
  sanitizerCalls: Vector[LuaSanitizerCall],
  sanitizerClassifications: Vector[LuaSanitizerClassification],
  reportClassifications: Vector[LuaReportClassification],
  vulnerabilityReports: Vector[LuaVulnerabilityReport],
  e5Boundaries: Vector[LuaE5Boundary],
  pathSearchStats: LuaPathSearchStats,
  performanceAttribution: LuaPerformanceAttribution
)

final case class LuaPairPerformanceProfile(
  sourceRef: String,
  sinkRef: String,
  sourceCallsiteId: String,
  sinkCallsiteId: String,
  sourceTrigger: String,
  sinkTrigger: String,
  counters: Map[String, Long]
) {
  val pairId: String =
    s"$sourceRef|$sourceCallsiteId|$sourceTrigger->$sinkRef|$sinkCallsiteId|$sinkTrigger"
}

final case class LuaPerformanceAttribution(
  p1CandidateCount: Long,
  p1RejectedCount: Long,
  p1AcceptedCount: Long,
  unattributedChangedFamilyWork: Long,
  pairProfiles: Vector[LuaPairPerformanceProfile],
  aggregateCounters: Map[String, Long] = Map.empty
)

final case class LuaPathSearchStats(
  localPathGraphModuleCount: Int,
  localPathGraphBuildCount: Int,
  localPathSearchCount: Int,
  distinctLocalPathQueryCount: Int,
  sourceSinkPairCount: Int,
  qualifiedSourceSinkPairCount: Int,
  prototypePrunedSourceSinkPairCount: Int
)

object LuaProgramSemantics {
  private val RkConstantBase     = 256
  private val Provenance         = "bytecode-only"
  private val BoundaryProvenance = "bytecode-boundary"
  private val RealFirmwareSanitizerSuffixes = Set(
    "shellquote",
    "tonumber",
    "parseCmdline",
    "_strformat",
    "_cmdformat",
    "macaddr",
    "macFormat",
    "ip4addr",
    "injection_test",
    "check_iface_name",
    "includeQuote",
    "checkTime",
    "doShell",
    "checkIp",
    "includeXxs",
    "filterExecShell",
    "binaryBase64Enc",
    "decCiphertext",
    "sha256",
    "setMacFilter",
    "setIpFilter",
    "apcli_get_connect",
    "setWifiAPMode",
    "cmdSafeCheck",
    "checkLanIpMask",
    "ipaddr",
    "lan_wan_ip_conflict_chk",
    "licenseActivated",
    "is_activated",
    "check_mac",
    "set_mac_filter",
    "encode",
    "getDstRule",
    "local_dev_data_check",
    "stat",
    "check_if_whitelist_opcode",
    "param_safety_check",
    "sqlite3_db_execute",
    "getStorageMountPathByUuid",
    "getWanIfname",
    "hackCharsCheck",
    "open",
    "del",
    "ip4mac",
    "match",
    "r29_0",
    "r3_0",
    "filePathGet",
    "checkPort",
    "setPTRules",
    "apcli_get_ifname_form_band"
  )
  private val PairCounterNames = Vector(
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

  private final class PerformanceAttributionCollector {
    private final case class PairKey(source: LuaSourceEndpoint, sink: LuaSinkEndpoint)
    private final case class ActivePair(
      key: PairKey,
      var counters: scala.collection.mutable.Map[String, Long],
      var bridgeCandidates: Long = 0L,
      var bridgePcPruned: Long = 0L,
      var bridgeReachabilityPruned: Long = 0L,
      var earlyShortCircuits: Long = 0L,
      var producedPath: Boolean = false,
      var pathAccepted: Long = 0L,
      var pathPruned: Long = 0L
    )
    private val profiles =
      scala.collection.mutable.LinkedHashMap.empty[PairKey, scala.collection.mutable.Map[String, Long]]
    private val aggregates                     = scala.collection.mutable.Map.empty[String, Long]
    private var activePair: Option[ActivePair] = None
    private var p1Candidates                   = 0L
    private var p1Rejected                     = 0L
    private var p1Accepted                     = 0L
    private var unattributed                   = 0L

    def recordP1(accepted: Boolean): Unit = {
      p1Candidates += 1
      if (accepted) p1Accepted += 1 else p1Rejected += 1
    }

    def withPair[A](source: LuaSourceEndpoint, sink: LuaSinkEndpoint)(work: => A): A = {
      val previous = activePair
      val key      = PairKey(source, sink)
      val current = ActivePair(
        key,
        scala.collection.mutable.Map(
          "source_reachability_check_count"    -> 1L,
          "source_reachability_accepted_count" -> 1L,
          "path_constructor_check_count"       -> 1L,
          "path_constructor_accepted_count"    -> 1L,
          "parameter_position_check_count"     -> 1L,
          "parameter_position_accepted_count"  -> 1L
        )
      )
      activePair = Some(current)
      try work
      finally {
        val bridgeCandidates = current.counters.getOrElse("bridge_argument_provenance_candidate_count", 0L)
        val shortCircuits    = current.counters.getOrElse("early_candidate_short_circuit_count", 0L)
        val localSearches    = current.counters.getOrElse("local_path_search_count", 0L)
        if (current.producedPath || (bridgeCandidates > shortCircuits && localSearches > 0L)) {
          if (profiles.contains(key)) throw new IllegalStateException(s"duplicate attributed pair: ${pairId(key)}")
          profiles += key -> current.counters
        }
        activePair = previous
      }
    }

    def increment(name: String, amount: Long = 1L): Unit = {
      increment(aggregates, name, amount)
      activePair match {
        case Some(current) if current.counters != null => increment(current.counters, name, amount)
        case Some(current)                             => incrementPending(current, name, amount)
        case None                                      => unattributed += amount
      }
    }

    def incrementAggregate(name: String, amount: Long = 1L): Unit = increment(aggregates, name, amount)

    def markPathProduced(): Unit =
      activePair match {
        case Some(current) => current.producedPath = true
        case None          => throw new IllegalStateException("path produced without active attributed pair")
      }

    def materializeActivePair(): Unit =
      activePair match {
        case Some(current) if current.counters == null =>
          current.counters = scala.collection.mutable.Map(
            "path_constructor_accepted_count"            -> current.pathAccepted,
            "path_constructor_pruned_count"              -> current.pathPruned,
            "bridge_argument_provenance_candidate_count" -> current.bridgeCandidates,
            "bridge_candidate_pc_pruned_count"           -> current.bridgePcPruned,
            "bridge_candidate_reachability_pruned_count" -> current.bridgeReachabilityPruned,
            "early_candidate_short_circuit_count"        -> current.earlyShortCircuits
          )
        case Some(_) =>
        case None    => unattributed += 1
      }

    def incrementFor(source: LuaSourceEndpoint, sink: LuaSinkEndpoint, name: String): Unit =
      increment(
        profiles.getOrElse(PairKey(source, sink), throw new IllegalStateException("missing attributed pair")),
        name,
        1L
      )

    def result(reports: Vector[LuaReportClassification]): LuaPerformanceAttribution = {
      profiles.foreach { case (key, counters) =>
        val reportCount =
          reports.count(report => report.sourceRef == key.source.sourceRef && report.sinkRef == key.sink.sinkRef)
        if (reportCount > 0) increment(counters, "report_count", reportCount.toLong)
      }
      val rows = profiles.toVector.map { case (key, values) =>
        LuaPairPerformanceProfile(
          key.source.sourceRef,
          key.sink.sinkRef,
          scopedCallsite(key.source.callsiteId),
          scopedCallsite(key.sink.callsiteId),
          key.source.trigger,
          key.sink.trigger,
          PairCounterNames.map(name => name -> values.getOrElse(name, 0L)).toMap
        )
      }
      LuaPerformanceAttribution(
        p1Candidates,
        p1Rejected,
        p1Accepted,
        unattributed,
        rows,
        PairCounterNames.map(name => name -> aggregates.getOrElse(name, 0L)).toMap
      )
    }

    private def increment(values: scala.collection.mutable.Map[String, Long], name: String, amount: Long): Unit =
      values.update(name, values.getOrElse(name, 0L) + amount)

    private def incrementPending(current: ActivePair, name: String, amount: Long): Unit =
      name match {
        case "path_constructor_check_count"               =>
        case "path_constructor_accepted_count"            => current.pathAccepted += amount
        case "path_constructor_pruned_count"              => current.pathPruned += amount
        case "bridge_argument_provenance_candidate_count" => current.bridgeCandidates += amount
        case "bridge_candidate_pc_pruned_count"           => current.bridgePcPruned += amount
        case "bridge_candidate_reachability_pruned_count" => current.bridgeReachabilityPruned += amount
        case "early_candidate_short_circuit_count"        => current.earlyShortCircuits += amount
        case _                                            => unattributed += amount
      }

    private def scopedCallsite(ref: String): String = {
      val split = parseQualifiedValueRef(ref)
      s"${split.modulePath}::${split.localRef.split(":r", 2).head}"
    }

    private def pairId(key: PairKey): String =
      s"${key.source.sourceRef}|${key.source.callsiteId}|${key.source.trigger}->${key.sink.sinkRef}|${key.sink.callsiteId}|${key.sink.trigger}"
  }

  def normalize(artifacts: Vector[(String, LuaBytecodeDecodeResult)]): LuaProgramSemantics = {
    val attribution = new PerformanceAttributionCollector
    val accepted = artifacts.collect {
      case (path, result) if result.artifact.accepted =>
        ProgramArtifact(path, result.root)
    }
    val modules     = accepted.flatMap(ModuleSummary.fromArtifact)
    val moduleIndex = new ModuleIndex(modules)
    val resolutions = modules.flatMap(module => module.requireCalls.map(resolveRequire(module, _, moduleIndex)))
    val returnTables = modules.flatMap(_.exports.map { moduleExport =>
      LuaModuleReturnTable(
        moduleExport.modulePath,
        moduleExport.tableRef,
        moduleExport.fieldName,
        moduleExport.targetPrototypeId,
        Provenance
      )
    })
    val fieldTargets =
      modules.flatMap(module => module.fieldCalls.flatMap(resolveFieldCall(module, _, resolutions, moduleIndex)))
    val crossTargets = fieldTargets.map(target =>
      LuaCrossBoundaryCallTarget(
        target.fromModulePath,
        target.callsiteId,
        target.targetModulePath,
        target.targetPrototypeId,
        "candidate",
        Provenance
      )
    )
    val localArgFlows        = modules.flatMap(localInterproceduralArgFlows)
    val localReturnFlows     = modules.flatMap(localInterproceduralReturnFlows)
    val crossInterprocedural = fieldTargets.flatMap(target => crossModuleFlows(target, moduleIndex))
    val sourceEndpoints      = modules.flatMap(sourceEndpointsForModule).distinct
    val sinkEndpoints        = modules.flatMap(sinkEndpointsForModule(_, attribution)).distinct
    val allArgFlows          = (localArgFlows ++ crossInterprocedural.map(_._1)).distinct
    val allReturnFlows       = (localReturnFlows ++ crossInterprocedural.flatMap(_._2)).distinct
    val pathSearch           = new LocalPathSearch(modules, attribution)
    val sourceSinkPruning =
      new SourceSinkPruning(modules, fieldTargets, allArgFlows, allReturnFlows, sinkEndpoints, attribution)
    val qualifiedSinksBySource = sourceEndpoints.map { sourceEndpoint =>
      sourceEndpoint -> sourceSinkPruning.qualifiedSinkEndpoints(sourceEndpoint, sinkEndpoints)
    }.toMap
    val taintPaths =
      realFirmwareTaintPaths(
        modules,
        sourceEndpoints,
        qualifiedSinksBySource,
        allArgFlows,
        allReturnFlows,
        pathSearch,
        attribution
      )
    val boundaries               = semanticBoundaries(modules, resolutions, fieldTargets)
    val ruleMatches              = ruleMatchesFor(sourceEndpoints, sinkEndpoints)
    val sanitizerCalls           = sanitizerCallsFor(modules)
    val sanitizerClassifications = sanitizerClassificationsFor(taintPaths, sanitizerCalls)
    val reportClassifications    = reportClassificationsFor(taintPaths, sanitizerClassifications)
    val vulnerabilityReports     = vulnerabilityReportsFor(taintPaths, reportClassifications)
    val e5Boundaries             = Vector.empty[LuaE5Boundary]

    LuaProgramSemantics(
      moduleResolutions = resolutions,
      moduleReturnTables = returnTables,
      moduleFieldCallTargets = fieldTargets,
      interproceduralArgFlows = allArgFlows,
      interproceduralReturnFlows = allReturnFlows,
      crossBoundaryCallTargets = crossTargets.distinct,
      taintPaths = taintPaths.distinct,
      boundaries = boundaries.distinct,
      ruleMatches = ruleMatches.distinct,
      sourceEndpoints = sourceEndpoints.distinct,
      sinkEndpoints = sinkEndpoints.distinct,
      sanitizerCalls = sanitizerCalls.distinct,
      sanitizerClassifications = sanitizerClassifications.distinct,
      reportClassifications = reportClassifications.distinct,
      vulnerabilityReports = vulnerabilityReports.distinct,
      e5Boundaries = e5Boundaries.distinct,
      pathSearchStats = pathSearch.stats(
        sourceSinkPairCount = sourceEndpoints.size * sinkEndpoints.size,
        qualifiedSourceSinkPairCount = qualifiedSinksBySource.values.map(_.size).sum
      ),
      performanceAttribution = attribution.result(reportClassifications)
    )
  }

  private def resolveRequire(
    module: ModuleSummary,
    requireCall: RequireCall,
    moduleIndex: ModuleIndex
  ): LuaModuleResolution =
    requireCall.requireString match {
      case Some(name) =>
        moduleIndex.resolve(name) match {
          case ModuleResolutionResult.Matched(target) =>
            LuaModuleResolution(
              requireCall.callsiteId,
              name,
              "matched",
              module.path,
              Some(target.path),
              None,
              Provenance
            )
          case ModuleResolutionResult.Unresolved =>
            LuaModuleResolution(
              requireCall.callsiteId,
              name,
              "unresolved",
              module.path,
              None,
              Some("unresolved-module"),
              BoundaryProvenance
            )
        }
      case None =>
        LuaModuleResolution(
          requireCall.callsiteId,
          "dynamic",
          "dynamic",
          module.path,
          None,
          Some("dynamic-require"),
          BoundaryProvenance
        )
    }

  private def resolveFieldCall(
    module: ModuleSummary,
    call: FieldCall,
    resolutions: Vector[LuaModuleResolution],
    moduleIndex: ModuleIndex
  ): Option[LuaModuleFieldCallTarget] = {
    val targetModulePath = resolutions
      .filter(resolution => resolution.fromModulePath == module.path && resolution.resolutionStatus == "matched")
      .find(resolution => call.requireRef.exists(ref => requireResolutionReturns(module, resolution, ref)))
      .flatMap(_.targetModulePath)
    targetModulePath.flatMap { path =>
      moduleIndex
        .module(path)
        .flatMap(_.exports.find(_.fieldName == call.fieldName))
        .map(moduleExport =>
          LuaModuleFieldCallTarget(
            module.path,
            call.callsiteId,
            call.fieldName,
            path,
            moduleExport.targetPrototypeId,
            Provenance
          )
        )
    }
  }

  private def requireResolutionReturns(
    module: ModuleSummary,
    resolution: LuaModuleResolution,
    requireRef: String
  ): Boolean =
    module.requireCalls.exists(call =>
      call.callsiteId == resolution.requireCallsiteId &&
        call.resultRef.contains(requireRef)
    )

  private def localInterproceduralArgFlows(module: ModuleSummary): Vector[LuaInterproceduralArgFlow] =
    module.localCalls.flatMap { call =>
      module.prototype(call.targetPrototypeId).toVector.flatMap { callee =>
        for {
          (fromArg, argumentIndex) <- call.argumentRefs.zipWithIndex
          toParam                  <- callee.parameterRefs.lift(argumentIndex)
          if !parameterFlowsToCallTarget(callee, argumentIndex)
        } yield LuaInterproceduralArgFlow(
          call.callsiteId,
          qualify(module.path, fromArg),
          argumentIndex,
          module.path,
          call.targetPrototypeId,
          qualify(module.path, toParam),
          Provenance
        )
      }
    }

  private def localInterproceduralReturnFlows(module: ModuleSummary): Vector[LuaInterproceduralReturnFlow] =
    module.localCalls.flatMap { call =>
      module.prototype(call.targetPrototypeId).flatMap { callee =>
        for {
          calleeReturn <- callee.returnRefs.headOption
          callerResult <- call.resultRef
        } yield LuaInterproceduralReturnFlow(
          call.callsiteId,
          module.path,
          call.targetPrototypeId,
          calleeReturn,
          qualify(module.path, callerResult),
          Provenance
        )
      }
    }

  private def crossModuleFlows(
    target: LuaModuleFieldCallTarget,
    moduleIndex: ModuleIndex
  ): Vector[(LuaInterproceduralArgFlow, Option[LuaInterproceduralReturnFlow])] = {
    val module = moduleIndex
      .module(target.fromModulePath)
      .getOrElse(
        throw new IllegalStateException(
          s"missing source module for resolved field target: module=${target.fromModulePath}"
        )
      )
    val call = module.fieldCalls
      .find(_.callsiteId == target.callsiteId)
      .getOrElse(
        throw new IllegalStateException(
          s"missing field call for resolved field target: module=${target.fromModulePath} callsite=${target.callsiteId}"
        )
      )
    val callee = moduleIndex
      .module(target.targetModulePath)
      .flatMap(_.prototype(target.targetPrototypeId))
      .getOrElse(
        throw new IllegalStateException(
          "missing callee prototype for resolved field target: " +
            s"module=${target.targetModulePath} prototype=${target.targetPrototypeId}"
        )
      )

    val returnFlow = for {
      calleeReturn <- callee.returnRefs.headOption
      callerResult <- call.resultRef
    } yield LuaInterproceduralReturnFlow(
      target.callsiteId,
      target.targetModulePath,
      target.targetPrototypeId,
      calleeReturn,
      qualify(module.path, callerResult),
      Provenance
    )
    call.argumentRefs.zipWithIndex.flatMap { case (fromArg, argumentIndex) =>
      callee.parameterRefs
        .lift(argumentIndex)
        .toVector
        .filter(_ => !parameterFlowsToCallTarget(callee, argumentIndex))
        .map { toParam =>
          LuaInterproceduralArgFlow(
            target.callsiteId,
            qualify(module.path, fromArg),
            argumentIndex,
            target.targetModulePath,
            target.targetPrototypeId,
            qualify(target.targetModulePath, toParam),
            Provenance
          ) -> returnFlow
        }
    }
  }

  private def realFirmwareTaintPaths(
    modules: Vector[ModuleSummary],
    sourceEndpoints: Vector[LuaSourceEndpoint],
    qualifiedSinksBySource: Map[LuaSourceEndpoint, Vector[LuaSinkEndpoint]],
    interproceduralArgFlows: Vector[LuaInterproceduralArgFlow],
    interproceduralReturnFlows: Vector[LuaInterproceduralReturnFlow],
    pathSearch: LocalPathSearch,
    attribution: PerformanceAttributionCollector
  ): Vector[LuaTaintPath] = {
    val crossModuleBridgeIndex =
      new CrossModuleBridgeIndex(
        interproceduralArgFlows,
        interproceduralReturnFlows,
        realFirmwareSanitizerProducedRefs(modules),
        attribution
      )
    sourceEndpoints.flatMap { source =>
      val qualifiedSinks = qualifiedSinksBySource
        .get(source)
        .getOrElse(throw new IllegalStateException(s"missing qualified sink set for source ${source.sourceRef}"))
      qualifiedSinks.flatMap { sink =>
        val path = attribution.withPair(source, sink) {
          val result = semanticBridgePath(source, sink, crossModuleBridgeIndex, pathSearch)
            .orElse(samePrototypeForwardPath(pathSearch, source.sourceRef, sink.sinkRef))
          if (result.nonEmpty) attribution.markPathProduced()
          result
        }
        path.map { pathSteps =>
          attribution.incrementFor(source, sink, "taint_path_count")
          LuaTaintPath(source.sourceRef, sink.sinkRef, pathSteps, "true-positive", Provenance)
        }
      }
    }.distinct
  }

  private def samePrototypeForwardPath(
    pathSearch: LocalPathSearch,
    sourceRef: String,
    sinkRef: String
  ): Option[Vector[String]] = {
    val source = parseQualifiedValueRef(sourceRef)
    val sink   = parseQualifiedValueRef(sinkRef)
    if (
      source.modulePath == sink.modulePath &&
      source.prototypeId == sink.prototypeId &&
      source.pc <= sink.pc
    )
      pathSearch.moduleLocalPathMode(
        sourceRef,
        sinkRef,
        includeRepresentativeCallReturns = true,
        includeRepresentativeValueEdges = true
      )
    else None
  }

  private def semanticBridgePath(
    sourceEndpoint: LuaSourceEndpoint,
    sinkEndpoint: LuaSinkEndpoint,
    crossModuleBridgeIndex: CrossModuleBridgeIndex,
    pathSearch: LocalPathSearch
  ): Option[Vector[String]] = {
    val provenanceBridge = crossModuleBridgeIndex.sourceScopedBridgePath(
      sourceEndpoint,
      sinkEndpoint,
      pathSearch,
      includeRepresentativeValueEdges = true
    )
    if (
      provenanceBridge
        .exists(path => !crossModuleBridgeIndex.pathCrossesSourceSanitizerProducedRef(path, sourceEndpoint.sourceRef))
    ) {
      provenanceBridge
    } else {
      val needsArgumentReturnBridge =
        crossModuleBridgeIndex.sourceRequiresRepresentativeReturnBridge(sourceEndpoint, sinkEndpoint, pathSearch)
      val argumentReturnBridge =
        if (provenanceBridge.nonEmpty || needsArgumentReturnBridge) {
          crossModuleBridgeIndex.sourceScopedRepresentativeBridgePath(sourceEndpoint, sinkEndpoint, pathSearch)
        } else {
          None
        }
      selectUnsanitizedAlternative(provenanceBridge, argumentReturnBridge, sourceEndpoint, crossModuleBridgeIndex)
    }
  }

  private def selectUnsanitizedAlternative(
    preferredPath: Option[Vector[String]],
    alternativePath: Option[Vector[String]],
    sourceEndpoint: LuaSourceEndpoint,
    crossModuleBridgeIndex: CrossModuleBridgeIndex
  ): Option[Vector[String]] =
    (preferredPath, alternativePath) match {
      case (Some(preferred), Some(alternative))
          if crossModuleBridgeIndex.pathCrossesSourceSanitizerProducedRef(preferred, sourceEndpoint.sourceRef) &&
            !crossModuleBridgeIndex.pathCrossesSourceSanitizerProducedRef(alternative, sourceEndpoint.sourceRef) =>
        Some(alternative)
      case (Some(preferred), _) => Some(preferred)
      case (None, alternative)  => alternative
    }

  private def representativeSinkSeeds(module: ModuleSummary, sinkEndpoint: LuaSinkEndpoint): Vector[String] = {
    val sink = parseQualifiedValueRef(sinkEndpoint.sinkRef)
    module
      .prototype(sink.prototypeId)
      .map { prototype =>
        val sanitizerReturn =
          prototype.calls
            .filter(call => call.pc < sink.pc && call.returnRefs.nonEmpty)
            .filter(call => isRepresentativeSanitizerCall(call.resolvedName))
            .sortBy(_.pc)
            .flatMap(_.returnRefs.headOption)
            .map(qualify(module.path, _))

        val sinkSlot = slotFromValueRef(sinkEndpoint.sinkRef)
        val slotSeeds = prototype.instructions
          .filter(instruction => instruction.pc < sink.pc && instruction.a == sinkSlot)
          .sortBy(_.pc)
          .map(instruction => qualify(module.path, s"${prototype.prototypeId}@pc${instruction.pc}:r${instruction.a}"))

        (sanitizerReturn ++ slotSeeds).distinct
      }
      .getOrElse(Vector.empty)
  }

  private final class CrossModuleBridgeIndex(
    interproceduralArgFlows: Vector[LuaInterproceduralArgFlow],
    interproceduralReturnFlows: Vector[LuaInterproceduralReturnFlow],
    sanitizerProducedRefs: Set[String],
    attribution: PerformanceAttributionCollector
  ) {
    private final case class BridgeFlow(
      sourcePrototype: String,
      targetPrototype: String,
      fromRef: String,
      toRef: String,
      callsiteId: String,
      callsitePc: Int,
      sortIndex: Int,
      isArgumentFlow: Boolean
    )

    private val argumentBridgeFlows: Vector[BridgeFlow] = interproceduralArgFlows.map { flow =>
      val source = parseQualifiedValueRef(flow.fromArgumentRef)
      BridgeFlow(
        prototypeRef(source.modulePath, source.prototypeId),
        prototypeRef(flow.targetModulePath, flow.targetPrototypeId),
        flow.fromArgumentRef,
        flow.toParameterRef,
        flow.callsiteId,
        requiredCallsitePc(flow.callsiteId),
        flow.argumentIndex,
        isArgumentFlow = true
      )
    }
    private val returnBridgeFlows: Vector[BridgeFlow] = interproceduralReturnFlows.map { flow =>
      val caller = parseQualifiedValueRef(flow.callerResultRef)
      BridgeFlow(
        prototypeRef(flow.targetModulePath, flow.targetPrototypeId),
        prototypeRef(caller.modulePath, caller.prototypeId),
        qualify(flow.targetModulePath, flow.calleeReturnRef),
        flow.callerResultRef,
        flow.callsiteId,
        requiredCallsitePc(flow.callsiteId),
        sortIndex = 0,
        isArgumentFlow = false
      )
    }
    private val representativeBridgeFlows = (argumentBridgeFlows ++ returnBridgeFlows).distinct
    private val directEdges: Set[(String, String)] =
      argumentBridgeFlows.map(flow => flow.sourcePrototype -> flow.targetPrototype).toSet
    private val representativeDirectEdges: Set[(String, String)] =
      representativeBridgeFlows.map(flow => flow.sourcePrototype -> flow.targetPrototype).toSet
    private val outgoingPrototypeEdges: Map[String, Set[String]] =
      directEdges.groupMap(_._1)(_._2).view.mapValues(_.toSet).toMap
    private val representativeIncomingPrototypeEdges: Map[String, Set[String]] =
      representativeDirectEdges.groupMap(_._2)(_._1).view.mapValues(_.toSet).toMap
    private val reachabilityCache                    = scala.collection.mutable.Map.empty[(String, String), Boolean]
    private val representativeDistanceCache          = scala.collection.mutable.Map.empty[String, Map[String, Int]]
    private val representativeValueReachabilityCache = scala.collection.mutable.Map.empty[String, Set[String]]
    private val localBridgePathCache =
      scala.collection.mutable.Map.empty[(String, String, Boolean, Boolean), Option[Vector[String]]]

    private val argumentFlowsBySourcePrototype: Map[String, Vector[BridgeFlow]] =
      argumentBridgeFlows.groupBy(_.sourcePrototype)
    private val representativeFlowsBySourcePrototype: Map[String, Vector[BridgeFlow]] =
      representativeBridgeFlows.groupBy(_.sourcePrototype)

    def sourceScopedBridgePath(
      sourceEndpoint: LuaSourceEndpoint,
      sinkEndpoint: LuaSinkEndpoint,
      pathSearch: LocalPathSearch,
      includeRepresentativeValueEdges: Boolean
    ): Option[Vector[String]] = {
      val source        = parseQualifiedValueRef(sourceEndpoint.sourceRef)
      val sink          = parseQualifiedValueRef(sinkEndpoint.sinkRef)
      val sinkPrototype = prototypeRef(sink.modulePath, sink.prototypeId)
      val pending  = scala.collection.mutable.Queue((sourceEndpoint.sourceRef, Vector(sourceEndpoint.sourceRef), 0))
      val seen     = scala.collection.mutable.Set(sourceEndpoint.sourceRef)
      val maxDepth = 4
      var firstSanitizedPath: Option[Vector[String]] = None
      var unsanitizedPath: Option[Vector[String]]    = None

      while (pending.nonEmpty && unsanitizedPath.isEmpty) {
        val (currentRef, pathPrefix, depth) = pending.dequeue()
        val currentPrototype                = prototypeRefFromAnyQualifiedRef(currentRef)
        val currentPc                       = pcFromAnyValueRef(currentRef)

        val currentIsEntryParameter = isPrototypeEntryParameterRef(currentRef)
        val candidateFlows = argumentFlowsBySourcePrototype
          .getOrElse(currentPrototype, Vector.empty)
          .filter { flow =>
            attribution.increment("bridge_argument_provenance_candidate_count")
            val flowPc = flow.callsitePc
            val accepted = currentPc match {
              case Some(pc) if flowPc >= pc        => true
              case Some(_)                         => false
              case None if currentIsEntryParameter => true
              case None =>
                throw new IllegalStateException(
                  s"missing pc provenance for cross-module bridge ref: current_ref=$currentRef"
                )
            }
            if (!accepted) {
              attribution.increment("bridge_candidate_pc_pruned_count")
              attribution.increment("early_candidate_short_circuit_count")
            }
            accepted
          }
          .filter { flow =>
            val accepted =
              flow.targetPrototype == sinkPrototype || reachesSinkPrototype(flow.targetPrototype, sinkPrototype)
            if (!accepted) {
              attribution.increment("bridge_candidate_reachability_pruned_count")
              attribution.increment("early_candidate_short_circuit_count")
            }
            accepted
          }
          .sortBy(flow => (flow.callsitePc, flow.targetPrototype, flow.sortIndex))

        val reachableArgumentPaths = {
          val candidateRefs = candidateFlows.map(_.fromRef).toSet
          if (includeRepresentativeValueEdges)
            pathSearch.moduleLocalPathsToAnyWithRepresentativeValues(currentRef, candidateRefs)
          else
            pathSearch.moduleLocalPathsToAny(currentRef, candidateRefs, includeRepresentativeCallReturns = true)
        }

        val candidateIterator = candidateFlows.filter(flow => reachableArgumentPaths.contains(flow.fromRef)).iterator
        while (candidateIterator.hasNext && unsanitizedPath.isEmpty) {
          val flow = candidateIterator.next()
          attribution.materializeActivePair()
          val targetParamRef  = flow.toRef
          val targetPrototype = flow.targetPrototype
          val currentToArgumentPath = preferredLocalPathMode(
            pathSearch,
            currentRef,
            flow.fromRef,
            includeRepresentativeCallReturns = true,
            includeRepresentativeValueEdges = includeRepresentativeValueEdges
          )
          currentToArgumentPath
            .foreach { currentToArgument =>
              val bridgePrefix = pathPrefix ++ currentToArgument.drop(1) :+ targetParamRef
              if (targetPrototype == sinkPrototype) {
                preferredLocalPathMode(
                  pathSearch,
                  targetParamRef,
                  sinkEndpoint.sinkRef,
                  includeRepresentativeCallReturns = true,
                  includeRepresentativeValueEdges = includeRepresentativeValueEdges
                )
                  .map(parameterToSink => bridgePrefix ++ parameterToSink.drop(1))
                  .foreach { completePath =>
                    if (crossesSourceSanitizerProducedRef(completePath, sourceEndpoint.sourceRef)) {
                      if (firstSanitizedPath.isEmpty) firstSanitizedPath = Some(completePath)
                    } else {
                      unsanitizedPath = Some(completePath)
                    }
                  }
              } else if (depth < maxDepth && !seen(targetParamRef)) {
                seen += targetParamRef
                pending.enqueue((targetParamRef, bridgePrefix, depth + 1))
              }
            }
        }

      }

      unsanitizedPath.orElse(firstSanitizedPath)
    }

    def sourceScopedRepresentativeBridgePath(
      sourceEndpoint: LuaSourceEndpoint,
      sinkEndpoint: LuaSinkEndpoint,
      pathSearch: LocalPathSearch
    ): Option[Vector[String]] = {
      val source          = parseQualifiedValueRef(sourceEndpoint.sourceRef)
      val sink            = parseQualifiedValueRef(sinkEndpoint.sinkRef)
      val sourcePrototype = prototypeRef(source.modulePath, source.prototypeId)
      val sinkPrototype   = prototypeRef(sink.modulePath, sink.prototypeId)
      val distanceToSink  = representativeDistancesToSink(sinkPrototype)
      val pending =
        scala.collection.mutable.Queue((sourceEndpoint.sourceRef, Vector(sourceEndpoint.sourceRef), sourcePrototype, 0))
      val seen                          = scala.collection.mutable.Set(sourceEndpoint.sourceRef)
      val maxDepth                      = 4
      var found: Option[Vector[String]] = None

      while (pending.nonEmpty && found.isEmpty) {
        val (currentRef, pathPrefix, currentPrototype, depth) = pending.dequeue()
        val currentPc                                         = pcFromAnyValueRef(currentRef)
        val currentIsEntryParameter                           = isPrototypeEntryParameterRef(currentRef)
        val currentDistance                                   = distanceToSink.getOrElse(currentPrototype, Int.MaxValue)
        val candidateFlows =
          (if (depth == 0) argumentFlowsBySourcePrototype else representativeFlowsBySourcePrototype)
            .getOrElse(currentPrototype, Vector.empty)
            .filter { flow =>
              attribution.increment("bridge_argument_provenance_candidate_count")
              val pcAccepted =
                if (depth == 0) flow.callsitePc > source.pc
                else if (!flow.isArgumentFlow) true
                else {
                  val flowPc = flow.callsitePc
                  currentPc match {
                    case Some(pc) if flowPc >= pc        => true
                    case Some(_)                         => false
                    case None if currentIsEntryParameter => true
                    case None =>
                      throw new IllegalStateException(
                        s"missing pc provenance for cross-module bridge ref: current_ref=$currentRef"
                      )
                  }
                }
              if (!pcAccepted) {
                attribution.increment("bridge_candidate_pc_pruned_count")
                attribution.increment("early_candidate_short_circuit_count")
              }
              pcAccepted
            }
            .filter { flow =>
              val reachable =
                if (depth == 0 && sourcePrototype == sinkPrototype)
                  flow.targetPrototype != sinkPrototype && distanceToSink.contains(flow.targetPrototype)
                else distanceToSink.get(flow.targetPrototype).contains(currentDistance - 1)
              if (!reachable) {
                attribution.increment("bridge_candidate_reachability_pruned_count")
                attribution.increment("early_candidate_short_circuit_count")
              }
              reachable
            }
            .sortBy(flow => (flow.callsitePc, flow.targetPrototype, flow.sortIndex))

        val reachableBridgePrefixes =
          reachableRepresentativeBridgePrefixes(
            pathSearch,
            currentRef,
            candidateFlows,
            includeArgumentRepresentativeValues = depth > 0
          )

        val candidateIterator = candidateFlows.filter(flow => reachableBridgePrefixes.contains(flow.fromRef)).iterator
        while (candidateIterator.hasNext && found.isEmpty) {
          val flow         = candidateIterator.next()
          val bridgePrefix = pathPrefix ++ reachableBridgePrefixes(flow.fromRef).drop(1) :+ flow.toRef
          if (flow.targetPrototype == sinkPrototype) {
            found = preferredLocalPathForBridgeFlow(pathSearch, flow, sinkEndpoint.sinkRef)
              .map(targetToSink => bridgePrefix ++ targetToSink.drop(1))
          } else if (depth < maxDepth && !seen(flow.toRef)) {
            seen += flow.toRef
            pending.enqueue((flow.toRef, bridgePrefix, flow.targetPrototype, depth + 1))
          }
        }
      }

      found
    }

    private def reachableRepresentativeBridgePrefixes(
      pathSearch: LocalPathSearch,
      currentRef: String,
      candidateFlows: Vector[BridgeFlow],
      includeArgumentRepresentativeValues: Boolean
    ): Map[String, Vector[String]] = {
      val argumentFlowRefs = candidateFlows.filter(_.isArgumentFlow).map(_.fromRef).toSet
      val argumentFlowPaths =
        if (includeArgumentRepresentativeValues)
          pathSearch.moduleLocalPathsToAnyWithRepresentativeValues(currentRef, argumentFlowRefs)
        else
          pathSearch.moduleLocalPathsToAny(currentRef, argumentFlowRefs, includeRepresentativeCallReturns = true)
      val returnFlowPaths = pathSearch.moduleLocalPathsToAny(
        currentRef,
        candidateFlows.filterNot(_.isArgumentFlow).map(_.fromRef).toSet,
        includeRepresentativeCallReturns = true
      )
      argumentFlowPaths ++ returnFlowPaths
    }

    private def representativeDistancesToSink(sinkPrototype: String): Map[String, Int] =
      representativeDistanceCache.getOrElseUpdate(
        sinkPrototype, {
          val pending  = scala.collection.mutable.Queue(sinkPrototype)
          val distance = scala.collection.mutable.Map(sinkPrototype -> 0)
          while (pending.nonEmpty) {
            val current         = pending.dequeue()
            val currentDistance = distance(current)
            representativeIncomingPrototypeEdges.getOrElse(current, Set.empty).foreach { previous =>
              if (!distance.contains(previous)) {
                distance(previous) = currentDistance + 1
                pending.enqueue(previous)
              }
            }
          }
          distance.toMap
        }
      )

    def sourceRequiresRepresentativeReturnBridge(
      sourceEndpoint: LuaSourceEndpoint,
      sinkEndpoint: LuaSinkEndpoint,
      pathSearch: LocalPathSearch
    ): Boolean = {
      val sinkPrototype = prototypeRef(sinkEndpoint.sinkRef)
      representativeValueReachableReturnPrototypes(sourceEndpoint, pathSearch)(sinkPrototype)
    }

    private def representativeValueReachableReturnPrototypes(
      sourceEndpoint: LuaSourceEndpoint,
      pathSearch: LocalPathSearch
    ): Set[String] =
      representativeValueReachabilityCache.getOrElseUpdate(
        sourceEndpoint.sourceRef, {
          val source          = parseQualifiedValueRef(sourceEndpoint.sourceRef)
          val sourcePrototype = prototypeRef(source.modulePath, source.prototypeId)
          val pending         = scala.collection.mutable.Queue((sourceEndpoint.sourceRef, sourcePrototype, 0, false))
          val seen            = scala.collection.mutable.Set((sourceEndpoint.sourceRef, sourcePrototype, 0, false))
          val reachable       = scala.collection.mutable.Set.empty[String]
          val maxDepth        = 4

          while (pending.nonEmpty) {
            val (currentRef, currentPrototype, depth, usedReturnBridge) = pending.dequeue()
            val currentPc                                               = pcFromAnyValueRef(currentRef)
            val currentIsEntryParameter                                 = isPrototypeEntryParameterRef(currentRef)
            val candidateFlows =
              (if (depth == 0) argumentFlowsBySourcePrototype else representativeFlowsBySourcePrototype)
                .getOrElse(currentPrototype, Vector.empty)
                .filter { flow =>
                  attribution.increment("bridge_argument_provenance_candidate_count")
                  val pcAccepted =
                    if (depth == 0) flow.callsitePc > source.pc
                    else if (!flow.isArgumentFlow) true
                    else {
                      currentPc match {
                        case Some(pc) if flow.callsitePc >= pc => true
                        case Some(_)                           => false
                        case None if currentIsEntryParameter   => true
                        case None =>
                          throw new IllegalStateException(
                            s"missing pc provenance for cross-module bridge ref: current_ref=$currentRef"
                          )
                      }
                    }
                  if (!pcAccepted) {
                    attribution.increment("bridge_candidate_pc_pruned_count")
                    attribution.increment("early_candidate_short_circuit_count")
                  }
                  pcAccepted
                }

            val reachableBridgePrefixes =
              reachableRepresentativeBridgePrefixes(
                pathSearch,
                currentRef,
                candidateFlows,
                includeArgumentRepresentativeValues = depth > 0
              )
            candidateFlows
              .filter(flow => reachableBridgePrefixes.contains(flow.fromRef))
              .sortBy(flow => (flow.callsitePc, flow.targetPrototype, flow.sortIndex))
              .foreach { flow =>
                val nextUsedReturnBridge = usedReturnBridge || !flow.isArgumentFlow
                if (nextUsedReturnBridge) reachable += flow.targetPrototype
                if (depth < maxDepth) {
                  val state = (flow.toRef, flow.targetPrototype, depth + 1, nextUsedReturnBridge)
                  if (!seen(state)) {
                    seen += state
                    pending.enqueue(state)
                  }
                }
              }
          }
          reachable.toSet
        }
      )

    private def cachedLocalPath(
      pathSearch: LocalPathSearch,
      sourceRef: String,
      sinkRef: String,
      includeRepresentativeCallReturns: Boolean,
      includeRepresentativeValueEdges: Boolean
    ): Option[Vector[String]] = {
      attribution.increment("bridge_local_path_attempt_count")
      val key = (sourceRef, sinkRef, includeRepresentativeCallReturns, includeRepresentativeValueEdges)
      attribution.increment(
        if (localBridgePathCache.contains(key)) "bridge_path_cache_hit_count" else "bridge_path_cache_miss_count"
      )
      val result = localBridgePathCache.getOrElseUpdate(
        key,
        pathSearch.moduleLocalPathMode(
          sourceRef,
          sinkRef,
          includeRepresentativeCallReturns,
          includeRepresentativeValueEdges
        )
      )
      if (result.nonEmpty) attribution.increment("bridge_local_path_success_count")
      result
    }

    private def preferredLocalPath(
      pathSearch: LocalPathSearch,
      sourceRef: String,
      sinkRef: String,
      includeRepresentativeCallReturns: Boolean
    ): Option[Vector[String]] =
      preferredLocalPathMode(
        pathSearch,
        sourceRef,
        sinkRef,
        includeRepresentativeCallReturns,
        includeRepresentativeValueEdges = false
      )

    private def preferredLocalPathForBridgeFlow(
      pathSearch: LocalPathSearch,
      flow: BridgeFlow,
      sinkRef: String
    ): Option[Vector[String]] =
      if (flow.isArgumentFlow)
        preferredLocalPath(pathSearch, flow.toRef, sinkRef, includeRepresentativeCallReturns = true)
      else
        preferredLocalPathMode(
          pathSearch,
          flow.toRef,
          sinkRef,
          includeRepresentativeCallReturns = false,
          includeRepresentativeValueEdges = true
        )

    private def preferredLocalPathMode(
      pathSearch: LocalPathSearch,
      sourceRef: String,
      sinkRef: String,
      includeRepresentativeCallReturns: Boolean,
      includeRepresentativeValueEdges: Boolean
    ): Option[Vector[String]] =
      cachedLocalPath(pathSearch, sourceRef, sinkRef, includeRepresentativeCallReturns, includeRepresentativeValueEdges)
        .map { localPath =>
          if (crossesSanitizerProducedRef(localPath)) {
            pathSearch.moduleLocalPathAvoidingMode(
              sourceRef,
              sinkRef,
              sanitizerProducedRefs,
              includeRepresentativeCallReturns,
              includeRepresentativeValueEdges
            ) match {
              case Some(unsanitizedPath) => unsanitizedPath
              case None                  => localPath
            }
          } else {
            localPath
          }
        }

    private def crossesSanitizerProducedRef(path: Vector[String]): Boolean =
      path.exists(sanitizerProducedRefs)

    private def crossesSourceSanitizerProducedRef(path: Vector[String], sourceRef: String): Boolean = {
      val sourceModulePath = modulePathFromQualifiedRef(sourceRef)
      path.exists(ref => sanitizerProducedRefs(ref) && modulePathFromQualifiedRef(ref) == sourceModulePath)
    }

    def pathCrossesSourceSanitizerProducedRef(path: Vector[String], sourceRef: String): Boolean =
      crossesSourceSanitizerProducedRef(path, sourceRef)

    private def pcFromAnyValueRef(valueRef: String): Option[Int] =
      localRefFromQualifiedRef(valueRef)
        .split("@pc", 2)
        .lift(1)
        .flatMap(_.split(":r", 2).headOption)
        .flatMap(_.toIntOption)

    private def requiredCallsitePc(callsiteId: String): Int =
      callsitePc(callsiteId).getOrElse(throw new IllegalStateException(s"missing callsite pc: callsite_id=$callsiteId"))

    private def isPrototypeEntryParameterRef(valueRef: String): Boolean =
      !localRefFromQualifiedRef(valueRef).contains("@pc")

    private def reachesSinkPrototype(start: String, sinkPrototype: String): Boolean =
      reachabilityCache.getOrElseUpdate(
        start -> sinkPrototype, {
          val pending = scala.collection.mutable.Queue(start)
          val seen    = scala.collection.mutable.Set.empty[String]
          var found   = false
          while (pending.nonEmpty && !found) {
            val current = pending.dequeue()
            if (!seen(current)) {
              seen += current
              outgoingPrototypeEdges.getOrElse(current, Set.empty).foreach { target =>
                if (target == sinkPrototype) found = true
                else if (!seen(target)) pending.enqueue(target)
              }
            }
          }
          found
        }
      )

  }

  private final class LocalPathSearch(modules: Vector[ModuleSummary], attribution: PerformanceAttributionCollector) {
    private final class BlockedRefsIdentityKey(val refs: Set[String]) {
      override def equals(other: Any): Boolean =
        other match {
          case that: BlockedRefsIdentityKey => refs.asInstanceOf[AnyRef] eq that.refs.asInstanceOf[AnyRef]
          case _                            => false
        }
      override def hashCode(): Int = System.identityHashCode(refs.asInstanceOf[AnyRef])
    }

    private val modulesByPath = modules.map(module => module.path -> module).toMap
    private val graphCache =
      scala.collection.mutable.Map.empty[(String, Boolean, Boolean), Map[String, Vector[String]]]
    private val pathCache =
      scala.collection.mutable.Map.empty[(String, String, Boolean, Boolean), Option[Vector[String]]]
    private val avoidingGraphCache =
      scala.collection.mutable.Map
        .empty[(String, Boolean, Boolean, BlockedRefsIdentityKey), Map[String, Vector[String]]]
    private val avoidingPathCache =
      scala.collection.mutable.Map
        .empty[(String, String, Boolean, Boolean, BlockedRefsIdentityKey), Option[Vector[String]]]
    private val representativeSinkPathCache = scala.collection.mutable.Map.empty[String, Vector[Vector[String]]]
    private val graphModules                = scala.collection.mutable.Set.empty[String]
    private var graphBuildCount             = 0
    private var localPathSearchCount        = 0
    private var batchedDistinctPathCount    = 0
    private var uncachedDistinctPathCount   = 0

    def moduleLocalPath(
      sourceRef: String,
      sinkRef: String,
      includeRepresentativeCallReturns: Boolean = false
    ): Option[Vector[String]] =
      moduleLocalPathMode(sourceRef, sinkRef, includeRepresentativeCallReturns, includeRepresentativeValueEdges = false)

    def moduleLocalPathMode(
      sourceRef: String,
      sinkRef: String,
      includeRepresentativeCallReturns: Boolean,
      includeRepresentativeValueEdges: Boolean
    ): Option[Vector[String]] = {
      attribution.materializeActivePair()
      attribution.increment("local_path_search_count")
      localPathSearchCount += 1
      val sourceModulePath = modulePathFromQualifiedRef(sourceRef)
      val sinkModulePath   = modulePathFromQualifiedRef(sinkRef)
      if (sourceModulePath != sinkModulePath) {
        attribution.increment("local_path_cache_miss_count")
        attribution.increment("distinct_local_path_query_count")
        uncachedDistinctPathCount += 1
        None
      } else {
        val key = (sourceRef, sinkRef, includeRepresentativeCallReturns, includeRepresentativeValueEdges)
        val hit = pathCache.contains(key)
        attribution.increment(if (hit) "local_path_cache_hit_count" else "local_path_cache_miss_count")
        if (!hit) attribution.increment("distinct_local_path_query_count")
        pathCache.getOrElseUpdate(
          key,
          modulesByPath.get(sourceModulePath).flatMap { module =>
            val bySource = graphFor(module, includeRepresentativeCallReturns, includeRepresentativeValueEdges)
            breadthFirstPath(bySource, sourceRef, sinkRef)
          }
        )
      }
    }

    def moduleLocalPathAvoiding(
      sourceRef: String,
      sinkRef: String,
      blockedRefs: Set[String],
      includeRepresentativeCallReturns: Boolean = false
    ): Option[Vector[String]] =
      moduleLocalPathAvoidingMode(
        sourceRef,
        sinkRef,
        blockedRefs,
        includeRepresentativeCallReturns,
        includeRepresentativeValueEdges = false
      )

    def moduleLocalPathAvoidingMode(
      sourceRef: String,
      sinkRef: String,
      blockedRefs: Set[String],
      includeRepresentativeCallReturns: Boolean,
      includeRepresentativeValueEdges: Boolean
    ): Option[Vector[String]] = {
      attribution.materializeActivePair()
      attribution.increment("local_path_search_count")
      localPathSearchCount += 1
      val sourceModulePath = modulePathFromQualifiedRef(sourceRef)
      val sinkModulePath   = modulePathFromQualifiedRef(sinkRef)
      if (sourceModulePath != sinkModulePath || blockedRefs(sourceRef) || blockedRefs(sinkRef)) {
        attribution.increment("local_path_cache_miss_count")
        attribution.increment("distinct_local_path_query_count")
        uncachedDistinctPathCount += 1
        None
      } else {
        val blockedKey = BlockedRefsIdentityKey(blockedRefs)
        val key = (sourceRef, sinkRef, includeRepresentativeCallReturns, includeRepresentativeValueEdges, blockedKey)
        val hit = avoidingPathCache.contains(key)
        attribution.increment(if (hit) "local_path_cache_hit_count" else "local_path_cache_miss_count")
        if (!hit) attribution.increment("distinct_local_path_query_count")
        avoidingPathCache.getOrElseUpdate(
          key,
          modulesByPath.get(sourceModulePath).flatMap { module =>
            val bySource = avoidingGraphFor(
              module,
              includeRepresentativeCallReturns,
              includeRepresentativeValueEdges,
              blockedRefs,
              blockedKey
            )
            breadthFirstPath(bySource, sourceRef, sinkRef)
          }
        )
      }
    }

    def moduleLocalPathsToAny(
      sourceRef: String,
      sinkRefs: Set[String],
      includeRepresentativeCallReturns: Boolean = false
    ): Map[String, Vector[String]] =
      moduleLocalPathsToAnyMode(
        sourceRef,
        sinkRefs,
        includeRepresentativeCallReturns,
        includeRepresentativeValueEdges = false
      )

    def moduleLocalPathsToAnyWithRepresentativeValues(
      sourceRef: String,
      sinkRefs: Set[String]
    ): Map[String, Vector[String]] =
      moduleLocalPathsToAnyMode(
        sourceRef,
        sinkRefs,
        includeRepresentativeCallReturns = true,
        includeRepresentativeValueEdges = true
      )

    private def moduleLocalPathsToAnyMode(
      sourceRef: String,
      sinkRefs: Set[String],
      includeRepresentativeCallReturns: Boolean,
      includeRepresentativeValueEdges: Boolean
    ): Map[String, Vector[String]] =
      if (sinkRefs.isEmpty) Map.empty
      else {
        attribution.materializeActivePair()
        attribution.increment("local_path_search_count")
        attribution.increment("local_path_cache_miss_count")
        attribution.increment("distinct_local_path_query_count")
        localPathSearchCount += 1
        batchedDistinctPathCount += 1
        val sourceModulePath = modulePathFromQualifiedRef(sourceRef)
        val invalidSinkModules = sinkRefs
          .map(modulePathFromQualifiedRef)
          .filter(_ != sourceModulePath)
        if (invalidSinkModules.nonEmpty) {
          throw new IllegalStateException(
            s"cross-module refs in local path batch: source_ref=$sourceRef sink_modules=${invalidSinkModules.toVector.sorted
                .mkString("[", ",", "]")}"
          )
        }
        modulesByPath
          .get(sourceModulePath)
          .map { module =>
            val bySource = graphFor(module, includeRepresentativeCallReturns, includeRepresentativeValueEdges)
            breadthFirstPathsToAny(bySource, sourceRef, sinkRefs)
          }
          .getOrElse(Map.empty)
      }

    def representativeSinkPaths(
      module: ModuleSummary,
      sinkEndpoint: LuaSinkEndpoint,
      seeds: => Vector[String]
    ): Vector[Vector[String]] =
      representativeSinkPathCache.getOrElseUpdate(
        sinkEndpoint.sinkRef,
        seeds.flatMap(seed => moduleLocalPath(seed, sinkEndpoint.sinkRef))
      )

    def stats(sourceSinkPairCount: Int, qualifiedSourceSinkPairCount: Int): LuaPathSearchStats = {
      val distinctLocalPathQueryCount =
        pathCache.size + avoidingPathCache.size + batchedDistinctPathCount + uncachedDistinctPathCount
      LuaPathSearchStats(
        localPathGraphModuleCount = graphModules.size,
        localPathGraphBuildCount = graphBuildCount,
        localPathSearchCount = localPathSearchCount,
        distinctLocalPathQueryCount = distinctLocalPathQueryCount,
        sourceSinkPairCount = sourceSinkPairCount,
        qualifiedSourceSinkPairCount = qualifiedSourceSinkPairCount,
        prototypePrunedSourceSinkPairCount = sourceSinkPairCount - qualifiedSourceSinkPairCount
      )
    }

    private def graphFor(
      module: ModuleSummary,
      includeRepresentativeCallReturns: Boolean,
      includeRepresentativeValueEdges: Boolean
    ): Map[String, Vector[String]] = {
      val key = (module.path, includeRepresentativeCallReturns, includeRepresentativeValueEdges)
      val hit = graphCache.contains(key)
      attribution.increment(if (hit) "local_path_graph_cache_hit_count" else "local_path_graph_cache_miss_count")
      if (!hit) attribution.increment("local_path_graph_build_count")
      graphCache.getOrElseUpdate(
        key, {
          graphBuildCount += 1
          graphModules += module.path
          val localEdges = module.localFlows
            .map(flow => qualify(module.path, flow.sourceRef) -> qualify(module.path, flow.sinkRef))
          val globalEdges =
            module.globalFlows.map(flow => qualify(module.path, flow.writeRef) -> qualify(module.path, flow.readRef))
          val representativeCallReturn =
            if (includeRepresentativeCallReturns)
              representativeCallReturnEdges(module)
            else Vector.empty
          val representativeValueEdges =
            if (includeRepresentativeValueEdges)
              representativeTableValueEdges(module) ++
                representativeParameterTableReadEdges(module) ++
                representativeIteratorValueEdges(module) ++
                representativeExpressionResultEdges(module)
            else Vector.empty
          (localEdges ++ globalEdges ++ representativeCallReturn ++ representativeValueEdges).groupMap(_._1)(_._2)
        }
      )
    }

    private def avoidingGraphFor(
      module: ModuleSummary,
      includeRepresentativeCallReturns: Boolean,
      includeRepresentativeValueEdges: Boolean,
      blockedRefs: Set[String],
      blockedKey: BlockedRefsIdentityKey
    ): Map[String, Vector[String]] =
      avoidingGraphCache.getOrElseUpdate(
        (module.path, includeRepresentativeCallReturns, includeRepresentativeValueEdges, blockedKey), {
          graphFor(module, includeRepresentativeCallReturns, includeRepresentativeValueEdges).flatMap {
            case (source, targets) =>
              if (blockedRefs(source)) None
              else {
                val retainedTargets = targets.filterNot(blockedRefs)
                Option.when(retainedTargets.nonEmpty)(source -> retainedTargets)
              }
          }
        }
      )

    private def breadthFirstPath(
      bySource: Map[String, Vector[String]],
      sourceRef: String,
      sinkRef: String
    ): Option[Vector[String]] = {
      val queue                         = scala.collection.mutable.Queue(Vector(sourceRef))
      val seen                          = scala.collection.mutable.Set(sourceRef)
      var found: Option[Vector[String]] = None
      while (queue.nonEmpty && found.isEmpty) {
        val path = queue.dequeue()
        attribution.increment("targeted_search_node_visit_count")
        if (path.last == sinkRef) found = Some(path)
        else {
          bySource.getOrElse(path.last, Vector.empty).foreach { next =>
            attribution.increment("targeted_search_edge_visit_count")
            if (!seen(next)) {
              seen += next
              queue.enqueue(path :+ next)
            }
          }
        }
      }
      found
    }

    private def breadthFirstPathsToAny(
      bySource: Map[String, Vector[String]],
      sourceRef: String,
      sinkRefs: Set[String]
    ): Map[String, Vector[String]] = {
      val remaining = scala.collection.mutable.Set.from(sinkRefs)
      val found     = scala.collection.mutable.Map.empty[String, Vector[String]]
      val queue     = scala.collection.mutable.Queue(Vector(sourceRef))
      val seen      = scala.collection.mutable.Set(sourceRef)
      while (queue.nonEmpty && remaining.nonEmpty) {
        val path = queue.dequeue()
        attribution.increment("targeted_search_node_visit_count")
        if (remaining(path.last)) {
          found += path.last -> path
          remaining -= path.last
        }
        if (remaining.nonEmpty) {
          bySource.getOrElse(path.last, Vector.empty).foreach { next =>
            attribution.increment("targeted_search_edge_visit_count")
            if (!seen(next)) {
              seen += next
              queue.enqueue(path :+ next)
            }
          }
        }
      }
      found.toMap
    }
  }

  private final class SourceSinkPruning(
    modules: Vector[ModuleSummary],
    fieldTargets: Vector[LuaModuleFieldCallTarget],
    interproceduralArgFlows: Vector[LuaInterproceduralArgFlow],
    interproceduralReturnFlows: Vector[LuaInterproceduralReturnFlow],
    sinkEndpoints: Vector[LuaSinkEndpoint],
    attribution: PerformanceAttributionCollector
  ) {
    private val callGraphAdjacency = buildCallGraphAdjacency()
    private val sinkPrototypeByRef = sinkEndpoints.map(row => row.sinkRef -> prototypeRef(row.sinkRef)).toMap
    private val directReachableSinkRefsBySource = buildDirectReachableSinkRefsBySource()
    private val directInterproceduralPairs = interproceduralArgFlows.map { flow =>
      val source = parseQualifiedValueRef(flow.fromArgumentRef)
      prototypeRef(source.modulePath, source.prototypeId) -> prototypeRef(flow.targetModulePath, flow.targetPrototypeId)
    }.toSet
    private val fedArgumentIndexesBySourceAndTarget = interproceduralArgFlows
      .groupMap { flow =>
        val source = parseQualifiedValueRef(flow.fromArgumentRef)
        prototypeRef(source.modulePath, source.prototypeId) -> prototypeRef(
          flow.targetModulePath,
          flow.targetPrototypeId
        )
      }(_.argumentIndex)
      .view
      .mapValues(_.toSet)
      .toMap
    private val interproceduralFlowsBySourcePrototype = interproceduralArgFlows.groupBy { flow =>
      val source = parseQualifiedValueRef(flow.fromArgumentRef)
      prototypeRef(source.modulePath, source.prototypeId)
    }
    private val reachablePrototypesByStartPrototype = scala.collection.mutable.Map.empty[String, Set[String]]
    private val sourceScopedBridgePrototypeCache    = scala.collection.mutable.Map.empty[String, Set[String]]

    def qualifiedSinkEndpoints(
      sourceEndpoint: LuaSourceEndpoint,
      allSinkEndpoints: Vector[LuaSinkEndpoint]
    ): Vector[LuaSinkEndpoint] = {
      val sourcePrototypeRef    = prototypeRef(sourceEndpoint.sourceRef)
      val reachablePrototypes   = reachableFrom(callGraphAdjacency, sourcePrototypeRef)
      val sourceSpecificSinks   = directReachableSinkRefsBySource.getOrElse(sourcePrototypeRef, Set.empty)
      val missingProvenanceRefs = Vector.newBuilder[String]
      val qualified = allSinkEndpoints.filter { sinkEndpoint =>
        val sinkPrototypeRef = sinkPrototypeByRef
          .get(sinkEndpoint.sinkRef)
          .getOrElse(
            throw new IllegalStateException(
              s"missing sink prototype mapping for path-search prefilter: source_ref=${sourceEndpoint.sourceRef} sink_ref=${sinkEndpoint.sinkRef}"
            )
          )
        attribution.incrementAggregate("source_reachability_check_count")
        val reachesAndHasSourceSpecificProvenance =
          if (!reachablePrototypes(sinkPrototypeRef)) {
            attribution.incrementAggregate("prototype_unreachable_pair_count")
            false
          } else if (sinkPrototypeRef == sourcePrototypeRef) {
            attribution.incrementAggregate("source_reachability_accepted_count")
            true
          } else if (sourceSpecificSinks.isEmpty) {
            missingProvenanceRefs += sinkEndpoint.sinkRef
            attribution.incrementAggregate("source_specific_provenance_pruned_pair_count")
            false
          } else {
            val accepted = sourceSpecificSinks(sinkEndpoint.sinkRef)
            attribution.incrementAggregate(
              if (accepted) "source_reachability_accepted_count"
              else "source_specific_provenance_pruned_pair_count"
            )
            accepted
          }
        val accepted = if (!reachesAndHasSourceSpecificProvenance) {
          false
        } else {
          attribution.incrementAggregate("path_constructor_check_count")
          val constructorAccepted = pathConstructorCanAttempt(sourceEndpoint, sinkEndpoint)
          attribution.incrementAggregate(
            if (constructorAccepted) "path_constructor_accepted_count" else "path_constructor_pruned_count"
          )
          if (!constructorAccepted) false
          else {
            attribution.incrementAggregate("parameter_position_check_count")
            val parameterAccepted = parameterPositionAllows(sourcePrototypeRef, sinkPrototypeRef, sinkEndpoint)
            attribution.incrementAggregate(
              if (parameterAccepted) "parameter_position_accepted_count" else "parameter_position_pruned_count"
            )
            parameterAccepted
          }
        }
        accepted
      }
      val missing = missingProvenanceRefs.result()
      if (missing.nonEmpty) {
        throw new IllegalStateException(
          "missing source-specific sink reachability provenance for path-search prefilter: " +
            s"source_ref=${sourceEndpoint.sourceRef} source_prototype_ref=$sourcePrototypeRef missing_sink_refs=${missing.sorted
                .mkString("[", ",", "]")}"
        )
      }
      qualified
    }

    private def parameterPositionAllows(
      sourcePrototypeRef: String,
      sinkPrototypeRef: String,
      sinkEndpoint: LuaSinkEndpoint
    ): Boolean =
      if (sinkPrototypeRef == sourcePrototypeRef) true
      else {
        val mappingKey = sourcePrototypeRef -> sinkPrototypeRef
        if (!directInterproceduralPairs(mappingKey)) true
        else {
          fedArgumentIndexesBySourceAndTarget.get(mappingKey) match {
            case Some(fedIndexes) => fedIndexes(sinkEndpoint.parameterIndex)
            case None =>
              throw new IllegalStateException(
                "missing fed-argument provenance for parameter-position filter: " +
                  s"source_prototype_ref=$sourcePrototypeRef sink_ref=${sinkEndpoint.sinkRef}"
              )
          }
        }
      }

    private def pathConstructorCanAttempt(sourceEndpoint: LuaSourceEndpoint, sinkEndpoint: LuaSinkEndpoint): Boolean = {
      val source           = parseQualifiedValueRef(sourceEndpoint.sourceRef)
      val sink             = parseQualifiedValueRef(sinkEndpoint.sinkRef)
      val bridgePrototypes = sourceScopedBridgePrototypes(sourceEndpoint.sourceRef, source)
      val samePrototypeForward =
        source.modulePath == sink.modulePath && source.prototypeId == sink.prototypeId && source.pc <= sink.pc
      samePrototypeForward || bridgePrototypes(prototypeRef(sink.modulePath, sink.prototypeId))
    }

    private def sourceScopedBridgePrototypes(sourceRef: String, source: QualifiedValueRef): Set[String] =
      sourceScopedBridgePrototypeCache.getOrElseUpdate(
        sourceRef, {
          val sourcePrototype = prototypeRef(source.modulePath, source.prototypeId)
          interproceduralFlowsBySourcePrototype
            .getOrElse(sourcePrototype, Vector.empty)
            .filter { flow =>
              attribution.incrementAggregate("bridge_argument_provenance_candidate_count")
              val accepted = requiredCallsitePc(flow.callsiteId) >= source.pc
              if (!accepted) {
                attribution.incrementAggregate("bridge_candidate_pc_pruned_count")
                attribution.incrementAggregate("early_candidate_short_circuit_count")
              }
              accepted
            }
            .flatMap { flow =>
              val targetPrototype = prototypeRef(flow.targetModulePath, flow.targetPrototypeId)
              reachablePrototypesIncludingSelf(targetPrototype)
            }
            .toSet
        }
      )

    private def reachablePrototypesIncludingSelf(startPrototype: String): Set[String] =
      reachablePrototypesByStartPrototype.getOrElseUpdate(
        startPrototype,
        reachableFrom(callGraphAdjacency, startPrototype)
      )

    private def buildCallGraphAdjacency(): Map[String, Set[String]] = {
      val adjacency = scala.collection.mutable.Map.empty[String, scala.collection.mutable.Set[String]]
      def addNode(ref: String): Unit =
        adjacency.getOrElseUpdate(ref, scala.collection.mutable.Set.empty)
      def addEdge(source: String, target: String): Unit = {
        addNode(source)
        addNode(target)
        adjacency(source) += target
      }

      modules.foreach { module =>
        module.prototypes.foreach(prototype => addNode(prototypeRef(module.path, prototype.prototypeId)))
        module.localCalls.foreach { call =>
          addEdge(
            prototypeRef(module.path, prototypeIdFromCallsiteId(call.callsiteId)),
            prototypeRef(module.path, call.targetPrototypeId)
          )
        }
      }
      fieldTargets.foreach { target =>
        addEdge(
          prototypeRef(target.fromModulePath, prototypeIdFromCallsiteId(target.callsiteId)),
          prototypeRef(target.targetModulePath, target.targetPrototypeId)
        )
      }
      interproceduralArgFlows.foreach { flow =>
        val source = parseQualifiedValueRef(flow.fromArgumentRef)
        addEdge(
          prototypeRef(source.modulePath, source.prototypeId),
          prototypeRef(flow.targetModulePath, flow.targetPrototypeId)
        )
      }
      interproceduralReturnFlows.foreach { flow =>
        val caller = parseQualifiedValueRef(flow.callerResultRef)
        addEdge(
          prototypeRef(flow.targetModulePath, flow.targetPrototypeId),
          prototypeRef(caller.modulePath, caller.prototypeId)
        )
      }
      adjacency.view.mapValues(_.toSet).toMap
    }

    private def buildDirectReachableSinkRefsBySource(): Map[String, Set[String]] = {
      val sinkRefsByPrototype = sinkPrototypeByRef.groupMap(_._2)(_._1).view.mapValues(_.toSet).toMap
      val reverseAdjacency    = scala.collection.mutable.Map.empty[String, scala.collection.mutable.Set[String]]
      callGraphAdjacency.foreach { case (sourcePrototype, targetPrototypes) =>
        reverseAdjacency.getOrElseUpdate(sourcePrototype, scala.collection.mutable.Set.empty)
        targetPrototypes.foreach { targetPrototype =>
          reverseAdjacency.getOrElseUpdate(targetPrototype, scala.collection.mutable.Set.empty) += sourcePrototype
        }
      }

      val result = scala.collection.mutable.Map.empty[String, scala.collection.mutable.Set[String]]
      sinkRefsByPrototype.foreach { case (sinkPrototypeRef, sinkRefs) =>
        val pending = scala.collection.mutable.Queue(sinkPrototypeRef)
        val seen    = scala.collection.mutable.Set.empty[String]
        while (pending.nonEmpty) {
          val prototypeRef = pending.dequeue()
          if (!seen(prototypeRef)) {
            seen += prototypeRef
            result.getOrElseUpdate(prototypeRef, scala.collection.mutable.Set.empty) ++= sinkRefs
            reverseAdjacency.getOrElse(prototypeRef, Set.empty).foreach(pending.enqueue(_))
          }
        }
      }
      result.view.mapValues(_.toSet).toMap
    }
  }

  private def reachableFrom(graph: Map[String, Set[String]], source: String): Set[String] = {
    val pending = scala.collection.mutable.Queue(source)
    val seen    = scala.collection.mutable.Set.empty[String]
    while (pending.nonEmpty) {
      val current = pending.dequeue()
      if (!seen(current)) {
        seen += current
        graph.getOrElse(current, Set.empty).diff(seen.toSet).toVector.sorted.foreach(pending.enqueue(_))
      }
    }
    seen.toSet
  }

  private def prototypeRef(valueRef: String): String = {
    val parsed = parseQualifiedValueRef(valueRef)
    prototypeRef(parsed.modulePath, parsed.prototypeId)
  }

  private def prototypeRefFromAnyQualifiedRef(valueRef: String): String =
    prototypeRef(modulePathFromQualifiedRef(valueRef), prototypeIdFromLocalValueRef(localRefFromQualifiedRef(valueRef)))

  private def prototypeRef(modulePath: String, prototypeId: String): String = s"$modulePath::$prototypeId"

  private def prototypeIdFromLocalValueRef(valueRef: String): String =
    valueRef.split("@pc", 2).headOption.getOrElse(valueRef).split(":r", 2).headOption.getOrElse(valueRef)

  private def representativeCallReturnEdges(module: ModuleSummary): Vector[(String, String)] =
    (module.prototypes.flatMap { prototype =>
      prototype.calls
        .filter(call => isRepresentativeTaintPreservingCall(call.resolvedName))
        .flatMap { call =>
          call.returnRefs.headOption.toVector.flatMap { returnRef =>
            call.argumentRefs.map(argumentRef => qualify(module.path, argumentRef) -> qualify(module.path, returnRef))
          }
        }
    } ++ module.fieldCalls
      .filter(call => isRepresentativeFieldReturnCall(call))
      .flatMap { call =>
        call.resultRef.toVector.flatMap { returnRef =>
          call.argumentRefs.map(argumentRef => qualify(module.path, argumentRef) -> qualify(module.path, returnRef))
        }
      }).distinct

  private def isRepresentativeFieldReturnCall(call: FieldCall): Boolean =
    call.resolvedName.exists(name => isRepresentativeTaintPreservingCall(Some(name)))

  private def isRepresentativeTaintPreservingCall(name: Option[String]): Boolean =
    name.exists(resolved =>
      isRepresentativeSanitizerCall(Some(resolved)) ||
        Set("ciphertextFormat", "json.encode", "cjson.encode", "string.format", "string.lower", "string.upper")
          .contains(resolved)
    )

  private def representativeTableValueEdges(module: ModuleSummary): Vector[(String, String)] =
    module.prototypes.flatMap { prototype =>
      val tableSourcesBySlot = scala.collection.mutable.Map.empty[Int, Set[String]].withDefaultValue(Set.empty)
      val edges              = Vector.newBuilder[(String, String)]
      prototype.instructions.sortBy(_.pc).foreach { instruction =>
        if (instruction.opcode == LuaOpcode.GetTable && tableSourcesBySlot(instruction.b).nonEmpty) {
          val tableRead = valueRef(prototype.prototypeId, instruction.pc, instruction.b)
          val result    = valueRef(prototype.prototypeId, instruction.pc, instruction.a)
          edges += qualify(module.path, tableRead) -> qualify(module.path, result)
        }
        representativeReadSlots(prototype, instruction).foreach { slot =>
          val read = valueRef(prototype.prototypeId, instruction.pc, slot)
          tableSourcesBySlot(slot).foreach { source =>
            edges += qualify(module.path, source) -> qualify(module.path, read)
          }
        }
        if (instruction.opcode == LuaOpcode.SetTable && instruction.b >= RkConstantBase) {
          for {
            valueSlot <- instruction.c.filter(_ < RkConstantBase)
          } {
            val tableRead = valueRef(prototype.prototypeId, instruction.pc, instruction.a)
            val valueRead = valueRef(prototype.prototypeId, instruction.pc, valueSlot)
            edges += qualify(module.path, valueRead) -> qualify(module.path, tableRead)
            tableSourcesBySlot += instruction.a      -> (tableSourcesBySlot(instruction.a) + valueRead)
          }
        }
        if (instruction.opcode == LuaOpcode.SetList && instruction.b > 0) {
          val tableRead = valueRef(prototype.prototypeId, instruction.pc, instruction.a)
          setListValueSlots(instruction).foreach { valueSlot =>
            val valueRead = valueRef(prototype.prototypeId, instruction.pc, valueSlot)
            edges += qualify(module.path, valueRead) -> qualify(module.path, tableRead)
            tableSourcesBySlot += instruction.a      -> (tableSourcesBySlot(instruction.a) + valueRead)
          }
        }
      }
      edges.result()
    }

  private def representativeParameterTableReadEdges(module: ModuleSummary): Vector[(String, String)] =
    module.prototypes.flatMap { prototype =>
      val parameterSlots = (0 until prototype.numParams).toSet
      prototype.instructions.collect {
        case instruction
            if instruction.opcode == LuaOpcode.GetTable &&
              parameterSlots(instruction.b) &&
              instruction.c.exists(_ >= RkConstantBase) =>
          qualify(module.path, valueRef(prototype.prototypeId, instruction.pc, instruction.b)) ->
            qualify(module.path, valueRef(prototype.prototypeId, instruction.pc, instruction.a))
      }
    }

  private def representativeIteratorValueEdges(module: ModuleSummary): Vector[(String, String)] =
    module.prototypes.flatMap { prototype =>
      prototype.calls
        .filter(call => call.resolvedName.contains("ipairs"))
        .flatMap { iteratorCall =>
          val iteratorInputs = iteratorCall.argumentRefs.map(qualify(module.path, _))
          for {
            callInstruction <- prototype.instructions
              .find(instruction => instruction.pc == iteratorCall.pc && instruction.opcode == LuaOpcode.Call)
              .toVector
            loop  <- ipairsTForLoop(prototype, callInstruction).toVector
            input <- iteratorInputs
            edge  <- tforLoopTableReadEdges(module.path, prototype, loop, input)
          } yield edge
        }
    }.distinct

  private final case class TForLoopRegion(bodyStartPc: Int, loopInstruction: LuaInstruction)

  private def ipairsTForLoop(prototype: PrototypeSummary, iteratorCall: LuaInstruction): Option[TForLoopRegion] =
    for {
      jump <- prototype.instructions.find(instruction =>
        instruction.pc == iteratorCall.pc + 1 && instruction.opcode == LuaOpcode.Jmp
      )
      tforLoop <- prototype.instructions
        .filter(instruction =>
          instruction.opcode == LuaOpcode.TForLoop &&
            instruction.a == iteratorCall.a &&
            instruction.pc > jump.pc
        )
        .sortBy(_.pc)
        .headOption
    } yield TForLoopRegion(jump.pc, tforLoop)

  private def tforLoopTableReadEdges(
    modulePath: String,
    prototype: PrototypeSummary,
    loop: TForLoopRegion,
    iteratorInput: String
  ): Vector[(String, String)] = {
    val tforLoop = loop.loopInstruction
    val loopValueSlots = tforLoop.c
      .map(count => (tforLoop.a + 3 until tforLoop.a + 3 + count).toSet)
      .getOrElse(Set.empty)
    prototype.instructions
      .filter(instruction => instruction.pc > loop.bodyStartPc && instruction.pc < tforLoop.pc)
      .collect {
        case instruction
            if instruction.opcode == LuaOpcode.GetTable &&
              loopValueSlots(instruction.b) &&
              instruction.c.exists(_ >= RkConstantBase) =>
          val tableRead = qualify(modulePath, valueRef(prototype.prototypeId, instruction.pc, instruction.b))
          val fieldRead = qualify(modulePath, valueRef(prototype.prototypeId, instruction.pc, instruction.a))
          Vector(iteratorInput -> tableRead, tableRead -> fieldRead)
      }
      .flatten
  }

  private def representativeExpressionResultEdges(module: ModuleSummary): Vector[(String, String)] =
    module.prototypes.flatMap { prototype =>
      val sortedInstructions = prototype.instructions.sortBy(_.pc)
      sortedInstructions.flatMap { instruction =>
        val result = valueRef(prototype.prototypeId, instruction.pc, instruction.a)
        val operandEdges = instruction.opcode match {
          case LuaOpcode.Add | LuaOpcode.Sub | LuaOpcode.Mul | LuaOpcode.Div | LuaOpcode.Mod | LuaOpcode.Pow =>
            (Vector(Some(instruction.b)) ++ Vector(instruction.c)).flatten
              .filter(_ < RkConstantBase)
              .map(slot =>
                qualify(module.path, valueRef(prototype.prototypeId, instruction.pc, slot)) -> qualify(
                  module.path,
                  result
                )
              )
          case LuaOpcode.Concat =>
            (instruction.b to instruction.c.getOrElse(instruction.b)).toVector
              .map(slot =>
                qualify(module.path, valueRef(prototype.prototypeId, instruction.pc, slot)) -> qualify(
                  module.path,
                  result
                )
              )
          case _ => Vector.empty
        }
        val reachingEdges =
          if (operandEdges.nonEmpty)
            expressionResultReadEdges(module.path, prototype, sortedInstructions, instruction)
          else Vector.empty
        operandEdges ++ reachingEdges
      }
    }.distinct

  private def expressionResultReadEdges(
    modulePath: String,
    prototype: PrototypeSummary,
    sortedInstructions: Vector[LuaInstruction],
    expression: LuaInstruction
  ): Vector[(String, String)] = {
    val resultSlot = expression.a
    val resultRef  = valueRef(prototype.prototypeId, expression.pc, resultSlot)
    val edges      = Vector.newBuilder[(String, String)]
    var stopped    = false
    sortedInstructions
      .filter(_.pc > expression.pc)
      .foreach { instruction =>
        if (!stopped) {
          representativeReadSlots(prototype, instruction)
            .filter(_ == resultSlot)
            .foreach(slot =>
              edges += qualify(modulePath, resultRef) -> qualify(
                modulePath,
                valueRef(prototype.prototypeId, instruction.pc, slot)
              )
            )
          if (overwritesSlot(instruction, resultSlot)) {
            stopped = true
          }
        }
      }
    edges.result()
  }

  private def overwritesSlot(instruction: LuaInstruction, slot: Int): Boolean =
    instruction.opcode match {
      case LuaOpcode.SetGlobal | LuaOpcode.SetUpval | LuaOpcode.SetTable | LuaOpcode.SetList | LuaOpcode.Return |
          LuaOpcode.Eq | LuaOpcode.Lt | LuaOpcode.Le | LuaOpcode.Test | LuaOpcode.Jmp =>
        false
      case LuaOpcode.Call | LuaOpcode.TailCall =>
        instruction.c match {
          case Some(0) => instruction.a <= slot
          case Some(1) => false
          case Some(n) => slot >= instruction.a && slot < instruction.a + n - 1
          case None    => false
        }
      case _ => instruction.a == slot
    }

  private def representativeReadSlots(prototype: PrototypeSummary, instruction: LuaInstruction): Vector[Int] =
    instruction.opcode match {
      case LuaOpcode.Move     => Vector(instruction.b)
      case LuaOpcode.GetTable => Vector(Some(instruction.b), instruction.c.filter(_ < RkConstantBase)).flatten
      case LuaOpcode.SetTable =>
        Vector(
          Some(instruction.a),
          Some(instruction.b).filter(_ < RkConstantBase),
          instruction.c.filter(_ < RkConstantBase)
        ).flatten
      case LuaOpcode.SetList => Vector(instruction.a) ++ setListValueSlots(instruction)
      case LuaOpcode.Call | LuaOpcode.TailCall =>
        (Vector(instruction.a) ++ callArgumentRefs(prototype, instruction).map(slotFromLocalValueRef)).distinct
      case LuaOpcode.Return =>
        instruction.b match {
          case 0 | 1 => Vector.empty
          case n     => (instruction.a until (instruction.a + n - 1)).toVector
        }
      case LuaOpcode.Eq | LuaOpcode.Lt | LuaOpcode.Le =>
        Vector(Some(instruction.b).filter(_ < RkConstantBase), instruction.c.filter(_ < RkConstantBase)).flatten
      case LuaOpcode.Add | LuaOpcode.Sub | LuaOpcode.Mul | LuaOpcode.Div | LuaOpcode.Mod | LuaOpcode.Pow =>
        Vector(Some(instruction.b).filter(_ < RkConstantBase), instruction.c.filter(_ < RkConstantBase)).flatten
      case LuaOpcode.Unm | LuaOpcode.Not | LuaOpcode.Len =>
        Vector(instruction.b)
      case LuaOpcode.Concat =>
        (instruction.b to instruction.c.getOrElse(instruction.b)).toVector
      case _ => Vector.empty
    }

  private def setListValueSlots(instruction: LuaInstruction): Vector[Int] =
    if (instruction.b > 0) (1 to instruction.b).map(offset => instruction.a + offset).toVector else Vector.empty

  private def valueRef(prototypeId: String, pc: Int, slot: Int): String =
    s"$prototypeId@pc$pc:r$slot"

  private def slotFromLocalValueRef(ref: String): Int =
    ref
      .split(":r", 2)
      .lift(1)
      .flatMap(_.toIntOption)
      .getOrElse(throw new IllegalArgumentException(s"Lua local value ref is missing slot: $ref"))

  private def sourceEndpointsForModule(module: ModuleSummary): Vector[LuaSourceEndpoint] = {
    val directEndpoints = module.prototypes.flatMap { prototype =>
      prototype.calls.flatMap { call =>
        call.resolvedName
          .filter(name => triggerMatches("*.formvalue", name))
          .flatMap(name =>
            call.returnRefs.headOption.map(sourceRef =>
              LuaSourceEndpoint(
                qualify(module.path, sourceRef),
                qualify(module.path, call.callsiteId),
                name,
                Provenance
              )
            )
          )
      }
    }
    val requireStringsByRef = module.requireCalls.flatMap(call => call.resultRef.zip(call.requireString)).toMap
    val requireFieldEndpoints = module.fieldCalls.flatMap { call =>
      for {
        requireRef    <- call.requireRef
        requireString <- requireStringsByRef.get(requireRef)
        if requireString == "luci.http" && call.fieldName == "formvalue"
        resultRef <- call.resultRef
      } yield LuaSourceEndpoint(
        qualify(module.path, resultRef),
        qualify(module.path, call.callsiteId),
        s"$requireString.${call.fieldName}",
        Provenance
      )
    }
    preferCanonicalSourceEndpoints(directEndpoints ++ requireFieldEndpoints)
  }

  private def preferCanonicalSourceEndpoints(endpoints: Vector[LuaSourceEndpoint]): Vector[LuaSourceEndpoint] =
    endpoints
      .groupBy(_.sourceRef)
      .values
      .map { candidates =>
        candidates.sortBy(endpoint => if (endpoint.trigger == "luci.http.formvalue") 0 else 1).head
      }
      .toVector

  private def sinkEndpointsForModule(
    module: ModuleSummary,
    attribution: PerformanceAttributionCollector
  ): Vector[LuaSinkEndpoint] =
    (module.prototypes.flatMap { prototype =>
      prototype.calls.flatMap { call =>
        call.resolvedName
          .flatMap(name => directCommandSinkTrigger(module, name))
          .flatMap { name =>
            sinkValueRef(call)
              .filter { argumentRef =>
                val accepted = !isConcreteStringArgument(prototype, call.pc, argumentRef)
                attribution.recordP1(accepted)
                accepted
              }
              .map(argumentRef =>
                LuaSinkEndpoint(
                  qualify(module.path, argumentRef),
                  qualify(module.path, call.callsiteId),
                  canonicalSinkTrigger(name),
                  0,
                  Provenance
                )
              )
          }
      }
    } ++ module.fieldCalls
      .flatMap(call =>
        fieldCallSinkTrigger(module, call).flatMap { trigger =>
          for {
            prototype   <- module.prototype(prototypeIdFromCallsiteId(call.callsiteId))
            pc          <- callsitePc(call.callsiteId)
            argumentRef <- call.argumentRefs.headOption
            accepted = !isConcreteStringArgument(prototype, pc, argumentRef)
            _        = attribution.recordP1(accepted)
            if accepted
          } yield LuaSinkEndpoint(
            qualify(module.path, argumentRef),
            qualify(module.path, call.callsiteId),
            trigger,
            0,
            Provenance
          )
        }
      )).distinct

  private def directCommandSinkTrigger(module: ModuleSummary, name: String): Option[String] =
    name match {
      case "os.execute" | "io.popen" => Some(canonicalSinkTrigger(name))
      case "forkExec" if modulePathDeclaresRequire(module.path, "xiaoqiang.common.XQFunction") =>
        Some("forkExec")
      case _ => None
    }

  private def fieldCallSinkTrigger(module: ModuleSummary, call: FieldCall): Option[String] =
    if (
      call.fieldName == "exec" &&
      (call.resolvedName.contains("luci.util.exec") ||
        fieldCallRequirePath(module, call).contains(modulePathForRequire("luci.util")))
    ) {
      Some("luci.util.exec")
    } else if (
      call.fieldName == "forkExec" &&
      fieldCallRequirePath(module, call).contains(modulePathForRequire("xiaoqiang.common.XQFunction"))
    ) {
      Some("xiaoqiang.common.XQFunction.forkExec")
    } else None

  private def fieldCallRequirePath(module: ModuleSummary, call: FieldCall): Option[String] =
    (
      call.requireRef.flatMap(module.requireResultRefs.get).toVector ++
        capturedUpvalueRequirePath(module, call).toVector
    ).distinct match {
      case Vector(single) => Some(single)
      case Vector()       => None
      case many =>
        throw new IllegalStateException(
          s"ambiguous field-call require path: module=${module.path} callsite=${call.callsiteId} paths=${many.sorted
              .mkString("[", ",", "]")}"
        )
    }

  private def capturedUpvalueRequirePath(module: ModuleSummary, call: FieldCall): Option[String] =
    for {
      targetRef <- call.targetRef
      target    <- registerWrite(targetRef)
      prototype <- module.prototype(prototypeIdFromCallsiteId(call.callsiteId))
      getTable <- prototype.instructions.find(instruction =>
        instruction.pc == target.pc &&
          instruction.a == target.slot &&
          instruction.opcode == LuaOpcode.GetTable
      )
      upvalueSlot <- getUpvalueBefore(prototype, getTable.pc, getTable.b)
      requireRef  <- module.capturedRequireRefs.get(capturedRequireRefKey(prototype.prototypeId, upvalueSlot))
      requirePath <- module.requireResultRefs.get(requireRef)
    } yield requirePath

  private def canonicalSinkTrigger(name: String): String =
    name match {
      case "require.popen" => "io.popen"
      case other           => other
    }

  private def sinkValueRef(call: ResolvedCall): Option[String] =
    call.argumentRefs.headOption

  private def isRepresentativeSanitizerCall(name: Option[String]): Boolean =
    name.exists { resolved =>
      resolved == "tonumber" ||
      resolved == "tostring" ||
      resolved == "string.format" ||
      resolved.endsWith("._cmdformat") ||
      resolved.endsWith(".macFormat") ||
      resolved.endsWith(".binaryBase64Enc")
    }

  private def ruleMatchesFor(
    sourceEndpoints: Vector[LuaSourceEndpoint],
    sinkEndpoints: Vector[LuaSinkEndpoint]
  ): Vector[LuaRuleMatch] = {
    val sourceRules =
      sourceEndpoints.map(row => LuaRuleMatch(row.callsiteId, "source", "formvalue", row.trigger, None, row.provenance))
    val sinkRules = sinkEndpoints.map(row =>
      LuaRuleMatch(
        row.callsiteId,
        "sink",
        finalSegment(row.trigger),
        row.trigger,
        Some(row.parameterIndex),
        row.provenance
      )
    )
    sourceRules ++ sinkRules
  }

  private def sanitizerCallsFor(modules: Vector[ModuleSummary]): Vector[LuaSanitizerCall] =
    modules.flatMap(sanitizerCallsForModule).distinct

  private def sanitizerCallsForModule(module: ModuleSummary): Vector[LuaSanitizerCall] = {
    val resolvedCalls = module.prototypes.flatMap { prototype =>
      prototype.calls.flatMap { call =>
        for {
          name <- call.resolvedName
          if isRealFirmwareSanitizerName(name)
          sanitizedValueRef <- sanitizerProducedValueRef(prototype, call)
        } yield LuaSanitizerCall(
          qualify(module.path, call.callsiteId),
          name,
          qualify(module.path, sanitizedValueRef),
          Provenance
        )
      }
    }
    resolvedCalls.distinct
  }

  private def realFirmwareSanitizerProducedRefs(modules: Vector[ModuleSummary]): Set[String] =
    modules
      .flatMap(sanitizerCallsForModule)
      .map(_.sanitizedValueRef)
      .toSet

  private def isRealFirmwareSanitizerName(name: String): Boolean =
    RealFirmwareSanitizerSuffixes.contains(name.split('.').lastOption.getOrElse(name))

  private def sanitizerProducedValueRef(prototype: PrototypeSummary, call: ResolvedCall): Option[String] =
    (call.returnRefs ++ callTargetValueRef(prototype, call).toVector).find(_.nonEmpty)

  private def callTargetValueRef(prototype: PrototypeSummary, call: ResolvedCall): Option[String] =
    prototype.instructions
      .find(instruction =>
        instruction.pc == call.pc && (instruction.opcode == LuaOpcode.Call || instruction.opcode == LuaOpcode.TailCall)
      )
      .map(instruction => s"${prototype.prototypeId}@pc${instruction.pc}:r${instruction.a}")

  private def sanitizerClassificationsFor(
    taintPaths: Vector[LuaTaintPath],
    sanitizerCalls: Vector[LuaSanitizerCall]
  ): Vector[LuaSanitizerClassification] =
    taintPaths.flatMap { path =>
      sanitizerCalls
        .flatMap { call =>
          val onChain        = path.pathSteps.contains(call.sanitizedValueRef)
          val comparableCall = sameModuleRef(call.sanitizedValueRef, path.sourceRef)
          val classification = if (onChain) "sanitized" else "not-sanitized"
          Option.when(onChain || comparableCall)(
            LuaSanitizerClassification(
              path.sourceRef,
              path.sinkRef,
              call.callsiteId,
              call.sanitizerName,
              appliesToSink = true,
              onDataflowChain = onChain,
              classification
            )
          )
        }
    }

  private def reportClassificationsFor(
    taintPaths: Vector[LuaTaintPath],
    sanitizerClassifications: Vector[LuaSanitizerClassification]
  ): Vector[LuaReportClassification] =
    taintPaths.map { path =>
      val sanitized = sanitizerClassifications.exists(row =>
        row.sourceRef == path.sourceRef && row.sinkRef == path.sinkRef && row.onDataflowChain
      )
      if (sanitized) {
        LuaReportClassification(path.sourceRef, path.sinkRef, "sanitized", "on-chain-sanitizer")
      } else {
        LuaReportClassification(path.sourceRef, path.sinkRef, "true-positive", "no-on-chain-sanitizer")
      }
    }

  private def vulnerabilityReportsFor(
    taintPaths: Vector[LuaTaintPath],
    reportClassifications: Vector[LuaReportClassification]
  ): Vector[LuaVulnerabilityReport] =
    taintPaths.flatMap { path =>
      reportClassifications
        .find(row => row.sourceRef == path.sourceRef && row.sinkRef == path.sinkRef)
        .filter(_.classification == "true-positive")
        .map(row =>
          LuaVulnerabilityReport(
            path.sourceRef,
            path.sinkRef,
            "path-proven",
            row.classification,
            path.pathSteps,
            path.provenance
          )
        )
    }

  private def semanticBoundaries(
    modules: Vector[ModuleSummary],
    resolutions: Vector[LuaModuleResolution],
    fieldTargets: Vector[LuaModuleFieldCallTarget]
  ): Vector[LuaE4Boundary] = {
    val resolutionBoundaries = resolutions.collect {
      case resolution if resolution.resolutionStatus != "matched" =>
        LuaE4Boundary(
          s"${resolution.fromModulePath}:require:${resolution.requireString}",
          "module-resolution",
          unresolvedReason(resolution)
        )
    }
    val unresolvedCalls = modules.flatMap { module =>
      module.unresolvedLocalCalls.map(call =>
        LuaE4Boundary(qualify(module.path, call.callsiteId), "interprocedural", "unresolved-callee")
      )
    }
    val fieldTargetCallsites = fieldTargets.map(_.callsiteId).toSet
    val missingFields = modules.flatMap { module =>
      module.fieldCalls
        .filter(call => !fieldTargetCallsites(call.callsiteId))
        .filter(call => module.requireResultRefs.keySet.exists(ref => call.requireRef.contains(ref)))
        .map(call => LuaE4Boundary(qualify(module.path, call.callsiteId), "module-field", "missing-export-field"))
    }
    resolutionBoundaries ++ unresolvedCalls ++ missingFields
  }

  private def qualify(modulePath: String, ref: String): String = s"$modulePath:$ref"

  private final case class ProgramArtifact(path: String, root: Option[LuaPrototype])

  private final case class ModuleSummary(
    path: String,
    declaredModuleNames: Set[String],
    prototypes: Vector[PrototypeSummary],
    requireCalls: Vector[RequireCall],
    requireResultRefs: Map[String, String],
    capturedRequireRefs: Map[String, String],
    exports: Vector[ModuleExport],
    fieldCalls: Vector[FieldCall],
    localCalls: Vector[LocalCall],
    unresolvedLocalCalls: Vector[UnresolvedLocalCall],
    localFlows: Vector[LuaLocalFlow],
    globalFlows: Vector[LuaGlobalFlow]
  ) {
    def prototype(id: String): Option[PrototypeSummary] = prototypes.find(_.prototypeId == id)
  }

  private object ModuleSummary {
    def fromArtifact(artifact: ProgramArtifact): Option[ModuleSummary] =
      artifact.root.map { root =>
        val capturedNames = capturedUpvalueNames(root)
        val prototypes = allPrototypes(root).map(prototype => PrototypeSummary.fromPrototype(prototype, capturedNames))
        val declaredNames = declaredModuleNames(root)
        val requireCalls  = prototypes.flatMap(detectRequireCalls)
        val exports       = detectExports(artifact.path, root)
        val localCalls =
          detectLocalCalls(root) ++ detectPlainModuleGlobalExportCalls(artifact.path, prototypes, exports)
        val localSemantic = LuaInstructionSemantics.normalize(root)
        val capturedRefs  = capturedRequireRefs(root, requireCalls)
        val fieldCalls =
          prototypes.flatMap(prototype => detectFieldCalls(artifact.path, prototype, requireCalls, capturedRefs))
        ModuleSummary(
          path = artifact.path,
          declaredModuleNames = declaredNames,
          prototypes = prototypes,
          requireCalls = requireCalls,
          requireResultRefs = requireCalls.collect {
            case call if call.resultRef.nonEmpty && call.requireString.exists(_.nonEmpty) =>
              call.resultRef.get -> modulePathForRequire(call.requireString.get)
          }.toMap,
          capturedRequireRefs = capturedRefs,
          exports = exports,
          fieldCalls = fieldCalls,
          localCalls = localCalls,
          unresolvedLocalCalls = localSemantic.unresolvedCalls.map(call => UnresolvedLocalCall(call.callsiteId)),
          localFlows = localSemantic.localFlows,
          globalFlows = localSemantic.globalFlows
        )
      }
  }

  private final case class PrototypeSummary(
    prototypeId: String,
    numParams: Int,
    maxStack: Int,
    instructions: Vector[LuaInstruction],
    constants: Vector[LuaConstant],
    parameterRefs: Vector[String],
    returnRefs: Vector[String],
    calls: Vector[ResolvedCall],
    capturedNames: Map[Int, String]
  )

  private object PrototypeSummary {
    def fromPrototype(
      prototype: LuaPrototype,
      capturedNamesByPrototype: Map[String, Map[Int, String]] = Map.empty
    ): PrototypeSummary = {
      val capturedNames = capturedNamesByPrototype.getOrElse(prototype.prototypeId, Map.empty)
      PrototypeSummary(
        prototype.prototypeId,
        prototype.numParams,
        prototype.maxStack,
        prototype.instructions,
        prototype.constants,
        (0 until prototype.numParams).map(slot => s"${prototype.prototypeId}:r$slot").toVector,
        returnRefs(prototype),
        resolvedCalls(prototype, capturedNames),
        capturedNames
      )
    }
  }

  private final case class RequireCall(
    callsiteId: String,
    prototypeId: String,
    resultRef: Option[String],
    requireString: Option[String]
  )

  private final case class ModuleExport(
    modulePath: String,
    tableRef: String,
    fieldName: String,
    targetPrototypeId: String
  )

  private final case class FieldCall(
    callsiteId: String,
    fieldName: String,
    resolvedName: Option[String],
    targetRef: Option[String],
    requireRef: Option[String],
    argumentRefs: Vector[String],
    resultRef: Option[String]
  )

  private final case class RegisterFieldTarget(
    fieldName: String,
    resolvedName: Option[String],
    targetRef: String,
    requireRef: Option[String]
  )

  private final case class LocalCall(
    callsiteId: String,
    targetPrototypeId: String,
    argumentRefs: Vector[String],
    resultRef: Option[String]
  )

  private final case class UnresolvedLocalCall(callsiteId: String)

  private final case class ResolvedCall(
    callsiteId: String,
    prototypeId: String,
    pc: Int,
    resolvedName: Option[String],
    argumentRefs: Vector[String],
    returnRefs: Vector[String]
  )

  private final class ModuleIndex(modules: Vector[ModuleSummary]) {
    private val byPath = modules.map(module => module.path -> module).toMap
    private val byDeclaredName =
      modules.flatMap(module => module.declaredModuleNames.map(name => name -> module)).groupMap(_._1)(_._2)

    def module(path: String): Option[ModuleSummary] = byPath.get(path)

    def resolve(requireString: String): ModuleResolutionResult =
      if (requireString.isEmpty) ModuleResolutionResult.Unresolved
      else {
        val requiredSuffix = modulePathForRequire(requireString)
        val pathCandidates =
          modules.filter(module => module.path == requiredSuffix || module.path.endsWith(s"/$requiredSuffix"))
        val candidates = (pathCandidates ++ byDeclaredName.getOrElse(requireString, Vector.empty)).distinct
          .sortBy(module => (module.path.count(_ == '/'), module.path))
        candidates.headOption match {
          case Some(module) => ModuleResolutionResult.Matched(module)
          case None         => ModuleResolutionResult.Unresolved
        }
      }
  }

  private enum ModuleResolutionResult {
    case Matched(module: ModuleSummary)
    case Unresolved
  }

  private def allPrototypes(prototype: LuaPrototype): Vector[LuaPrototype] =
    prototype +: prototype.nested.flatMap(allPrototypes)

  private def detectRequireCalls(prototype: PrototypeSummary): Vector[RequireCall] =
    prototype.instructions
      .filter(instruction => instruction.opcode == LuaOpcode.Call && isGlobalName(prototype, instruction.a, "require"))
      .map { instruction =>
        val argumentString = precedingLoadString(prototype, instruction.pc, instruction.a + 1)
        RequireCall(
          callsiteId = s"${prototype.prototypeId}@pc${instruction.pc}",
          prototypeId = prototype.prototypeId,
          resultRef = callReturnRefs(instruction).headOption.map(ref => s"${prototype.prototypeId}$ref"),
          requireString = argumentString
        )
      }

  private def detectExports(path: String, root: LuaPrototype): Vector[ModuleExport] = {
    val returnedTableExports = root.instructions
      .filter(instruction => instruction.opcode == LuaOpcode.SetTable)
      .flatMap { instruction =>
        for {
          fieldName <- constantName(root.constants, instruction.b - RkConstantBase)
            .filter(_ => instruction.b >= RkConstantBase)
          targetSlot <- instruction.c
          target     <- closureInSlotBefore(root, instruction.pc, targetSlot)
          if isReturnedTable(root, instruction.a)
        } yield ModuleExport(path, s"${root.prototypeId}:r${instruction.a}", fieldName, target)
      }
    returnedTableExports ++ detectPlainModuleGlobalExports(path, root) ++
      detectPlainModuleGlobalTableFieldExports(path, root)
  }

  private def detectPlainModuleGlobalExports(path: String, root: LuaPrototype): Vector[ModuleExport] =
    if (hasPlainModuleLiteralCall(root)) {
      root.instructions
        .filter(_.opcode == LuaOpcode.SetGlobal)
        .flatMap { instruction =>
          for {
            fieldName <- constantName(root.constants, instruction.b)
            target    <- closureInSlotBefore(root, instruction.pc, instruction.a)
          } yield ModuleExport(path, s"$path:module-global", fieldName, target)
        }
    } else Vector.empty

  private def detectPlainModuleGlobalTableFieldExports(path: String, root: LuaPrototype): Vector[ModuleExport] = {
    val summary          = PrototypeSummary.fromPrototype(root)
    val registerStrings  = scala.collection.mutable.Map.empty[Int, String]
    val registerClosures = scala.collection.mutable.Map.empty[Int, String]
    val registerTables   = scala.collection.mutable.Map.empty[Int, String]
    val tableFields      = scala.collection.mutable.Map.empty[String, scala.collection.mutable.Map[String, String]]
    val globalTables     = scala.collection.mutable.Map.empty[String, String]
    val prototypeIds     = allPrototypes(root).map(_.prototypeId).toSet
    var moduleCallSeen   = false

    def clearRegister(slot: Int): Unit = {
      registerStrings -= slot
      registerClosures -= slot
      registerTables -= slot
    }

    def copyRegister(dest: Int, source: Int): Unit = {
      val stringValue  = registerStrings.get(source)
      val closureValue = registerClosures.get(source)
      val tableValue   = registerTables.get(source)
      clearRegister(dest)
      stringValue.foreach(registerStrings += dest -> _)
      closureValue.foreach(registerClosures += dest -> _)
      tableValue.foreach(registerTables += dest -> _)
    }

    def clearCallWrites(instruction: LuaInstruction): Unit =
      callReturnRefs(instruction).flatMap(registerWrite).foreach(write => clearRegister(write.slot))

    def setTableKey(instruction: LuaInstruction): Option[String] =
      if (instruction.b >= RkConstantBase) constantName(root.constants, instruction.b - RkConstantBase)
      else registerStrings.get(instruction.b)

    def setTableClosureValue(instruction: LuaInstruction): Option[String] =
      instruction.c.filter(_ < RkConstantBase).flatMap(registerClosures.get)

    root.instructions.sortBy(_.pc).foreach {
      case instruction if instruction.opcode == LuaOpcode.LoadK =>
        clearRegister(instruction.a)
        constantName(root.constants, instruction.b).foreach(registerStrings += instruction.a -> _)

      case instruction if instruction.opcode == LuaOpcode.Closure =>
        clearRegister(instruction.a)
        registerClosures += instruction.a -> s"${root.prototypeId}.${instruction.b}"

      case instruction if instruction.opcode == LuaOpcode.NewTable =>
        clearRegister(instruction.a)
        val tableRef = s"${root.prototypeId}@pc${instruction.pc}:r${instruction.a}"
        registerTables += instruction.a -> tableRef
        tableFields.getOrElseUpdate(tableRef, scala.collection.mutable.Map.empty)

      case instruction if instruction.opcode == LuaOpcode.Move =>
        copyRegister(instruction.a, instruction.b)

      case instruction if instruction.opcode == LuaOpcode.GetGlobal =>
        clearRegister(instruction.a)
        constantName(root.constants, instruction.b).foreach { fieldName =>
          globalTables.get(fieldName).foreach(registerTables += instruction.a -> _)
        }

      case instruction if instruction.opcode == LuaOpcode.SetGlobal =>
        for {
          fieldName <- constantName(root.constants, instruction.b)
          tableRef  <- registerTables.get(instruction.a)
          if moduleCallSeen
        } globalTables += fieldName -> tableRef

      case instruction if instruction.opcode == LuaOpcode.SetTable =>
        for {
          tableRef <- registerTables.get(instruction.a)
          key      <- setTableKey(instruction)
          target   <- setTableClosureValue(instruction)
        } tableFields.getOrElseUpdate(tableRef, scala.collection.mutable.Map.empty) += key -> target

      case instruction if instruction.opcode == LuaOpcode.Call || instruction.opcode == LuaOpcode.TailCall =>
        if (
          summary.calls.exists(call => call.callsiteId == s"${root.prototypeId}@pc${instruction.pc}") &&
          callTargetNameAt(summary, instruction.pc, instruction.a).contains("module") &&
          precedingLoadString(summary, instruction.pc, instruction.a + 1).nonEmpty
        ) {
          moduleCallSeen = true
        }
        clearCallWrites(instruction)

      case instruction if instruction.opcode == LuaOpcode.Return || instruction.opcode == LuaOpcode.SetList =>

      case instruction =>
        clearRegister(instruction.a)
    }

    globalTables.toVector.flatMap { case (globalName, tableRef) =>
      tableFields.get(tableRef).toVector.flatMap { fields =>
        fields.toVector.collect {
          case (fieldName, targetPrototypeId) if prototypeIds(targetPrototypeId) =>
            ModuleExport(path, s"$path:module-global", s"$globalName.$fieldName", targetPrototypeId)
        }
      }
    }.distinct
  }

  private def hasPlainModuleLiteralCall(root: LuaPrototype): Boolean = {
    declaredModuleNames(root).nonEmpty
  }

  private def declaredModuleNames(root: LuaPrototype): Set[String] = {
    val summary = PrototypeSummary.fromPrototype(root)
    summary.calls.flatMap { call =>
      if (call.resolvedName.contains("module")) {
        call.argumentRefs.headOption
          .flatMap(ref => registerWrite(ref))
          .flatMap(write => precedingStringConstant(summary, call.pc, write.slot))
      } else None
    }.toSet
  }

  private def detectLocalCalls(root: LuaPrototype): Vector[LocalCall] = {
    val semantics = LuaInstructionSemantics.normalize(root)
    val closureCalls = semantics.callTargetCandidates.flatMap { candidate =>
      semantics.callSites
        .find(_.callsiteId == candidate.callsiteId)
        .map { callsite =>
          LocalCall(
            callsiteId = callsite.callsiteId,
            targetPrototypeId = candidate.targetRef,
            argumentRefs = callsite.firstArgSlot
              .zip(callsite.argCount)
              .toVector
              .flatMap { case (firstSlot, count) =>
                (firstSlot until (firstSlot + count)).map(slot => s"${callsite.prototypeId}@pc${callsite.pc}:r$slot")
              },
            resultRef = callsite.firstReturnSlot.map(slot => s"${callsite.prototypeId}@pc${callsite.pc}:r$slot")
          )
        }
    }
    closureCalls.distinct
  }

  private def detectPlainModuleGlobalExportCalls(
    path: String,
    prototypes: Vector[PrototypeSummary],
    exports: Vector[ModuleExport]
  ): Vector[LocalCall] = {
    val uniquePlainGlobalExports = exports
      .filter(_.tableRef == s"$path:module-global")
      .groupBy(_.fieldName)
      .collect {
        case (fieldName, exportsForField) if exportsForField.size == 1 =>
          fieldName -> exportsForField.head.targetPrototypeId
      }
      .toMap

    prototypes.flatMap { prototype =>
      prototype.calls.flatMap { call =>
        for {
          resolvedName      <- call.resolvedName
          targetPrototypeId <- uniquePlainGlobalExports.get(resolvedName)
        } yield LocalCall(
          callsiteId = call.callsiteId,
          targetPrototypeId = targetPrototypeId,
          argumentRefs = call.argumentRefs,
          resultRef = call.returnRefs.headOption
        )
      }
    }.distinct
  }

  private def detectFieldCalls(
    path: String,
    prototype: PrototypeSummary,
    requireCalls: Vector[RequireCall],
    capturedRequireRefs: Map[String, String]
  ): Vector[FieldCall] = {
    val requireCallsByCallsite = requireCalls.map(call => call.callsiteId -> call).toMap
    val requireStringByRef = requireCalls
      .flatMap(call => call.resultRef.zip(call.requireString))
      .toMap
    val registerNames        = scala.collection.mutable.Map.empty[Int, Vector[String]]
    val registerRequireRefs  = scala.collection.mutable.Map.empty[Int, String]
    val registerFieldTargets = scala.collection.mutable.Map.empty[Int, RegisterFieldTarget]
    val rows                 = Vector.newBuilder[FieldCall]

    def clearRegister(slot: Int): Unit = {
      registerNames -= slot
      registerRequireRefs -= slot
      registerFieldTargets -= slot
    }

    def clearCallWrites(instruction: LuaInstruction): Unit =
      callReturnRefs(instruction).flatMap(registerWrite).foreach(write => clearRegister(write.slot))

    def clearDefaultWrite(instruction: LuaInstruction): Unit =
      instruction.opcode match {
        case LuaOpcode.SetGlobal | LuaOpcode.SetTable | LuaOpcode.SetList | LuaOpcode.Return | LuaOpcode.Eq |
            LuaOpcode.Lt | LuaOpcode.Le | LuaOpcode.Test | LuaOpcode.Jmp =>
        case LuaOpcode.Call | LuaOpcode.TailCall =>
          clearCallWrites(instruction)
        case _ =>
          clearRegister(instruction.a)
      }

    def copyRegister(dest: Int, source: Int): Unit = {
      clearRegister(dest)
      registerNames.get(source).foreach(registerNames += dest -> _)
      registerRequireRefs.get(source).foreach(registerRequireRefs += dest -> _)
      registerFieldTargets.get(source).foreach(registerFieldTargets += dest -> _)
    }

    prototype.instructions.sortBy(_.pc).foreach {
      case instruction if instruction.opcode == LuaOpcode.GetGlobal =>
        clearRegister(instruction.a)
        constantName(prototype.constants, instruction.b).foreach(name => registerNames += instruction.a -> Vector(name))

      case instruction if instruction.opcode == LuaOpcode.GetUpval =>
        clearRegister(instruction.a)
        capturedRequireRefs.get(capturedRequireRefKey(prototype.prototypeId, instruction.b)).foreach { ref =>
          registerRequireRefs += instruction.a -> ref
          requireStringByRef.get(ref).foreach(name => registerNames += instruction.a -> Vector(name))
        }
        prototype.capturedNames.get(instruction.b).foreach(name => registerNames += instruction.a -> Vector(name))

      case instruction if instruction.opcode == LuaOpcode.Move =>
        copyRegister(instruction.a, instruction.b)

      case instruction if instruction.opcode == LuaOpcode.GetTable =>
        val key        = fieldName(prototype.constants, instruction)
        val baseName   = registerNames.get(instruction.b)
        val baseTarget = registerFieldTargets.get(instruction.b)
        val requireRef = registerRequireRefs.get(instruction.b).orElse(baseTarget.flatMap(_.requireRef))
        clearRegister(instruction.a)
        key.foreach { field =>
          val resolvedName = baseName.map(parts => (parts :+ field).mkString("."))
          baseName.foreach(parts => registerNames += instruction.a -> (parts :+ field))
          requireRef.foreach(registerRequireRefs += instruction.a -> _)
          if (requireRef.nonEmpty) {
            val exportField = baseTarget.map(target => s"${target.fieldName}.$field").getOrElse(field)
            registerFieldTargets += instruction.a -> RegisterFieldTarget(
              exportField,
              resolvedName,
              s"${prototype.prototypeId}@pc${instruction.pc}:r${instruction.a}",
              requireRef
            )
          }
        }

      case instruction if instruction.opcode == LuaOpcode.Self =>
        val key        = instruction.c.flatMap(value => constantName(prototype.constants, value - RkConstantBase))
        val baseName   = registerNames.get(instruction.b)
        val baseTarget = registerFieldTargets.get(instruction.b)
        val requireRef = registerRequireRefs.get(instruction.b).orElse(baseTarget.flatMap(_.requireRef))
        clearRegister(instruction.a)
        copyRegister(instruction.a + 1, instruction.b)
        key.foreach { field =>
          val resolvedName = baseName.map(parts => (parts :+ field).mkString("."))
          baseName.foreach(parts => registerNames += instruction.a -> (parts :+ field))
          requireRef.foreach(registerRequireRefs += instruction.a -> _)
          if (requireRef.nonEmpty) {
            val exportField = baseTarget.map(target => s"${target.fieldName}.$field").getOrElse(field)
            registerFieldTargets += instruction.a -> RegisterFieldTarget(
              exportField,
              resolvedName,
              s"${prototype.prototypeId}@pc${instruction.pc}:r${instruction.a}",
              requireRef
            )
          }
        }

      case instruction if instruction.opcode == LuaOpcode.Call || instruction.opcode == LuaOpcode.TailCall =>
        val callsiteId = s"${prototype.prototypeId}@pc${instruction.pc}"
        registerFieldTargets.get(instruction.a).foreach { target =>
          rows += FieldCall(
            callsiteId,
            target.fieldName,
            target.resolvedName,
            Some(target.targetRef),
            target.requireRef,
            callArgumentRefs(prototype, instruction),
            callReturnRefs(instruction).headOption.map(ref => s"${prototype.prototypeId}$ref")
          )
        }
        val requireCall = requireCallsByCallsite.get(callsiteId).filter { call =>
          call.requireString.nonEmpty && callTargetNameAt(prototype, instruction.pc, instruction.a).contains("require")
        }
        clearCallWrites(instruction)
        requireCall.foreach { call =>
          call.resultRef.foreach { ref =>
            registerWrite(ref)
              .filter(_.slot == instruction.a)
              .foreach { _ =>
                registerRequireRefs += instruction.a -> ref
                call.requireString.foreach(name => registerNames += instruction.a -> Vector(name))
              }
          }
        }

      case instruction
          if instruction.opcode == LuaOpcode.LoadK || instruction.opcode == LuaOpcode.LoadBool ||
            instruction.opcode == LuaOpcode.LoadNil || instruction.opcode == LuaOpcode.NewTable ||
            instruction.opcode == LuaOpcode.Closure || instruction.opcode == LuaOpcode.Vararg =>
        clearRegister(instruction.a)

      case instruction =>
        clearDefaultWrite(instruction)
    }

    val statefulRows = rows.result()
    val directRows = prototype.instructions
      .filter(instruction => instruction.opcode == LuaOpcode.Call || instruction.opcode == LuaOpcode.TailCall)
      .flatMap { instruction =>
        precedingGetTable(prototype, instruction.pc, instruction.a).flatMap { getTable =>
          fieldName(prototype.constants, getTable).map { field =>
            val baseName = callTargetNameAt(prototype, getTable.pc, getTable.b)
            FieldCall(
              callsiteId = s"${prototype.prototypeId}@pc${instruction.pc}",
              fieldName = field,
              resolvedName = baseName.map(name => s"$name.$field"),
              targetRef = Some(s"${prototype.prototypeId}@pc${getTable.pc}:r${getTable.a}"),
              requireRef = requireRefForGetTable(prototype, getTable, requireCalls, capturedRequireRefs),
              argumentRefs = callArgumentRefs(prototype, instruction),
              resultRef = callReturnRefs(instruction).headOption.map(ref => s"${prototype.prototypeId}$ref")
            )
          }
        }
      }
    (statefulRows ++ directRows).distinct
  }

  private def requireRefForGetTable(
    prototype: PrototypeSummary,
    getTable: LuaInstruction,
    requireCalls: Vector[RequireCall],
    capturedRequireRefs: Map[String, String]
  ): Option[String] = {
    getUpvalueBefore(prototype, getTable.pc, getTable.b) match {
      case Some(upvalueSlot) =>
        capturedRequireRefs.get(capturedRequireRefKey(prototype.prototypeId, upvalueSlot))
      case None =>
        localRequireRefForGetTable(prototype, getTable, requireCalls)
    }
  }

  private def localRequireRefForGetTable(
    prototype: PrototypeSummary,
    getTable: LuaInstruction,
    requireCalls: Vector[RequireCall]
  ): Option[String] =
    requireCalls
      .filter(_.prototypeId == prototype.prototypeId)
      .flatMap(_.resultRef)
      .flatMap(ref => registerWrite(ref).map(write => write -> ref))
      .filter { case (write, _) => write.slot == getTable.b && write.pc < getTable.pc }
      .sortBy { case (write, _) => write.pc }
      .lastOption
      .map { case (_, ref) => ref }

  private def capturedRequireRefs(root: LuaPrototype, requireCalls: Vector[RequireCall]): Map[String, String] = {
    def collect(prototype: LuaPrototype, inheritedUpvalueRefs: Map[Int, String]): Map[String, String] = {
      val registerRequireRefs = scala.collection.mutable.Map.empty[Int, (Int, String)]
      val captured            = scala.collection.mutable.Map.empty[String, String]
      val sorted              = prototype.instructions.sortBy(_.pc)
      val bindingPcs          = closureBindingPcs(prototype)

      def clearSlot(slot: Int): Unit =
        registerRequireRefs -= slot

      def newestRequireRef(slot: Int, pc: Int): Option[String] =
        registerRequireRefs
          .get(slot)
          .filter(_._1 < pc)
          .map(_._2)

      def boundLocalRequireRef(slot: Int, closurePc: Int): Option[String] =
        requireCalls
          .filter(call => call.prototypeId == prototype.prototypeId && call.requireString.nonEmpty)
          .flatMap(call => call.resultRef.flatMap(ref => registerWrite(ref).map(write => (write, ref))))
          .filter { case (write, _) =>
            write.slot == slot &&
            write.pc < closurePc &&
            !hasRuntimeOverwrite(slot, write.pc, closurePc, sorted, bindingPcs)
          }
          .sortBy { case (write, _) => write.pc }
          .lastOption
          .map { case (_, ref) => ref }

      def childUpvalueRefs(closure: LuaInstruction): Map[Int, String] = {
        val childPrototypeId = s"${prototype.prototypeId}.${closure.b}"
        prototype.nested
          .find(_.prototypeId == childPrototypeId)
          .map { child =>
            sorted
              .dropWhile(_.pc <= closure.pc)
              .take(effectiveUpvalueCount(child))
              .zipWithIndex
              .flatMap {
                case (binder, upvalueSlot) if binder.opcode == LuaOpcode.Move =>
                  boundLocalRequireRef(binder.b, closure.pc).map(upvalueSlot -> _)
                case (binder, upvalueSlot) if binder.opcode == LuaOpcode.GetUpval =>
                  inheritedUpvalueRefs.get(binder.b).map(upvalueSlot -> _)
                case _ => None
              }
              .toMap
          }
          .getOrElse(Map.empty)
      }

      sorted.foreach {
        case instruction if bindingPcs(instruction.pc) =>
        case instruction if instruction.opcode == LuaOpcode.Call =>
          val callsiteId = s"${prototype.prototypeId}@pc${instruction.pc}"
          val resolvedRequire = requireCalls
            .find(call =>
              call.prototypeId == prototype.prototypeId &&
                call.callsiteId == callsiteId &&
                call.requireString.nonEmpty
            )
            .flatMap(_.resultRef)
          clearSlot(instruction.a)
          resolvedRequire.foreach(ref => registerRequireRefs += instruction.a -> (instruction.pc -> ref))
        case instruction if instruction.opcode == LuaOpcode.GetUpval =>
          val inherited = inheritedUpvalueRefs.get(instruction.b)
          clearSlot(instruction.a)
          inherited.foreach(ref => registerRequireRefs += instruction.a -> (instruction.pc -> ref))
        case instruction if instruction.opcode == LuaOpcode.Move =>
          val moved = newestRequireRef(instruction.b, instruction.pc)
          clearSlot(instruction.a)
          moved.foreach(ref => registerRequireRefs += instruction.a -> (instruction.pc -> ref))
        case instruction if instruction.opcode == LuaOpcode.Closure =>
          clearSlot(instruction.a)
          val childPrototypeId = s"${prototype.prototypeId}.${instruction.b}"
          val childRefs        = childUpvalueRefs(instruction)
          childRefs.foreach { case (upvalueSlot, ref) =>
            captured += capturedRequireRefKey(childPrototypeId, upvalueSlot) -> ref
          }
          prototype.nested
            .find(_.prototypeId == childPrototypeId)
            .foreach(child => captured ++= collect(child, childRefs))
        case instruction
            if instruction.opcode == LuaOpcode.LoadK || instruction.opcode == LuaOpcode.LoadBool ||
              instruction.opcode == LuaOpcode.LoadNil || instruction.opcode == LuaOpcode.GetGlobal ||
              instruction.opcode == LuaOpcode.GetTable || instruction.opcode == LuaOpcode.NewTable ||
              instruction.opcode == LuaOpcode.Self || instruction.opcode == LuaOpcode.Vararg ||
              instruction.opcode == LuaOpcode.TailCall =>
          clearSlot(instruction.a)
        case _ =>
      }

      captured.toMap
    }

    collect(root, Map.empty)
  }

  private def hasRuntimeOverwrite(
    slot: Int,
    fromPc: Int,
    toPc: Int,
    sortedInstructions: Vector[LuaInstruction],
    bindingPcs: Set[Int]
  ): Boolean =
    sortedInstructions.exists { instruction =>
      instruction.pc > fromPc &&
      instruction.pc < toPc &&
      !bindingPcs(instruction.pc) &&
      !(instruction.opcode == LuaOpcode.Move && instruction.a == slot && instruction.b == slot) &&
      overwritesSlot(instruction, slot)
    }

  private def parameterFlowsToCallTarget(prototype: PrototypeSummary, parameterSlot: Int): Boolean = {
    var aliases = Set(parameterSlot)
    prototype.instructions.sortBy(_.pc).exists { instruction =>
      val isTarget =
        (instruction.opcode == LuaOpcode.Call || instruction.opcode == LuaOpcode.TailCall) && aliases(instruction.a)
      if (!isTarget) {
        instruction.opcode match {
          case LuaOpcode.Move =>
            val sourceIsAlias = aliases(instruction.b)
            if (aliases(instruction.a)) {
              aliases -= instruction.a
            }
            if (sourceIsAlias) {
              aliases += instruction.a
            }
          case LuaOpcode.LoadK | LuaOpcode.LoadBool | LuaOpcode.LoadNil | LuaOpcode.GetUpval | LuaOpcode.GetGlobal |
              LuaOpcode.GetTable | LuaOpcode.NewTable | LuaOpcode.Self |
              LuaOpcode.Closure | LuaOpcode.Vararg | LuaOpcode.Call | LuaOpcode.TailCall =>
            aliases -= instruction.a
          case _ =>
        }
      }
      isTarget
    }
  }

  private final case class RegisterWrite(pc: Int, slot: Int)

  private def registerWrite(ref: String): Option[RegisterWrite] =
    ref.split("@pc", 2).lift(1).flatMap { afterPc =>
      afterPc.split(":r", 2).toList match {
        case pcText :: slotText :: Nil =>
          pcText.toIntOption.zip(slotText.toIntOption).map((pc, slot) => RegisterWrite(pc, slot))
        case _ => None
      }
    }

  private def getUpvalueBefore(prototype: PrototypeSummary, pc: Int, slot: Int): Option[Int] =
    prototype.instructions
      .filter(instruction => instruction.pc < pc && instruction.a == slot && instruction.opcode == LuaOpcode.GetUpval)
      .lastOption
      .map(_.b)

  private def capturedRequireRefKey(prototypeId: String, upvalueSlot: Int): String = s"$prototypeId:u$upvalueSlot"

  private def capturedUpvalueNames(root: LuaPrototype): Map[String, Map[Int, String]] = {
    def collect(prototype: LuaPrototype, currentUpvalueNames: Map[Int, String]): Map[String, Map[Int, String]] = {
      val namesBySlot         = scala.collection.mutable.Map.empty[Int, String]
      val capturedByPrototype = scala.collection.mutable.Map.empty[String, Map[Int, String]]
      val bindingPcs          = closureBindingPcs(prototype)
      def clearWrittenSlot(instruction: LuaInstruction): Unit =
        namesBySlot -= instruction.a
      def childUpvalueNames(closure: LuaInstruction): Map[Int, String] = {
        val childPrototypeId = s"${prototype.prototypeId}.${closure.b}"
        prototype.nested
          .find(_.prototypeId == childPrototypeId)
          .map { child =>
            prototype.instructions
              .filter(_.pc > closure.pc)
              .sortBy(_.pc)
              .take(effectiveUpvalueCount(child))
              .zipWithIndex
              .flatMap {
                case (binder, upvalueSlot) if binder.opcode == LuaOpcode.Move =>
                  namesBySlot.get(binder.b).map(name => upvalueSlot -> name)
                case (binder, upvalueSlot) if binder.opcode == LuaOpcode.GetUpval =>
                  currentUpvalueNames.get(binder.b).map(name => upvalueSlot -> name)
                case _ => None
              }
              .toMap
          }
          .getOrElse(Map.empty)
      }

      prototype.instructions.sortBy(_.pc).foreach {
        case instruction if bindingPcs(instruction.pc) =>
        case instruction if instruction.opcode == LuaOpcode.GetGlobal =>
          clearWrittenSlot(instruction)
          constantName(prototype.constants, instruction.b).foreach(name => namesBySlot += instruction.a -> name)
        case instruction if instruction.opcode == LuaOpcode.GetUpval =>
          clearWrittenSlot(instruction)
          currentUpvalueNames.get(instruction.b).foreach(name => namesBySlot += instruction.a -> name)
        case instruction if instruction.opcode == LuaOpcode.GetTable =>
          val resolved = for {
            base  <- namesBySlot.get(instruction.b)
            field <- fieldName(prototype.constants, instruction)
          } yield s"$base.$field"
          clearWrittenSlot(instruction)
          resolved.foreach(name => namesBySlot += instruction.a -> name)
        case instruction if instruction.opcode == LuaOpcode.Move =>
          val resolved = namesBySlot.get(instruction.b)
          clearWrittenSlot(instruction)
          resolved.foreach(name => namesBySlot += instruction.a -> name)
        case instruction if instruction.opcode == LuaOpcode.Closure =>
          val childPrototypeId = s"${prototype.prototypeId}.${instruction.b}"
          val childNames       = childUpvalueNames(instruction)
          capturedByPrototype += childPrototypeId -> childNames
          prototype.nested
            .find(_.prototypeId == childPrototypeId)
            .foreach(child => capturedByPrototype ++= collect(child, childNames))
        case instruction if instruction.opcode == LuaOpcode.Call =>
          val resolved = for {
            targetName <- namesBySlot.get(instruction.a)
            if targetName == "require"
            if instruction.c.forall(_ != 1)
            moduleName <- precedingLoadString(prototype, instruction.pc, instruction.a + 1)
          } yield moduleName
          clearWrittenSlot(instruction)
          resolved.foreach(name => namesBySlot += instruction.a -> name)
        case instruction if instruction.opcode == LuaOpcode.LoadK =>
          clearWrittenSlot(instruction)
        case instruction
            if instruction.opcode == LuaOpcode.LoadBool || instruction.opcode == LuaOpcode.LoadNil ||
              instruction.opcode == LuaOpcode.NewTable || instruction.opcode == LuaOpcode.Vararg ||
              instruction.opcode == LuaOpcode.TailCall =>
          clearWrittenSlot(instruction)
        case instruction
            if instruction.opcode != LuaOpcode.SetGlobal && instruction.opcode != LuaOpcode.SetTable &&
              instruction.opcode != LuaOpcode.SetUpval && instruction.opcode != LuaOpcode.Return =>
          clearWrittenSlot(instruction)
        case _ =>
      }

      capturedByPrototype.toMap
    }

    collect(root, Map.empty)
  }

  private def closureBindingPcs(prototype: LuaPrototype): Set[Int] =
    prototype.instructions
      .sortBy(_.pc)
      .zipWithIndex
      .flatMap {
        case (closure, index) if closure.opcode == LuaOpcode.Closure =>
          prototype.nested.find(_.prototypeId == s"${prototype.prototypeId}.${closure.b}").toVector.flatMap { child =>
            (1 to effectiveUpvalueCount(child))
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

  private def effectiveUpvalueCount(prototype: LuaPrototype): Int = {
    val usedUpvalueCount = prototype.instructions
      .collect {
        case instruction if instruction.opcode == LuaOpcode.GetUpval || instruction.opcode == LuaOpcode.SetUpval =>
          instruction.b + 1
      }
      .maxOption
      .getOrElse(0)
    prototype.upvalueCount.max(usedUpvalueCount)
  }

  private def returnRefs(prototype: LuaPrototype): Vector[String] = {
    val explicitReturns = prototype.instructions
      .filter(_.opcode == LuaOpcode.Return)
      .flatMap { instruction =>
        val slots = instruction.b match {
          case 0 => Vector.empty
          case 1 => Vector.empty
          case n => (instruction.a until (instruction.a + n - 1)).toVector
        }
        slots.map(slot => s"${prototype.prototypeId}@pc${instruction.pc}:r$slot")
      }
    val tailCallReturns = prototype.instructions
      .filter(_.opcode == LuaOpcode.TailCall)
      .map(instruction => s"${prototype.prototypeId}@pc${instruction.pc}:r${instruction.a}")
    explicitReturns ++ tailCallReturns
  }

  private def resolvedCalls(
    prototype: LuaPrototype,
    capturedNames: Map[Int, String] = Map.empty
  ): Vector[ResolvedCall] = {
    val summary = PrototypeSummary(
      prototype.prototypeId,
      prototype.numParams,
      prototype.maxStack,
      prototype.instructions,
      prototype.constants,
      Vector.empty,
      Vector.empty,
      Vector.empty,
      capturedNames
    )
    prototype.instructions
      .filter(instruction => instruction.opcode == LuaOpcode.Call || instruction.opcode == LuaOpcode.TailCall)
      .map(instruction =>
        ResolvedCall(
          callsiteId = s"${prototype.prototypeId}@pc${instruction.pc}",
          prototypeId = prototype.prototypeId,
          pc = instruction.pc,
          resolvedName = resolvedCallName(summary, instruction),
          argumentRefs = callArgumentRefs(prototype, instruction),
          returnRefs = callReturnRefs(instruction).map(ref => s"${prototype.prototypeId}$ref")
        )
      )
  }

  private def resolvedCallName(prototype: PrototypeSummary, call: LuaInstruction): Option[String] =
    callTargetNameAt(prototype, call.pc, call.a)

  private def callTargetNameAt(prototype: PrototypeSummary, pc: Int, slot: Int): Option[String] =
    prototype.instructions
      .filter(instruction => instruction.pc < pc && instruction.a == slot)
      .sortBy(_.pc)
      .lastOption
      .flatMap {
        case instruction if instruction.opcode == LuaOpcode.GetGlobal =>
          constantName(prototype.constants, instruction.b)
        case instruction if instruction.opcode == LuaOpcode.GetUpval =>
          prototype.capturedNames.get(instruction.b)
        case instruction if instruction.opcode == LuaOpcode.GetTable =>
          for {
            baseName <- callTargetNameAt(prototype, instruction.pc, instruction.b)
            field    <- fieldName(prototype.constants, instruction)
          } yield s"$baseName.$field"
        case instruction if instruction.opcode == LuaOpcode.Call && instruction.c.forall(_ != 1) =>
          for {
            targetName <- callTargetNameAt(prototype, instruction.pc, instruction.a)
            if targetName == "require"
            moduleName <- precedingLoadString(prototype, instruction.pc, instruction.a + 1)
          } yield moduleName
        case _ => None
      }

  private def precedingGlobalLoadName(prototype: PrototypeSummary, pc: Int, slot: Int): Option[String] =
    prototype.instructions
      .filter(instruction => instruction.pc < pc && instruction.a == slot && instruction.opcode == LuaOpcode.GetGlobal)
      .lastOption
      .flatMap(instruction => constantName(prototype.constants, instruction.b))

  private def isGlobalName(prototype: PrototypeSummary, slot: Int, name: String): Boolean =
    prototype.instructions.exists(instruction =>
      instruction.opcode == LuaOpcode.GetGlobal &&
        instruction.a == slot &&
        constantName(prototype.constants, instruction.b).contains(name)
    )

  private def precedingLoadString(prototype: PrototypeSummary, pc: Int, slot: Int): Option[String] =
    prototype.instructions
      .filter(instruction => instruction.pc < pc && instruction.a == slot && instruction.opcode == LuaOpcode.LoadK)
      .lastOption
      .flatMap(instruction => constantName(prototype.constants, instruction.b))

  private def precedingLoadString(prototype: LuaPrototype, pc: Int, slot: Int): Option[String] =
    prototype.instructions
      .filter(instruction => instruction.pc < pc && instruction.a == slot && instruction.opcode == LuaOpcode.LoadK)
      .lastOption
      .flatMap(instruction => constantName(prototype.constants, instruction.b))

  private def precedingStringConstant(prototype: PrototypeSummary, pc: Int, slot: Int): Option[String] =
    precedingLoadString(prototype, pc, slot)

  private def closureInSlotBefore(prototype: LuaPrototype, pc: Int, slot: Int): Option[String] =
    prototype.instructions
      .filter(instruction => instruction.pc < pc && instruction.a == slot && instruction.opcode == LuaOpcode.Closure)
      .lastOption
      .map(instruction => s"${prototype.prototypeId}.${instruction.b}")

  private def isReturnedTable(root: LuaPrototype, slot: Int): Boolean =
    root.instructions.exists(instruction =>
      instruction.opcode == LuaOpcode.Return && instruction.a == slot && instruction.b == 2
    )

  private def precedingGetTable(prototype: PrototypeSummary, pc: Int, slot: Int): Option[LuaInstruction] =
    prototype.instructions
      .filter(instruction => instruction.pc < pc && overwritesSlot(instruction, slot))
      .sortBy(_.pc)
      .lastOption
      .filter(_.opcode == LuaOpcode.GetTable)

  private def fieldName(constants: Vector[LuaConstant], instruction: LuaInstruction): Option[String] =
    instruction.c.filter(_ >= RkConstantBase).flatMap(value => constantName(constants, value - RkConstantBase))

  private def callArgumentRefs(prototype: LuaPrototype, instruction: LuaInstruction): Vector[String] =
    callArgumentRefs(prototype.prototypeId, prototype.maxStack, instruction)

  private def callArgumentRefs(prototype: PrototypeSummary, instruction: LuaInstruction): Vector[String] =
    callArgumentRefs(prototype.prototypeId, prototype.maxStack, instruction)

  private def callArgumentRefs(prototypeId: String, maxStack: Int, instruction: LuaInstruction): Vector[String] = {
    val slots = instruction.b match {
      case 0 => ((instruction.a + 1) until maxStack).toVector
      case 1 => Vector.empty
      case n => ((instruction.a + 1) until (instruction.a + n)).toVector
    }
    slots.map(slot => s"$prototypeId@pc${instruction.pc}:r$slot")
  }

  private def callReturnRefs(instruction: LuaInstruction): Vector[String] = {
    val slots = instruction.c match {
      case Some(0) => Vector(instruction.a)
      case Some(1) => Vector.empty
      case Some(n) => (instruction.a until (instruction.a + n - 1)).toVector
      case None    => Vector.empty
    }
    slots.map(slot => s"@pc${instruction.pc}:r$slot")
  }

  private def isConcreteStringArgument(prototype: PrototypeSummary, callPc: Int, argumentRef: String): Boolean =
    registerWrite(argumentRef).exists(write =>
      prototype.instructions
        .filter(instruction => instruction.pc < callPc && instruction.a == write.slot)
        .sortBy(_.pc)
        .lastOption
        .exists(instruction =>
          instruction.opcode == LuaOpcode.LoadK &&
            constantName(prototype.constants, instruction.b).nonEmpty
        )
    )

  private def triggerMatches(pattern: String, name: String): Boolean =
    pattern.split('.').lastOption.contains(finalSegment(name))

  private def finalSegment(name: String): String =
    name.split('.').lastOption match {
      case Some(segment) if segment.nonEmpty => segment
      case _                                 => name
    }

  private def sameModuleRef(left: String, right: String): Boolean =
    left.split(':').headOption.nonEmpty && left.split(':').headOption == right.split(':').headOption

  private final case class QualifiedValueRef(modulePath: String, prototypeId: String, pc: Int, localRef: String)

  private def parseQualifiedValueRef(ref: String): QualifiedValueRef = {
    val splitAt = Vector(".luac:")
      .flatMap { marker =>
        val index = ref.indexOf(marker)
        if (index >= 0) Some(index + marker.length - 1) else None
      }
      .headOption
      .getOrElse(throw new IllegalArgumentException(s"Lua qualified ref is missing module path: $ref"))
    val modulePath = ref.substring(0, splitAt)
    val localRef   = ref.substring(splitAt + 1)
    val prototypeId = localRef
      .split("@pc", 2)
      .headOption
      .getOrElse(throw new IllegalArgumentException(s"Lua value ref is missing prototype id: $ref"))
    val pc = localRef
      .split("@pc", 2)
      .lift(1)
      .flatMap(_.split(":r", 2).headOption)
      .flatMap(_.toIntOption)
      .getOrElse(throw new IllegalArgumentException(s"Lua value ref is missing pc: $ref"))
    QualifiedValueRef(modulePath, prototypeId, pc, localRef)
  }

  private def slotFromValueRef(ref: String): Int =
    localRefFromQualifiedRef(ref)
      .split(":r", 2)
      .lift(1)
      .flatMap(_.toIntOption)
      .getOrElse(throw new IllegalArgumentException(s"Lua value ref is missing slot: $ref"))

  private def modulePathFromQualifiedRef(ref: String): String = {
    val splitAt = Vector(".luac:")
      .flatMap { marker =>
        val index = ref.indexOf(marker)
        if (index >= 0) Some(index + marker.length - 1) else None
      }
      .headOption
      .getOrElse(throw new IllegalArgumentException(s"Lua qualified ref is missing module path: $ref"))
    ref.substring(0, splitAt)
  }

  private def callsiteIdFromValueRef(ref: String): String =
    ref.split(":r", 2).headOption.getOrElse(ref)

  private def callsitePc(callsiteId: String): Option[Int] =
    callsiteId.indexOf("@pc") match {
      case -1    => None
      case index => callsiteId.substring(index + 3).toIntOption
    }

  private def requiredCallsitePc(callsiteId: String): Int =
    callsitePc(callsiteId).getOrElse(throw new IllegalStateException(s"missing callsite pc: callsite_id=$callsiteId"))

  private def prototypeIdFromCallsiteId(ref: String): String =
    ref.split("@pc", 2).headOption.getOrElse(ref)

  private def localRefFromQualifiedRef(ref: String): String = {
    val splitAt = Vector(".luac:")
      .flatMap { marker =>
        val index = ref.indexOf(marker)
        if (index >= 0) Some(index + marker.length - 1) else None
      }
      .headOption
      .getOrElse(throw new IllegalArgumentException(s"Lua qualified ref is missing module path: $ref"))
    ref.substring(splitAt + 1)
  }

  private def constantName(constants: Vector[LuaConstant], index: Int): Option[String] =
    constants.collectFirst { case LuaConstant(`index`, "string", LuaConstantValue.StringValue(value)) =>
      value
    }

  private def modulePathForRequire(requireString: String): String = {
    val parts = requireString.split('.').filter(_.nonEmpty)
    if (parts.isEmpty) throw new IllegalArgumentException("empty Lua require string")
    s"${parts.mkString("/")}.luac"
  }

  private def moduleNamesForPath(modulePath: String): Set[String] = {
    val path           = modulePath.stripSuffix(".luac").stripSuffix(".lua")
    val parts          = path.split('/').filter(_.nonEmpty).toVector
    val allSuffixNames = parts.indices.map(index => parts.drop(index).mkString(".")).filter(_.nonEmpty).toSet
    val bareName       = parts.lastOption.toSet
    allSuffixNames ++ bareName
  }

  private def modulePathDeclaresRequire(modulePath: String, requireString: String): Boolean =
    moduleNamesForPath(modulePath).contains(requireString)

  private def unresolvedReason(resolution: LuaModuleResolution): String =
    resolution.unresolvedReason match {
      case Some(reason) => reason
      case None =>
        throw new IllegalStateException(
          s"non-matched Lua module resolution without unresolved reason at ${resolution.fromModulePath}:${resolution.requireCallsiteId}"
        )
    }
}
