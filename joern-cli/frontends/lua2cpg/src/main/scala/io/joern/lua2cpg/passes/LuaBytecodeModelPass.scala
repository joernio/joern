package io.joern.lua2cpg.passes

import io.joern.lua2cpg.Config
import io.joern.lua2cpg.bytecode.*
import io.joern.x2cpg.{Ast, Defines, SourceFiles, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{Cpg, DispatchTypes, EdgeTypes, EvaluationStrategies, NodeTypes}
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.utils.FileUtil.*

import java.nio.file.{Files, Paths}
import scala.collection.mutable

class LuaBytecodeModelPass(
  cpg: Cpg,
  config: Config,
  decodedInputs: Option[Vector[LuaBytecodeModelPass.DecodedBytecode]] = None
) extends CpgPass(cpg) {

  private given ValidationMode = ValidationMode.Disabled

  private final case class PrototypeAst(ast: Ast, reachingDefEdges: Vector[(NewNode, NewNode, String)])
  override def run(diffGraph: DiffGraphBuilder): Unit = {
    val decoded          = decodedInputs.getOrElse(LuaBytecodeModelPass.decodeInputs(config))
    val programSemantics = LuaProgramSemantics.normalize(decoded.map(item => item.relativeName -> item.result))

    decoded.foreach { decodedItem =>
      val relativeName = decodedItem.relativeName
      val result       = decodedItem.result

      diffGraph.addNode(NewFile().name(relativeName).order(decodedItem.order))
      addArtifact(result, relativeName, diffGraph)
      if (canEmitSuccessFacts(result)) {
        result.root.foreach(root => addPrototypeTree(root, relativeName, programSemantics, diffGraph))
      }
      addDiagnostic(result, relativeName, diffGraph)
      if (isStrippedMetadata(result)) {
        addMetadataUnavailableDiagnostic(relativeName, diffGraph)
      }
    }
  }

  private def canEmitSuccessFacts(result: LuaBytecodeDecodeResult): Boolean =
    result.artifact.accepted && result.artifact.diagnostic.successFactsAllowed

  private def addArtifact(result: LuaBytecodeDecodeResult, relativeName: String, diffGraph: DiffGraphBuilder): Unit = {
    diffGraph.addNode(
      NewTypeDecl()
        .name("lua-bytecode-artifact")
        .fullName(s"lua:$relativeName")
        .code(artifactCode(result))
        .filename(relativeName)
        .isExternal(false)
        .astParentType(NodeTypes.FILE)
        .astParentFullName(relativeName)
    )
  }

  private def addPrototypeTree(
    prototype: LuaPrototype,
    relativeName: String,
    programSemantics: LuaProgramSemantics,
    diffGraph: DiffGraphBuilder
  ): Unit = {
    val prototypeTypeDecl = NewTypeDecl()
      .name("lua-bytecode-prototype")
      .fullName(prototypeTypeFullName(relativeName, prototype))
      .code(prototypeCode(prototype))
      .filename(relativeName)
      .isExternal(false)
      .astParentType(NodeTypes.FILE)
      .astParentFullName(relativeName)

    diffGraph.addNode(prototypeTypeDecl)
    val methodAst = prototypeMethodAst(prototype, relativeName, programSemantics)
    Ast.storeInDiffGraph(methodAst.ast, diffGraph)
    methodAst.reachingDefEdges.foreach { case (source, sink, variable) =>
      diffGraph.addEdge(source, sink, EdgeTypes.REACHING_DEF, variable)
    }
    prototype.nested.foreach(child => addPrototypeTree(child, relativeName, programSemantics, diffGraph))
  }

  private def prototypeMethodAst(
    prototype: LuaPrototype,
    relativeName: String,
    programSemantics: LuaProgramSemantics
  ): PrototypeAst = {
    val fullName  = prototypeMethodFullName(relativeName, prototype)
    val semantics = LuaInstructionSemantics.normalizePrototype(prototype)
    val method = NewMethod()
      .name(prototype.prototypeId)
      .code(prototypeCode(prototype))
      .fullName(fullName)
      .filename(relativeName)
      .signature(prototypeSignature(prototype))
      .isExternal(false)
      .astParentType(NodeTypes.TYPE_DECL)
      .astParentFullName(prototypeTypeFullName(relativeName, prototype))

    val parameters = (0 until prototype.numParams).map { index =>
      Ast(
        NewMethodParameterIn()
          .name(s"r$index")
          .code(s"${prototype.prototypeId}:r$index")
          .index(index + 1)
          .order(index + 1)
          .isVariadic(false)
          .evaluationStrategy(EvaluationStrategies.BY_VALUE)
          .typeFullName(Defines.Any)
      )
    }
    val semanticNodes = semanticValueNodes(prototype, semantics) ++
      semanticCallNodes(prototype, semantics) ++
      programSemanticCallNodes(prototype, relativeName, programSemantics)
    val block = Ast(NewBlock().code(prototype.prototypeId).typeFullName(Defines.Any))
      .withChildren(prototype.constants.map(constant => Ast(literalNode(prototype, constant))))
      .withChildren(prototype.instructions.map(instruction => Ast(instructionNode(prototype, instruction))))
      .withChildren(semanticNodes.map(Ast(_)))
    val methodReturn = Ast(
      NewMethodReturn()
        .code("RET")
        .order(prototype.numParams + 2)
        .evaluationStrategy(EvaluationStrategies.BY_VALUE)
        .typeFullName(Defines.Any)
    )

    val ast = Ast(method)
      .withChildren(parameters)
      .withChild(block)
      .withChild(methodReturn)
    PrototypeAst(ast, reachingDefEdges(ast, semantics))
  }

  private def literalNode(prototype: LuaPrototype, constant: LuaConstant): NewLiteral = {
    val text = constantValueText(constant.value)
    NewLiteral()
      .code(text)
      .typeFullName(s"lua.${constant.luaType}")
      .order(constant.index + 1)
      .argumentIndex(constant.index + 1)
      .lineNumber((constant.index + 1).toInt)
      .columnNumber(0)
  }

  private def instructionNode(prototype: LuaPrototype, instruction: LuaInstruction): NewCall =
    NewCall()
      .name(s"lua.bytecode.${instruction.opcode.mnemonic}")
      .code(s"${prototype.prototypeId}@pc${instruction.pc}:${instruction.opcode.mnemonic}")
      .methodFullName(s"lua.bytecode.${instruction.opcode.mnemonic}")
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .typeFullName(Defines.Any)
      .order(instruction.pc + 1)
      .argumentIndex(instruction.pc + 1)
      .lineNumber(instruction.pc + 1)
      .columnNumber(0)

  private def semanticValueNodes(prototype: LuaPrototype, semantics: LuaPrototypeSemantics): Vector[NewIdentifier] =
    semantics.registerEvents
      .filter(_.prototypeId == prototype.prototypeId)
      .map(_.valueRef)
      .distinct
      .sorted
      .zipWithIndex
      .map { case (ref, index) =>
        NewIdentifier()
          .name(ref)
          .code(ref)
          .typeFullName(Defines.Any)
          .order(10_000 + index)
          .argumentIndex(10_000 + index)
          .lineNumber(10_000 + index)
          .columnNumber(0)
      }

  private def semanticCallNodes(prototype: LuaPrototype, semantics: LuaPrototypeSemantics): Vector[NewCall] = {
    val candidateNodes = semantics.callTargetCandidates
      .filter(_.callsiteId.startsWith(s"${prototype.prototypeId}@pc"))
      .sortBy(candidate => (candidate.callsiteId, candidate.targetRef))
      .zipWithIndex
      .map { case (candidate, index) =>
        semanticCallNode(
          name = "lua.calltarget.candidate",
          code = s"${candidate.callsiteId} -> ${candidate.targetRef}",
          order = 20_000 + index
        )
      }
    val unresolvedNodes = semantics.unresolvedCalls
      .filter(_.callsiteId.startsWith(s"${prototype.prototypeId}@pc"))
      .sortBy(_.callsiteId)
      .zipWithIndex
      .map { case (call, index) =>
        semanticCallNode(
          name = "lua.calltarget.unresolved",
          code = s"${call.callsiteId} unresolved=${call.unresolvedReason}",
          order = 30_000 + index
        )
      }
    val boundaryNodes = semantics.negativeExpectations
      .filter(row =>
        row.sourceRef.startsWith(s"${prototype.prototypeId}@") || row.sourceRef.startsWith(s"${prototype.prototypeId}.")
      )
      .sortBy(_.negativeId)
      .zipWithIndex
      .map { case (row, index) =>
        semanticCallNode(name = "lua.semantic.boundary", code = row.negativeId, order = 40_000 + index)
      }
    candidateNodes ++ unresolvedNodes ++ boundaryNodes
  }

  private def programSemanticCallNodes(
    prototype: LuaPrototype,
    relativeName: String,
    semantics: LuaProgramSemantics
  ): Vector[NewCall] = {
    val prefix     = s"$relativeName:${prototype.prototypeId}@pc"
    val rootMethod = prototype.prototypeId == "root"
    val rootMarkers = if (rootMethod) {
      val moduleResolutions = semantics.moduleResolutions
        .filter(_.fromModulePath == relativeName)
        .sortBy(_.requireCallsiteId)
        .zipWithIndex
        .map { case (resolution, index) =>
          val target = resolution match {
            case LuaModuleResolution(_, _, "matched", _, Some(targetModulePath), None, _) => targetModulePath
            case LuaModuleResolution(_, _, _, _, None, Some(unresolvedReason), _)         => unresolvedReason
            case _ =>
              throw new IllegalStateException(
                s"inconsistent Lua module resolution state at ${resolution.fromModulePath}:${resolution.requireCallsiteId}"
              )
          }
          semanticCallNode(
            name = "lua.module.resolution",
            code =
              s"${resolution.fromModulePath} require ${resolution.requireString} -> ${resolution.resolutionStatus}:$target",
            order = 50_000 + index
          )
        }
      val returnTables = semantics.moduleReturnTables
        .filter(_.modulePath == relativeName)
        .sortBy(row => (row.fieldName, row.targetPrototypeId))
        .zipWithIndex
        .map { case (row, index) =>
          semanticCallNode(
            name = "lua.module.return_table",
            code = s"${row.modulePath}::${row.fieldName} -> ${row.targetPrototypeId}",
            order = 51_000 + index
          )
        }
      val boundaries = semantics.boundaries
        .filter(_.boundaryId.startsWith(relativeName))
        .sortBy(_.boundaryId)
        .zipWithIndex
        .map { case (boundary, index) =>
          semanticCallNode(
            name = "lua.e4.boundary",
            code = s"${boundary.boundaryId} reason=${boundary.reason}",
            order = 52_000 + index
          )
        }
      val e5Boundaries = semantics.e5Boundaries
        .filter(_.boundaryId.startsWith(relativeName))
        .sortBy(_.boundaryId)
        .zipWithIndex
        .map { case (boundary, index) =>
          semanticCallNode(
            name = "lua.e5.boundary",
            code = s"${boundary.boundaryId} reason=${boundary.reason}",
            order = 52_500 + index
          )
        }
      moduleResolutions ++ returnTables ++ boundaries ++ e5Boundaries
    } else Vector.empty

    val fieldCalls = semantics.moduleFieldCallTargets
      .filter(row => s"${row.fromModulePath}:${row.callsiteId}".startsWith(prefix))
      .sortBy(_.callsiteId)
      .zipWithIndex
      .map { case (row, index) =>
        semanticCallNode(
          name = "lua.module.field_call_target",
          code = s"${row.fromModulePath}:${row.callsiteId} -> ${row.targetModulePath}::${row.targetPrototypeId}",
          order = 53_000 + index
        )
      }
    val argFlows = semantics.interproceduralArgFlows
      .filter(row =>
        row.callsiteId.startsWith(s"${prototype.prototypeId}@pc") && row.fromArgumentRef.startsWith(relativeName)
      )
      .sortBy(row => (row.callsiteId, row.fromArgumentRef))
      .zipWithIndex
      .map { case (row, index) =>
        semanticCallNode(
          name = "lua.interproc.arg_flow",
          code = s"${row.fromArgumentRef} -> ${row.toParameterRef}",
          order = 54_000 + index
        )
      }
    val returnFlows = semantics.interproceduralReturnFlows
      .filter(row =>
        row.callsiteId.startsWith(s"${prototype.prototypeId}@pc") && row.callerResultRef.startsWith(relativeName)
      )
      .sortBy(row => (row.callsiteId, row.callerResultRef))
      .zipWithIndex
      .map { case (row, index) =>
        semanticCallNode(
          name = "lua.interproc.return_flow",
          code = s"${row.targetModulePath}::${row.calleeReturnRef} -> ${row.callerResultRef}",
          order = 55_000 + index
        )
      }
    val crossTargets = semantics.crossBoundaryCallTargets
      .filter(row =>
        row.fromModulePath == relativeName && s"${row.fromModulePath}:${row.callsiteId}".startsWith(prefix)
      )
      .sortBy(row => (row.callsiteId, row.targetModulePath, row.targetPrototypeId))
      .zipWithIndex
      .map { case (row, index) =>
        semanticCallNode(
          name = "lua.calltarget.cross_boundary",
          code = s"$relativeName:${row.callsiteId} -> ${row.targetModulePath}::${row.targetPrototypeId}",
          order = 56_000 + index
        )
      }
    val taintPaths = semantics.taintPaths
      .filter(row => row.sourceRef.startsWith(s"$relativeName:${prototype.prototypeId}@"))
      .sortBy(row => (row.sourceRef, row.sinkRef))
      .zipWithIndex
      .map { case (row, index) =>
        semanticCallNode(
          name = "lua.taint.path",
          code = s"${row.sourceRef} -> ${row.sinkRef} via ${row.pathSteps.mkString(";")}",
          order = 57_000 + index
        )
      }
    val ruleMatches = semantics.ruleMatches
      .filter(row => row.callsiteId.startsWith(prefix))
      .sortBy(row => (row.ruleKind, row.callsiteId, row.matchedName))
      .zipWithIndex
      .map { case (row, index) =>
        semanticCallNode(
          name = "lua.rule.match",
          code = s"${row.callsiteId} ${row.trigger} -> ${row.matchedName}",
          order = 58_000 + index
        )
      }
    val sourceEndpoints = semantics.sourceEndpoints
      .filter(row => row.sourceRef.startsWith(s"$relativeName:${prototype.prototypeId}@"))
      .sortBy(row => (row.sourceRef, row.trigger))
      .zipWithIndex
      .map { case (row, index) =>
        semanticCallNode(
          name = "lua.source.endpoint",
          code = s"${row.sourceRef} via ${row.trigger}",
          order = 59_000 + index
        )
      }
    val sinkEndpoints = semantics.sinkEndpoints
      .filter(row => row.sinkRef.startsWith(s"$relativeName:${prototype.prototypeId}@"))
      .sortBy(row => (row.sinkRef, row.trigger))
      .zipWithIndex
      .map { case (row, index) =>
        semanticCallNode(
          name = "lua.sink.endpoint",
          code = s"${row.sinkRef} via ${row.trigger} param=${row.parameterIndex}",
          order = 60_000 + index
        )
      }
    val sanitizerCalls = semantics.sanitizerCalls
      .filter(row => row.sanitizedValueRef.startsWith(s"$relativeName:${prototype.prototypeId}@"))
      .sortBy(row => (row.callsiteId, row.sanitizerName))
      .zipWithIndex
      .map { case (row, index) =>
        semanticCallNode(
          name = "lua.sanitizer.call",
          code = s"${row.callsiteId} ${row.sanitizerName} -> ${row.sanitizedValueRef}",
          order = 61_000 + index
        )
      }
    val sanitizerClassifications = semantics.sanitizerClassifications
      .filter(row => row.sourceRef.startsWith(s"$relativeName:${prototype.prototypeId}@"))
      .sortBy(row => (row.sourceRef, row.sinkRef, row.sanitizerCallsiteId))
      .zipWithIndex
      .map { case (row, index) =>
        semanticCallNode(
          name = "lua.sanitizer.classification",
          code =
            s"${row.sourceRef} -> ${row.sinkRef} classification=${row.classification} sanitizer=${row.sanitizerName}",
          order = 62_000 + index
        )
      }
    val reportClassifications = semantics.reportClassifications
      .filter(row => row.sourceRef.startsWith(s"$relativeName:${prototype.prototypeId}@"))
      .sortBy(row => (row.sourceRef, row.sinkRef))
      .zipWithIndex
      .map { case (row, index) =>
        semanticCallNode(
          name = "lua.report.classification",
          code = s"${row.sourceRef} -> ${row.sinkRef} classification=${row.classification} reason=${row.reason}",
          order = 63_000 + index
        )
      }
    val vulnerabilityReports = semantics.vulnerabilityReports
      .filter(row => row.sourceRef.startsWith(s"$relativeName:${prototype.prototypeId}@"))
      .sortBy(row => (row.sourceRef, row.sinkRef))
      .zipWithIndex
      .map { case (row, index) =>
        semanticCallNode(
          name = "lua.report.vulnerability",
          code =
            s"${row.sourceRef} -> ${row.sinkRef} status=${row.pathStatus} classification=${row.classification} path=${row.pathSteps
                .mkString(";")}",
          order = 64_000 + index
        )
      }

    rootMarkers ++ fieldCalls ++ argFlows ++ returnFlows ++ crossTargets ++ taintPaths ++ ruleMatches ++
      sourceEndpoints ++ sinkEndpoints ++ sanitizerCalls ++ sanitizerClassifications ++ reportClassifications ++
      vulnerabilityReports
  }

  private def semanticCallNode(name: String, code: String, order: Int): NewCall =
    NewCall()
      .name(name)
      .code(code)
      .methodFullName(name)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .typeFullName(Defines.Any)
      .order(order)
      .argumentIndex(order)
      .lineNumber(order)
      .columnNumber(0)

  private def reachingDefEdges(ast: Ast, semantics: LuaPrototypeSemantics): Vector[(NewNode, NewNode, String)] = {
    val nodesByCode = ast.nodes.collect {
      case node: NewIdentifier        => node.code -> node
      case node: NewMethodParameterIn => node.code -> node
    }.toMap
    val edges = mutable.LinkedHashSet.empty[(NewNode, NewNode, String)]
    semantics.localFlows.foreach { flow =>
      addEdge(nodesByCode, edges, flow.sourceRef, flow.sinkRef, flow.sourceRef)
    }
    semantics.tableFieldFlows.foreach { flow =>
      addEdge(nodesByCode, edges, flow.writeRef, flow.readRef, s"table:${flow.tableRef}:${flow.keyRef}")
    }
    semantics.globalFlows.foreach { flow =>
      addEdge(nodesByCode, edges, flow.valueRef, flow.readRef, s"global:${flow.globalName}")
    }
    semantics.upvalueFlows.foreach { flow =>
      addEdge(nodesByCode, edges, flow.captureRef, flow.writeRef, s"upvalue:${flow.upvalueId}")
    }
    edges.toVector
  }

  private def addEdge(
    nodesByCode: Map[String, NewNode],
    edges: mutable.LinkedHashSet[(NewNode, NewNode, String)],
    sourceRef: String,
    sinkRef: String,
    variable: String
  ): Unit =
    for {
      source <- nodesByCode.get(sourceRef)
      sink   <- nodesByCode.get(sinkRef)
    } edges += ((source, sink, variable))

  private def addDiagnostic(
    result: LuaBytecodeDecodeResult,
    relativeName: String,
    diffGraph: DiffGraphBuilder
  ): Unit = {
    val diagnostic = result.artifact.diagnostic
    diffGraph.addNode(
      NewTypeDecl()
        .name("lua-bytecode-diagnostic")
        .fullName(s"lua:$relativeName:diagnostic:${diagnostic.kind}")
        .code(diagnosticCode(result, diagnostic))
        .filename(relativeName)
        .isExternal(false)
        .astParentType(NodeTypes.FILE)
        .astParentFullName(relativeName)
    )
  }

  private def addMetadataUnavailableDiagnostic(relativeName: String, diffGraph: DiffGraphBuilder): Unit = {
    diffGraph.addNode(
      NewTypeDecl()
        .name("lua-bytecode-diagnostic")
        .fullName(s"lua:$relativeName:diagnostic:metadata-unavailable")
        .code("kind=metadata-unavailable severity=info success_facts_allowed=true")
        .filename(relativeName)
        .isExternal(false)
        .astParentType(NodeTypes.FILE)
        .astParentFullName(relativeName)
    )
  }

  private def isStrippedMetadata(result: LuaBytecodeDecodeResult): Boolean = {
    result.artifact.accepted && result.root.exists { root =>
      allPrototypes(root).exists(prototype =>
        prototype.sourceName.isEmpty &&
          prototype.lineNumbers.isEmpty &&
          prototype.locals.isEmpty &&
          prototype.upvalueNames.isEmpty
      )
    }
  }

  private def allPrototypes(prototype: LuaPrototype): Vector[LuaPrototype] =
    prototype +: prototype.nested.flatMap(allPrototypes)

  private def artifactCode(result: LuaBytecodeDecodeResult): String = {
    val artifact = result.artifact
    Seq(
      s"input_kind=${artifact.inputKind}",
      s"accepted=${artifact.accepted}",
      s"diagnostic=${artifact.diagnostic.kind}",
      artifact.profileId.map(profile => s"profile=$profile").getOrElse("profile=unavailable")
    ).mkString(" ")
  }

  private def prototypeCode(prototype: LuaPrototype): String = {
    Seq(
      s"prototype=${prototype.prototypeId}",
      s"parent=${prototype.parentPrototypeId.getOrElse("none")}",
      s"params=${prototype.numParams}",
      s"vararg=${prototype.isVararg}",
      s"max_stack=${prototype.maxStack}",
      s"upvalues=${prototype.upvalueCount}"
    ).mkString(" ")
  }

  private def prototypeSignature(prototype: LuaPrototype): String =
    s"(${Vector.fill(prototype.numParams)(Defines.Any).mkString(",")}):${Defines.Any}"

  private def diagnosticCode(result: LuaBytecodeDecodeResult, diagnostic: LuaDiagnostic): String =
    Seq(
      s"kind=${diagnostic.kind}",
      s"severity=${diagnostic.severity}",
      s"success_facts_allowed=${diagnostic.successFactsAllowed}",
      s"accepted=${result.artifact.accepted}",
      s"message=${diagnostic.message}"
    ).mkString(" ")

  private def constantValueText(value: LuaConstantValue): String = value match {
    case LuaConstantValue.NilValue            => "nil"
    case LuaConstantValue.BooleanValue(value) => value.toString
    case LuaConstantValue.NumberValue(value)  => numericText(value)
    case LuaConstantValue.StringValue(value)  => value
  }

  private def numericText(value: Double): String =
    if (value.isWhole) value.toLong.toString else value.toString

  private def prototypeTypeFullName(relativeName: String, prototype: LuaPrototype): String =
    s"${prototypeMethodFullName(relativeName, prototype)}:prototype"

  private def prototypeMethodFullName(relativeName: String, prototype: LuaPrototype): String =
    s"lua:$relativeName:${prototype.prototypeId}"
}

object LuaBytecodeModelPass {
  final case class DecodedBytecode(relativeName: String, result: LuaBytecodeDecodeResult, order: Int)

  def decodeInputs(config: Config): Vector[DecodedBytecode] = {
    val inputRoot = Paths.get(config.inputPath).absolutePathAsString
    val bytecodeFiles = SourceFiles.determine(
      inputPath = inputRoot,
      sourceFileExtensions = Set(".luac"),
      ignoredDefaultRegex = Some(config.defaultIgnoredFilesRegex),
      ignoredFilesRegex = Some(config.ignoredFilesRegex),
      ignoredFilesPath = Some(config.ignoredFiles)
    )()

    bytecodeFiles.zipWithIndex.map { case (file, index) =>
      val relativeName = SourceFiles.toRelativePath(file, inputRoot)
      val bytes        = Files.readAllBytes(Paths.get(file))
      val result       = LuaBytecodeDecoder.decode(relativeName, bytes)
      DecodedBytecode(relativeName, result, index + 1)
    }.toVector
  }

}
