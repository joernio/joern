package io.joern.dataflowengineoss.slicing

import better.files.File
import better.files.File.OpenOptions
import io.joern.x2cpg.passes.frontend.XTypeRecovery
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.PropertyNames
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.regex.Pattern
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Success, Using}

/** Makes use of statistical techniques to infer the type of a target object based on a CPG slice.
  */
class SliceBasedTypeInferencePass(
  cpg: Cpg,
  joernti: Option[JoernTI] = None,
  pathSep: String = ":",
  minConfidence: Float = 0.8f,
  typesNotToInfer: Set[String] = Set("object", "unk", "void"),
  logValues: Boolean = true
) extends CpgPass(cpg) {

  private lazy val logger         = LoggerFactory.getLogger(classOf[SliceBasedTypeInferencePass])
  private lazy val sliceConfig    = SliceConfig(sliceMode = SliceMode.Usages)
  private val changes             = ArrayBuffer.empty[InferredChange]
  private val ecmaPrefix          = "__ecma."
  private lazy val timeOfAnalysis = DateTimeFormatter.ofPattern("yyyy-MM-dd__HH_mm").format(LocalDateTime.now)

  private lazy val typeInferenceFile = generateLoggingFile("type_inference")
  private lazy val invalidInferences = generateLoggingFile("type_violations")
  private lazy val labellingFile     = generateLoggingFile("labelling_file")
  private lazy val yieldFile         = generateLoggingFile("inference_yield")

  private def generateLoggingFile(name: String): File = cpg.metaData.root.map(File(_)).headOption match {
    case Some(project) => File(s"./${project.name}_${name}_$timeOfAnalysis.csv")
    case None          => File(s"./${name}_$timeOfAnalysis.csv")
  }

  private case class InferredChange(
    location: String,
    nodeLabel: String,
    target: String,
    oldType: String,
    newType: String
  )

  private def location(n: StoredNode): String = {
    val lineNumber = n match {
      case x: Local => x.referencingIdentifiers.sortBy(_.lineNumber).headOption.flatMap(_.lineNumber).getOrElse(-1)
      case _        => n.property(PropertyNames.LINE_NUMBER, -1)
    }
    s"${n.file.name.headOption.getOrElse("Unknown")}#L$lineNumber"
  }

  lazy private val pathPattern = Pattern.compile("[\"']([\\w/.]+)[\"']")

  override def run(builder: DiffGraphBuilder): Unit = {
    joernti.foreach { ti =>
      Using.resource(ti) { tidal =>
        val slice: ProgramUsageSlice =
          UsageSlicing.calculateUsageSlice(cpg, sliceConfig).asInstanceOf[ProgramUsageSlice]
        tidal.infer(slice) match {
          case Failure(exception) =>
            logger.warn("Unable to enrich compilation unit type information with joernti, continuing...", exception)
          case Success(inferenceResults) =>
            val filteredResults = inferenceResults
              .filter(_.confidence > minConfidence)
              .filterNot(res => pathPattern.matcher(res.typ).matches())
              .filterNot(res => typesNotToInfer.contains(res.typ.toLowerCase))
              .map(builtinTypes)
            // Below tracks which inference results violating an existing type constraint
            val violatingInferenceResults = mutable.HashMap.empty[InferenceResult, Boolean]
            filteredResults
              .groupBy(_.scope)
              .foreach { case (scope, results) =>
                val method = cpg.method
                  .fullNameExact(scope)
                  .l
                lazy val methodDeclarations = method.ast.collect {
                  case n: Local             => n
                  case n: MethodParameterIn => n
                }.l
                lazy val methodIdentifiers = method.ast.isIdentifier.l
                val objectSlices           = slice.objectSlices.get(scope)
                // Set the type of targets which only have dummy types or have no types
                results
                  .filterNot { res =>
                    // Check the type constraints on these results
                    val associatedSlice = objectSlices.flatMap(_.slices.find(_.targetObj.name == res.targetIdentifier))
                    violatesExistingTypeConstraint(res, violatingInferenceResults, associatedSlice)
                  }
                  .foreach { res =>
                    // Handle locals
                    methodDeclarations
                      .nameExact(res.targetIdentifier)
                      .filter(onlyDummyOrAnyType)
                      .foreach { tgt =>
                        builder.setNodeProperty(tgt, PropertyNames.TYPE_FULL_NAME, res.typ)
                        if (logValues)
                          logChange(
                            location(tgt),
                            tgt.label,
                            res.targetIdentifier,
                            tgt.property(PropertyNames.TYPE_FULL_NAME, "<unknown>"),
                            res.typ
                          )
                      }
                    // Handle identifiers
                    methodIdentifiers
                      .nameExact(res.targetIdentifier)
                      .filter(onlyDummyOrAnyType) // We only want to write to nodes that need type info
                      .foreach { tgt =>
                        builder.setNodeProperty(tgt, PropertyNames.TYPE_FULL_NAME, res.typ)
                        logChange(location(tgt), tgt.label, res.targetIdentifier, tgt.typeFullName, res.typ)
                        // If there is a call where the identifier is the receiver then this also needs to be updated
                        if (tgt.argumentIndex <= 1 && tgt.astSiblings.isCall.exists(_.argumentIndex >= 2)) {
                          tgt.astSiblings.isCall.nameNot("<operator.*").find(onlyDummyOrAnyType).foreach { call =>
                            val inferredMethodCall = Seq(res.typ, call.name).mkString(pathSep)
                            builder.setNodeProperty(call, PropertyNames.METHOD_FULL_NAME, inferredMethodCall)
                            if (logValues)
                              logChange(
                                location(call),
                                call.label,
                                res.targetIdentifier,
                                call.methodFullName,
                                inferredMethodCall
                              )
                          }
                        }
                      }
                    res.targetIdentifier
                  }
                // Get yield
                if (logValues) {
                  val targetNodes = cpg.graph
                    .nodes("LOCAL", "IDENTIFIER", "METHOD_PARAMETER_IN")
                    .collect { case n: AstNode => n }
                    .l
                  val inferredTypes = changes.count(_.nodeLabel != "CALL")

                  val untypedNodes = targetNodes.count(onlyDummyOrAnyType) - inferredTypes
                  val typedNodes   = targetNodes.size - untypedNodes
                  yieldFile.write("LABEL,COUNT\n")
                  yieldFile.write(s"NUM_NODES,${targetNodes.size}\n")(OpenOptions.append)
                  yieldFile.write(s"UNTYPED,$untypedNodes\n")(OpenOptions.append)
                  yieldFile.write(s"TYPED,$typedNodes\n")(OpenOptions.append)
                  yieldFile.write(s"INFERRED,$inferredTypes\n")(OpenOptions.append)
                  yieldFile.write(s"TYPE_VIOLATIONS,${violatingInferenceResults.count(_._2)}\n")(OpenOptions.append)

                  invalidInferences.write("Scope,Target,InferredType\n")
                  violatingInferenceResults.sortBy(_._1.scope).filter(_._2).foreach { case (res, _) =>
                    invalidInferences.write(s"${res.scope},${res.targetIdentifier},${res.typ}\n")(OpenOptions.append)
                  }
                }
              }
        }
      }
      if (logValues) captureTypeInferenceChanges()
    }
  }

  private def captureTypeInferenceChanges(): Unit = {
    typeInferenceFile.write("Location,Label,Target,OldType,NewType\n")
    labellingFile.write("Location,Label,Target,OldType,NewType,Outcome,TypeCategory,Notes\n")
    changes.sortBy(_.location).foreach { change =>
      val line = change.productIterator
        .map {
          case x: Seq[_] => "[" + x.mkString("|") + "]"
          case x         => x.toString
        }
        .mkString(",")
      typeInferenceFile.write(s"$line\n")(OpenOptions.append)
      if (change.nodeLabel == "LOCAL" || change.nodeLabel == "METHOD_PARAMETER_IN") {
        val typeCategory = if (change.newType.startsWith(ecmaPrefix)) "BUILTIN" else "UDT"
        labellingFile.write(s"$line,_OUTCOME_,$typeCategory,_NOTES_\n")(OpenOptions.append)
      }
    }
  }

  private def logChange(
    location: String,
    label: String,
    name: String,
    existingTypes: String,
    inferredTypes: String
  ): Unit = {
    changes
      .addOne(InferredChange(location, label, name, existingTypes, inferredTypes))
  }

  private def builtinTypes(res: InferenceResult): InferenceResult =
    res.typ match {
      case x if builtins.contains(x.capitalize) => res.copy(typ = s"$ecmaPrefix${x.capitalize}")
      case _                                    => res
    }

  /** Determines if the node type information is made from dummy types or the "ANY" type.
    */
  private def onlyDummyOrAnyType(node: StoredNode): Boolean = {
    (node.property(PropertyNames.TYPE_FULL_NAME, "ANY") +: node.property(
      PropertyNames.DYNAMIC_TYPE_HINT_FULL_NAME,
      Seq.empty[String]
    ))
      .filterNot(_ == "ANY")
      .forall(XTypeRecovery.isDummyType)
  }

  private def violatesExistingTypeConstraint(
    res: InferenceResult,
    violatingInferenceResults: mutable.HashMap[InferenceResult, Boolean],
    usageSlice: Option[ObjectUsageSlice]
  ): Boolean =
    violatingInferenceResults.getOrElseUpdate(
      res, {
        usageSlice match {
          case Some(slice) =>
            val capitalizedType = res.typ.stripPrefix(ecmaPrefix).capitalize
            val typeDecl        = cpg.typeDecl.nameExact(capitalizedType).l

            // If we don't have the type decl then we can't invalidate the inference
            if (typeDecl.isEmpty) return false

            typeDecl.isExternal.headOption match {
              case Some(isExternal) if isExternal => return false
              case _ => // if it is not external, then we have the full definition for the type
            }

            // Get all member names including inherited ones
            val memberNames = typeDecl.member.name.toSet ++ typeDecl.inheritsFromTypeFullName
              .flatMap(t => cpg.typeDecl(t.takeWhile(_ != '<')).member.name.toSet)
              .toSet
            val invokedCalls   = slice.invokedCalls.map(_.callName).filterNot(_.startsWith("<operator")).toSet
            val differingCalls = invokedCalls.diff(memberNames)
            if (differingCalls.nonEmpty) {
              // If there are calls not associated with the type, then we have a constraint violation
              if (logValues)
                println(s"""
                   |Inference of $capitalizedType for ${res.scope}:${res.targetIdentifier} violates existing type definition:
                   | - Specified members $memberNames
                   | - Actual calls in slice $invokedCalls
                   |""".stripMargin)
              true
            } else {
              false
            }
          case None =>
            false // if the associate slice cannot we retrieved then we cannot reliably say this "violates" anything
        }
      }
    )

  val builtins: Set[String] = Set(
    "AggregateError",
    "Array",
    "ArrayBuffer",
    "AsyncFunction",
    "AsyncGenerator",
    "AsyncGeneratorFunction",
    "Atomics",
    "BigInt",
    "BigInt64Array",
    "BigUint64Array",
    "Boolean",
    "Buffer.from",
    "DataView",
    "Date",
    "Error",
    "EvalError",
    "FinalizationRegistry",
    "Float32Array",
    "Float64Array",
    "Function",
    "Generator",
    "GeneratorFunction",
    "HTMLImageElement",
    "Infinity",
    "Int16Array",
    "Int32Array",
    "Int8Array",
    "InternalError",
    "Intl",
    "JSON",
    "Map",
    "Math",
    "NaN",
    "Number",
    "Object",
    "Promise",
    "Proxy",
    "RangeError",
    "ReferenceError",
    "Reflect",
    "RegExp",
    "Set",
    "SharedArrayBuffer",
    "String",
    "Symbol",
    "SyntaxError",
    "TypeError",
    "TypedArray",
    "URIError",
    "Uint16Array",
    "Uint32Array",
    "Uint8Array",
    "Uint8ClampedArray",
    "WeakMap",
    "WeakRef",
    "WeakSet"
  )

}
