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
import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Success, Using}

/** Makes use of statistical techniques to infer the type of a target object based on a CPG slice.
  */
class SliceBasedTypeInferencePass(
  cpg: Cpg,
  joernti: Option[JoernTI] = None,
  pathSep: String = ":",
  minConfidence: Float = 0.8f,
  typesNotToInfer: Set[String] = Set("object", "unk", "void")
) extends CpgPass(cpg) {

  private lazy val logger         = LoggerFactory.getLogger(classOf[SliceBasedTypeInferencePass])
  private lazy val sliceConfig    = SliceConfig(sliceMode = SliceMode.Usages)
  private val changes             = ArrayBuffer.empty[InferredChange]
  private val ecmaPrefix          = "__ecma."
  private lazy val timeOfAnalysis = DateTimeFormatter.ofPattern("yyyy-MM-dd__HH_mm").format(LocalDateTime.now)

  private val typeInferenceFile = cpg.metaData.root.map(File(_)).headOption match {
    case Some(project) => File(s"./${project.name}_type_inference_$timeOfAnalysis.csv")
    case None          => File(s"./type_inference_$timeOfAnalysis.csv")
  }

  private val labellingFile = cpg.metaData.root.map(File(_)).headOption match {
    case Some(project) => File(s"./${project.name}_labelling_file_$timeOfAnalysis.csv")
    case None          => File(s"./type_inference_$timeOfAnalysis.csv")
  }

  private val yieldFile = cpg.metaData.root.map(File(_)).headOption match {
    case Some(project) => File(s"./${project.name}_inference_yield_$timeOfAnalysis.csv")
    case None          => File(s"./inference_yield_$timeOfAnalysis.csv")
  }

  private case class InferredChange(
    location: String,
    nodeLabel: String,
    target: String,
    oldType: String,
    newType: String
  )

  private def location(n: StoredNode): String =
    s"${n.file.name.headOption.getOrElse("Unknown")}#L${n.property(PropertyNames.LINE_NUMBER, None).getOrElse(-1)}"

  lazy private val pathPattern = Pattern.compile("[\"']([\\w/.]+)[\"']")

  override def run(builder: DiffGraphBuilder): Unit = {
    joernti.foreach { ti =>
      Using.resource(ti) { tidal =>
        val slice = UsageSlicing.calculateUsageSlice(cpg, sliceConfig).asInstanceOf[ProgramUsageSlice]
        tidal.infer(slice) match {
          case Failure(exception) =>
            logger.warn("Unable to enrich compilation unit type information with joernti, continuing...", exception)
          case Success(inferenceResults) =>
            val filteredResults = inferenceResults
              .filter(_.confidence > minConfidence)
              .filterNot(res => pathPattern.matcher(res.typ).matches())
              .filterNot(res => typesNotToInfer.contains(res.typ.toLowerCase))
              .map(builtinTypes)
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
                // Set the type of targets which only have dummy types or have no types
                results.foreach { res =>
                  // Handle locals
                  methodDeclarations.nameExact(res.targetIdentifier).filter(onlyDummyOrAnyType).foreach { tgt =>
                    builder.setNodeProperty(tgt, PropertyNames.TYPE_FULL_NAME, res.typ)
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
                val targetNodes = cpg.graph
                  .nodes("LOCAL", "IDENTIFIER", "CALL", "METHOD_PARAMETER_IN")
                  .cast[AstNode]
                  .l
                val untypedNodes = targetNodes.count(onlyDummyOrAnyType) - changes.size
                val typedNodes   = targetNodes.size - untypedNodes
                yieldFile.write("LABEL,COUNT\n")
                yieldFile.write(s"NUM_NODES,${targetNodes.size}\n")(OpenOptions.append)
                yieldFile.write(s"UNTYPED,$untypedNodes\n")(OpenOptions.append)
                yieldFile.write(s"TYPED,$typedNodes\n")(OpenOptions.append)
                yieldFile.write(s"INFERRED,${changes.size}\n")(OpenOptions.append)
              }
        }
      }
      captureTypeInferenceChanges()
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
      case x if x.charAt(0).isLower && builtins.contains(x.capitalize) => res.copy(typ = s"$ecmaPrefix${x.capitalize}")
      case _                                                           => res
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
