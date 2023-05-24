package io.joern.dataflowengineoss.slicing

import better.files.File
import better.files.File.OpenOptions
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.PropertyNames
import io.shiftleft.codepropertygraph.generated.nodes.{CfgNode, StoredNode}
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory

import java.util.regex.Pattern
import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Success, Using}

/** Makes use of statistical techniques to infer the type of a target object based on a CPG slice.
  */
class SliceBasedTypeInferencePass(
  cpg: Cpg,
  joernti: Option[JoernTI] = None,
  pathSep: String = ":",
  minConfidence: Float = 0.8f
) extends CpgPass(cpg) {

  private lazy val logger      = LoggerFactory.getLogger(classOf[SliceBasedTypeInferencePass])
  private lazy val sliceConfig = SliceConfig(sliceMode = SliceMode.Usages)
  private val changes          = ArrayBuffer.empty[InferredChange]

  private case class InferredChange(
    location: String,
    nodeLabel: String,
    target: String,
    oldType: Seq[String],
    newType: Seq[String],
    confidence: Float
  )

  private def location(n: CfgNode): String =
    s"${n.file.name.headOption.getOrElse("Unknown")}#L${n.lineNumber.getOrElse(-1)}"

  lazy private val pathPattern = Pattern.compile("[\"']([\\w/.]+)[\"']")

  override def run(builder: DiffGraphBuilder): Unit = {
    joernti.foreach { ti =>
      Using.resource(ti) { tidal =>
        val slice = UsageSlicing.calculateUsageSlice(cpg, sliceConfig).asInstanceOf[ProgramUsageSlice]
        tidal.infer(slice) match {
          case Failure(exception) =>
            logger.warn("Unable to enrich compilation unit type information with joernti, continuing...", exception)
          case Success(inferenceResults) =>
            inferenceResults
              // avoid using inferences that look like local paths
              .filterNot(res => pathPattern.matcher(res.typ).matches())
              .groupBy(_.scope)
              .foreach { case (scope, results) =>
                val methodIdentifiers = cpg.method
                  .fullNameExact(scope)
                  .ast
                  .isIdentifier
                  .l
                // Set the type of targets which only have dummy types or have no types
                results.filter(_.confidence > minConfidence).foreach { res =>
                  methodIdentifiers
                    .nameExact(res.targetIdentifier)
                    .filter(onlyDummyOrAnyType) // We only want to write to nodes that need type info
                    .foreach { tgt =>
                      builder.setNodeProperty(tgt, PropertyNames.TYPE_FULL_NAME, res.typ)
                      logChange(
                        location(tgt),
                        tgt.label,
                        res.targetIdentifier,
                        tgt.typeFullName +: tgt.dynamicTypeHintFullName,
                        res.alternatives.map(_.typ),
                        res.confidence
                      )
                      // If there is a call where the identifier is the receiver then this also needs to be updated
                      if (tgt.argumentIndex <= 1 && tgt.astSiblings.isCall.exists(_.argumentIndex >= 2)) {
                        tgt.astSiblings.isCall.nameNot("<operator.*").find(onlyDummyOrAnyType).foreach { call =>
                          val inferredMethodCall = Seq(res.typ, call.name).mkString(pathSep)
                          builder.setNodeProperty(call, PropertyNames.METHOD_FULL_NAME, inferredMethodCall)
                          logChange(
                            location(call),
                            call.label,
                            res.targetIdentifier,
                            call.methodFullName +: call.dynamicTypeHintFullName,
                            res.alternatives.map(_.typ),
                            res.confidence
                          )
                        }
                      }
                    }
                  res.targetIdentifier
                }
              }
        }
      }
      val f = File("./type_inference.csv")
      f.write("Location,Label,Target,OldTypes,SuggestedTypes,Confidence\n")
      changes.foreach { change =>
        f.write(
          change.productIterator
            .map {
              case x: Seq[_] => "[" + x.mkString("|") + "]"
              case x         => x.toString
            }
            .mkString(",") + "\n"
        )(OpenOptions.append)
      }
    }
  }

  private def logChange(
    location: String,
    label: String,
    name: String,
    existingTypes: Seq[String],
    inferredTypes: Seq[String],
    confidence: Float
  ): Unit = {
    changes
      .addOne(InferredChange(location, label, name, existingTypes, inferredTypes, confidence))
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

}
