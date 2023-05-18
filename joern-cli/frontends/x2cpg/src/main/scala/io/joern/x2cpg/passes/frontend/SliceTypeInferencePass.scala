package io.joern.x2cpg.passes.frontend

import better.files.File
import better.files.File.OpenOptions
import io.joern.slicing.{ProgramUsageSlice, SliceConfig, SliceMode, UsageSlicing}
import io.joern.x2cpg.JoernTI
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.PropertyNames
import io.shiftleft.codepropertygraph.generated.nodes.{CfgNode, StoredNode}
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory

import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Success, Using}

class SliceTypeInferencePass(cpg: Cpg, joernti: Option[JoernTI] = None, pathSep: String = ":") extends CpgPass(cpg) {

  private lazy val logger      = LoggerFactory.getLogger(classOf[SliceTypeInferencePass])
  private lazy val sliceConfig = SliceConfig(sliceMode = SliceMode.Usages)
  private val changes          = ArrayBuffer.empty[InferredChange]

  private case class InferredChange(
    location: String,
    nodeLabel: String,
    target: String,
    oldType: String,
    newType: String,
    confidence: Float
  )

  private def location(n: CfgNode): String =
    s"${n.file.name.headOption.getOrElse("Unknown")}#L${n.lineNumber.getOrElse(-1)}"

  override def run(builder: DiffGraphBuilder): Unit = {
    joernti.foreach { ti =>
      Using.resource(ti) { tidal =>
        cpg.method.isExternal(false).foreach { method =>
          val slice = UsageSlicing.calculateUsageSlice(cpg, method, sliceConfig).asInstanceOf[ProgramUsageSlice]
          tidal.infer(slice) match {
            case Failure(exception) =>
              logger.warn("Unable to enrich compilation unit type information with joernti, continuing...", exception)
            case Success(inferenceResults) =>
              // Set the type of targets which only have dummy types or have no types
              inferenceResults.filter(_.confidence > 0.8).foreach { res =>
                method.ast.isIdentifier
                  .nameExact(res.targetIdentifier)
                  .filter(onlyDummyOrAnyType) // We only want to write to nodes that need type info
                  .foreach { tgt =>
                    builder.setNodeProperty(tgt, PropertyNames.TYPE_FULL_NAME, res.typ)
                    changes
                      .addOne(
                        InferredChange(
                          location(tgt),
                          tgt.label,
                          res.targetIdentifier,
                          tgt.typeFullName,
                          res.typ,
                          res.confidence
                        )
                      )
                    // If there is a call where the identifier is the receiver then this also needs to be updated
                    if (tgt.argumentIndex <= 1 && tgt.astSiblings.isCall.exists(_.argumentIndex >= 2)) {
                      tgt.astSiblings.isCall.nameNot("<operator.*").find(onlyDummyOrAnyType).foreach { call =>
                        val inferredMethodCall = Seq(res.typ, call.name).mkString(pathSep)
                        builder.setNodeProperty(call, PropertyNames.METHOD_FULL_NAME, inferredMethodCall)
                        changes
                          .addOne(
                            InferredChange(
                              location(call),
                              call.label,
                              res.targetIdentifier,
                              call.methodFullName,
                              res.typ,
                              res.confidence
                            )
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
      f.write("Location,Label,Target,Old,New,Confidence\n")
      changes.foreach { change => f.write(change.productIterator.mkString(",") + "\n")(OpenOptions.append) }
    }
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
