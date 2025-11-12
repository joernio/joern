package io.shiftleft.semanticcpg.validation

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.passes.CpgPass

import scala.collection.mutable
import io.shiftleft.semanticcpg.language.*
import org.slf4j.{Logger, LoggerFactory}

class ValidationError(msg: String) extends RuntimeException(msg) {}

abstract class AbstractValidator(cpg: Cpg) extends CpgPass(cpg) {
  val violations: mutable.ArrayBuffer[(String, String)] = mutable.ArrayBuffer[(String, String)]()
  def registerViolation(key: String, msg: String): Unit = {
    // when debugging, add the breakpoint here
    violations.addOne((key, msg))
  }

  def getViolationsCompressed(): mutable.ArrayBuffer[(String, Int)] = {
    mutable.ArrayBuffer.from(
      violations.iterator.groupByStable(_._1).iterator.map { (k, v) => (v.head._2, v.size) }.sortBy { _._1 }
    )
  }
}

object PostFrontendValidator {
  val logger: Logger = LoggerFactory.getLogger(getClass)
}
/*This derives from CpgPass in order to get the good logging (e.g. timing, etc).
 * The pass is initially very bare-bones and under-complicated.
 *
 * Goal is to improve the pass once we fixed current violations, and then have new ideas what to check.
 * Then plug in faster checking code, then enable in sptests and prod.*/
class PostFrontendValidator(cpg: Cpg, throwOnError: Boolean) extends AbstractValidator(cpg) {
  import PostFrontendValidator.logger

  def run(): Unit = createAndApply()

  override def run(builder: DiffGraphBuilder): Unit = {
    for (node <- cpg.all) {
      if (node._astIn.size > 1) {
        registerViolation("astIn num", s"Node ${node} should have at most one incoming AST edge")
      }
      // is this really required? We could relax that condition.
      if (node._argumentIn.size > 1) {
        registerViolation("argumentIn num", s"Node ${node} should have at most one incoming ARGUMENT edge")
      }
      node match {
        case m: nodes.Method =>
          if (cpg.method.fullNameExact(m.fullName).size != 1) {
            registerViolation(
              "fullname uniqueness method",
              s"Fullname ${m}  - ${m.fullName} should occur at most oncee"
            )
          }
        case m: nodes.Type =>
          if (cpg.typ.fullNameExact(m.fullName).size != 1) {
            registerViolation("fullname uniqueness type", s"Fullname ${m}  - ${m.fullName} should occur at most oncee")
          }
        case m: nodes.TypeDecl =>
          if (cpg.typeDecl.fullNameExact(m.fullName).size != 1) {
            registerViolation(
              "fullname uniqueness typedecl",
              s"Fullname ${m}  - ${m.fullName} should occur at most oncee"
            )
          }
        case _ =>
      }
    }
    val tmp = getViolationsCompressed()
    if (tmp.nonEmpty) {
      val err = tmp.map { (msg, n) => s"${msg} ($n instances)" }.mkString("\n")
      logger.warn(s"Graph validation failure:\n$err")
      if (throwOnError) throw new ValidationError(err)
    }
  }
}
