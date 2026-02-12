package io.shiftleft.semanticcpg.validation

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.passes.CpgPass

import scala.collection.mutable
import io.shiftleft.semanticcpg.language.*
import org.slf4j.{Logger, LoggerFactory}

class ValidationError(msg: String) extends RuntimeException(msg) {}

abstract class AbstractValidator(cpg: Cpg) extends CpgPass(cpg) {
  val violationsBuffer: mutable.ArrayBuffer[(Any, String)] = mutable.ArrayBuffer[(Any, String)]()
  def registerViolation(key: Any, msg: String): Unit = {
    // when debugging, add the breakpoint here
    violationsBuffer.addOne((key, msg))
  }

  def getViolationsCompressed(): mutable.ArrayBuffer[(String, Int)] = {
    mutable.ArrayBuffer.from(
      violationsBuffer.iterator.groupByStable(_._1).iterator.map { (k, v) => (v.head._2, v.size) }.sortBy { _._1 }
    )
  }
}

object PostFrontendValidator {
  val logger: Logger = LoggerFactory.getLogger(getClass)

  enum ErrorType:
    case FULLNAME_UNIQUE_METHOD, FULLNAME_UNIQUE_TYPE, FULLNAME_UNIQUE_TYPEDECL, MULTI_REF, BAD_REF_TYPE, NONLOCAL_REF,
      MULTI_AST_IN, MULTI_ARG_IN, DUPLICATE_ORDER
}
/*This derives from CpgPass in order to get the good logging (e.g. timing, etc).
 * The pass is initially very bare-bones and under-complicated.
 *
 * Goal is to improve the pass once we fixed current violations, and then have new ideas what to check.
 * Then plug in faster checking code, then enable in sptests and prod.*/
class PostFrontendValidator(cpg: Cpg, throwOnError: Boolean) extends AbstractValidator(cpg) {
  import PostFrontendValidator.logger
  import PostFrontendValidator.ErrorType.*

  def run(): Unit = createAndApply()

  def checkFullnameUniqueness(node: nodes.StoredNode): Unit = node match {
    case m: nodes.Method =>
      if (cpg.method.fullNameExact(m.fullName).size != 1) {
        registerViolation(FULLNAME_UNIQUE_METHOD, s"Fullname ${m}  - ${m.fullName} should occur at most once")
      }
    case m: nodes.Type =>
      if (cpg.typ.fullNameExact(m.fullName).size != 1) {
        registerViolation(FULLNAME_UNIQUE_TYPE, s"Fullname ${m}  - ${m.fullName} should occur at most once")
      }
    case m: nodes.TypeDecl =>
      if (cpg.typeDecl.fullNameExact(m.fullName).size != 1) {
        registerViolation(FULLNAME_UNIQUE_TYPEDECL, s"Fullname ${m}  - ${m.fullName} should occur at most once")
      }
    case _ =>
  }

  // we're writing our owen because the Contains-pass might not have run yet
  def findMethod(node: nodes.StoredNode): Option[nodes.Method] = {
    var cur = node
    while (true) {
      cur match {
        case m: nodes.Method => return Some(m)
        case null            => return None
        case _               =>
      }
      cur = cur._astIn.headOption.orNull
    }
    ???
  }

  def checkRefs(node: nodes.StoredNode): Unit = node match {
    case id: nodes.Identifier =>
      val idM = findMethod(id)
      // Now, we would like to check for missing refs. However, these appear in lots of tests which wouldn't compile.
      // As long as we accept un-compileable code-fragments as valid, we cannot test for this.
      /*
      if (id._refOut.size == 0)
        registerViolation(
          "missing refs",
          s"Node ${node} (${id.code}) in ${idM.map { _.fullName }} should have one outgoing REF edge"
        }
       */
      if (id._refOut.size > 1)
        registerViolation(
          MULTI_REF,
          s"Node ${node} (${id.code}) in ${idM
              .map { _.fullName }} should have at most one outgoing REF edge (found: ${id._refOut.l} / ${id._refOut.flatMap(findMethod).map { _.fullName }.l})"
        )
      try {
        id.refsTo.foreach { _ => }
      } catch {
        case ex: java.lang.ClassCastException =>
          registerViolation(BAD_REF_TYPE, s"Node ${node} has badly typed outgoing refs: ${ex}")
      }
      val dstM = id._refOut.headOption.flatMap(findMethod)
      if (idM.isDefined && dstM.isDefined && idM != dstM) {
        registerViolation(
          NONLOCAL_REF,
          s"Node ${node} in method ${idM.get} (${idM.get.fullName}) refers to ${id._refOut.head} in ${dstM.get} (${dstM.get.fullName})"
        )
      }

    case _ =>
  }

  def checkDuplicateOrder(node: nodes.StoredNode): Unit = {
    node match {
      // CFG nodes must have different orders. But when CFG and non-CFG nodes collide, that's usually fine, e.g.
      // CALL and LOCAL: LOCAL yields an empty CFG, so no harm in combining it with CALL under the Cfg.(++) operation.
      case astNode: nodes.AstNode =>
        astNode.astChildren
          .collectAll[nodes.CfgNode]
          .filterNot {
            case _: nodes.Declaration | _: nodes.Annotation => true
            case n: nodes.TypeRef if n.order == -1          => true
            case _                                          => false
          }
          .groupBy(_.order)
          .foreach { case (order, nodes) =>
            if (nodes.size > 1) {
              registerViolation(DUPLICATE_ORDER, s"Nodes $nodes have same order $order inside node $astNode")
            }
          }
      case _ => // Do nothing: other nodes worth checking?
    }
  }

  override def run(builder: DiffGraphBuilder): Unit = {
    for (node <- cpg.all) {

      checkFullnameUniqueness(node)

      checkRefs(node)

      if (node._astIn.size > 1) {
        registerViolation(MULTI_AST_IN, s"Node ${node} should have at most one incoming AST edge")
      }
      // is this really required? We could relax that condition.
      if (node._argumentIn.size > 1) {
        registerViolation(MULTI_ARG_IN, s"Node ${node} should have at most one incoming ARGUMENT edge")
      }

      checkDuplicateOrder(node)

    }
    val violations = getViolationsCompressed()
    if (violations.nonEmpty) {
      val err = violations.map { (msg, n) => s"${msg} ($n instances)" }.mkString("\n")
      logger.warn(s"Graph validation failure:\n$err")
      if (throwOnError) throw new ValidationError(err)
    }
  }
}
