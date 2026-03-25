package io.shiftleft.semanticcpg.validation

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.passes.CpgPass

import scala.collection.mutable
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.validation.PostFrontendValidator.ErrorType
import org.slf4j.{Logger, LoggerFactory}

class ValidationError(msg: String) extends RuntimeException(msg) {}

enum ValidationLevel(val value: Int) extends Ordered[ValidationLevel] {
  case V0 extends ValidationLevel(0)
  case V1 extends ValidationLevel(1)

  override def compare(that: ValidationLevel): Int = this.value.compareTo(that.value)
}

/** Base validator pass that collects validation violations during graph traversal.
  *
  * @param cpg
  *   the code property graph to validate
  */
abstract class AbstractValidator(cpg: Cpg) extends CpgPass(cpg) {
  private val violationsBuffer: mutable.ArrayBuffer[(Any, String)] = mutable.ArrayBuffer[(Any, String)]()

  /** Register a validation violation for the given key. */
  def registerViolation(key: Any, msg: String): Unit = {
    // when debugging, add the breakpoint here
    violationsBuffer.addOne((key, msg))
  }

  /** @return
    *   compressed violations as (key, message, count) tuples in stable key order
    */
  def getViolationsCompressedWithKey: mutable.ArrayBuffer[(Any, String, Int)] = {
    mutable.ArrayBuffer.from(
      violationsBuffer.iterator
        .groupByStable(_._1)
        .iterator
        .map { case (key, violations) => (key, violations.head._2, violations.size) }
        .toSeq
        .sortBy(_._1.toString)
    )
  }
}

/** Validator checks run after frontend passes complete. */
object PostFrontendValidator {
  private val logger: Logger = LoggerFactory.getLogger(getClass)

  /** Newly added error types should have an `addedInLevel` one level higher than the highest */
  enum ErrorType(val addedInLevel: ValidationLevel) {
    case FULLNAME_UNIQUE_METHOD   extends ErrorType(ValidationLevel.V1)
    case FULLNAME_UNIQUE_TYPE     extends ErrorType(ValidationLevel.V1)
    case FULLNAME_UNIQUE_TYPEDECL extends ErrorType(ValidationLevel.V1)
    case MULTI_REF                extends ErrorType(ValidationLevel.V1)
    case BAD_REF_TYPE             extends ErrorType(ValidationLevel.V1)
    case NONLOCAL_REF             extends ErrorType(ValidationLevel.V1)
    case MULTI_AST_IN             extends ErrorType(ValidationLevel.V1)
    case MULTI_ARG_IN             extends ErrorType(ValidationLevel.V1)
    case DUPLICATE_ORDER          extends ErrorType(ValidationLevel.V1)
  }
}

/** This derives from CpgPass in order to get the good logging (e.g. timing, etc.). The pass is initially very
  * bare-bones and under-complicated.
  *
  * Goal is to improve the pass once we fixed current violations, and then have new ideas what to check. Then plug in
  * faster checking code, then enable in sptests and prod.
  *
  * NOTE: All validation checks with a level lower or equal to [[fatalValidationLevel]] will result in an exception. See
  * [[ValidationLevel]] and [[ErrorType]] for the highest validation level.
  */
class PostFrontendValidator(cpg: Cpg, fatalValidationLevel: ValidationLevel) extends AbstractValidator(cpg) {
  import PostFrontendValidator.logger
  import PostFrontendValidator.ErrorType.*

  /** Run the validator using the standard pass mechanism. */
  def run(): Unit = createAndApply()

  /** Walk up the AST until the enclosing method is found, if any. We are writing our own because the Contains-pass
    * might not have run yet.
    */
  private def findEnclosingMethod(node: nodes.StoredNode): Option[nodes.Method] = {
    @annotation.tailrec
    def loop(cur: nodes.StoredNode): Option[nodes.Method] = cur match {
      case m: nodes.Method => Some(m)
      case null            => None
      case _               => loop(cur._astIn.headOption.orNull)
    }
    loop(node)
  }

  /** Verify that fullName is unique for selected node kinds.
    */
  private def checkFullnameUniqueness(node: nodes.StoredNode): Unit = node match {
    case m: nodes.Method if cpg.method.fullNameExact(m.fullName).size != 1 =>
      registerViolation(FULLNAME_UNIQUE_METHOD, s"Fullname $m  - ${m.fullName} should occur at most once")
    case m: nodes.Type if cpg.typ.fullNameExact(m.fullName).size != 1 =>
      registerViolation(FULLNAME_UNIQUE_TYPE, s"Fullname $m  - ${m.fullName} should occur at most once")
    case m: nodes.TypeDecl if cpg.typeDecl.fullNameExact(m.fullName).size != 1 =>
      registerViolation(FULLNAME_UNIQUE_TYPEDECL, s"Fullname $m  - ${m.fullName} should occur at most once")
    case _ =>
  }

  /** Check that an identifier has at most one outgoing REF edge.
    *
    * @param id
    *   the identifier being checked
    * @param refOut
    *   outgoing REF targets
    * @param enclosingMethod
    *   method enclosing the identifier, if any
    */
  private def checkRefOutSize(
    id: nodes.Identifier,
    refOut: List[nodes.StoredNode],
    enclosingMethod: Option[nodes.Method]
  ): Unit = {
    // Now, we would like to check for missing refs. However, these appear in lots of tests which wouldn't compile.
    // As long as we accept un-compilable code-fragments as valid, we cannot test for this.
    /*
    if (refOut.size == 0)
      registerViolation(
        "missing refs",
        s"Node $node (${id.code}) in ${idM.map { _.fullName }} should have one outgoing REF edge"
      )
     */
    if (refOut.size > 1) {
      val enclosingMethodFullName = enclosingMethod.map(_.fullName)
      val refOutFullNames         = refOut.flatMap(findEnclosingMethod).map(_.fullName)
      registerViolation(
        MULTI_REF,
        s"Node $id (${id.code}) in $enclosingMethodFullName should have at most one outgoing REF edge (found: $refOut / $refOutFullNames)"
      )
    }
  }

  /** Validate that the nodes reachable by REF edges are properly typed for the identifier.
    *
    * @param id
    *   the identifier being checked
    * @param refOut
    *   outgoing REF targets
    */
  private def checkRefOutType(id: nodes.Identifier, refOut: List[nodes.StoredNode]): Unit = {
    try {
      id.refsTo.foreach { _ => }
    } catch {
      case ex: java.lang.ClassCastException =>
        registerViolation(BAD_REF_TYPE, s"Node $id has badly typed outgoing refs: $ex")
    }
  }

  /** Check that outgoing REF edges do not cross method boundaries.
    *
    * @param id
    *   the identifier being checked
    * @param refOut
    *   outgoing REF targets
    * @param enclosingMethod
    *   method enclosing the identifier, if any
    */
  private def checkNonLocalRefOut(
    id: nodes.Identifier,
    refOut: List[nodes.StoredNode],
    enclosingMethod: Option[nodes.Method]
  ): Unit = {
    val dstMethod = refOut.headOption.flatMap(findEnclosingMethod)
    (enclosingMethod, dstMethod) match {
      case (Some(src), Some(dst)) if src != dst =>
        registerViolation(
          NONLOCAL_REF,
          s"Node $id in method $src (${src.fullName}) refers to ${refOut.head} in $dst (${dst.fullName})"
        )
      case _ =>
    }
  }

  /** Check identifier references for type safety, non-local refs, and fan-out.
    */
  private def checkRefs(node: nodes.StoredNode): Unit = {
    node match {
      case id: nodes.Identifier =>
        val enclosingMethod = findEnclosingMethod(id)
        val refOut          = id._refOut.l
        checkRefOutSize(id, refOut, enclosingMethod)
        checkRefOutType(id, refOut)
        checkNonLocalRefOut(id, refOut, enclosingMethod)
      case _ =>
    }
  }

  /** Return CFG children for an AST node, excluding declarations and annotations.
    *
    * @param astNode
    *   the AST node to inspect
    * @return
    *   iterator of CFG nodes
    */
  private def cfgChildren(astNode: nodes.AstNode): Iterator[nodes.CfgNode] = {
    astNode.astChildren.isCfgNode.filter {
      // We are not interested in CFG nodes that are also declarations or annotations,
      // as these can be combined with other CFG nodes without harm.
      // When CFG and non-CFG nodes collide, that's usually fine, e.g.
      // CALL and LOCAL: LOCAL yields an empty CFG, so no harm in combining it with CALL under the Cfg.(++) operation.
      case _: nodes.Declaration | _: nodes.Annotation => false
      case _                                          => true
    }
  }

  /** Ensure CFG-node orders are unique within an AST node. */
  private def checkDuplicateOrder(node: nodes.StoredNode): Unit = node match {
    case astNode: nodes.AstNode =>
      cfgChildren(astNode).groupBy(_.order).foreach { case (order, nodes) =>
        if (nodes.size > 1) {
          registerViolation(DUPLICATE_ORDER, s"Nodes $nodes have same order $order inside node $astNode")
        }
      }
    case _ => // Do nothing: other nodes worth checking?
  }

  /** Ensure a node has at most one incoming AST edge. */
  private def checkAstIn(node: nodes.StoredNode): Unit = {
    if (node._astIn.size > 1) {
      registerViolation(MULTI_AST_IN, s"Node $node should have at most one incoming AST edge")
    }
  }

  /** Ensure a node has at most one incoming ARGUMENT edge. */
  private def checkArgumentIn(node: nodes.StoredNode): Unit = {
    if (node._argumentIn.size > 1) {
      registerViolation(MULTI_ARG_IN, s"Node $node should have at most one incoming ARGUMENT edge")
    }
  }

  /** Log compressed violations and optionally throw on error. */
  private def reportViolations(): Unit = {
    val violations = getViolationsCompressedWithKey
    if (violations.nonEmpty) {
      val err = violations.map { (_, msg, n) => s"$msg ($n instances)" }.mkString("\n")
      logger.warn(s"Graph validation failure:\n$err")

      val fatalViolations = violations.filter { case (key, _, _) =>
        key match {
          case err: ErrorType => err.addedInLevel <= fatalValidationLevel
          case _              => true
        }
      }

      if (fatalViolations.nonEmpty) {
        val fatalErr = fatalViolations.map { case (_, msg, n) => s"$msg ($n instances)" }.mkString("\n")
        throw new ValidationError(s"Fatal graph validation errors (Level <= $fatalValidationLevel):\n$fatalErr")
      }
    }
  }

  /** Execute validation checks over all nodes and report compressed violations. */
  override def run(builder: DiffGraphBuilder): Unit = {
    for (node <- cpg.all) {
      checkFullnameUniqueness(node)
      checkRefs(node)
      checkAstIn(node)
      checkArgumentIn(node)
      checkDuplicateOrder(node)
    }
    reportViolations()
  }

}
