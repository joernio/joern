package io.joern.dataflowengineoss.queryengine

import io.joern.dataflowengineoss.globalFromLiteral
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.operatorextension.allAssignmentTypes
import io.shiftleft.semanticcpg.utils.MemberAccess.isFieldAccess
import org.slf4j.LoggerFactory

import java.util.concurrent.*
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

case class StartingPointWithSource(startingPoint: CfgNode, source: StoredNode)
case class UsageInput(src: StoredNode, typeDecl: TypeDecl, astNode: AstNode)
case class ResultSummary(result: List[StartingPointWithSource], methodTasks: List[UsageInput])
object SourcesToStartingPoints {

  private val log = LoggerFactory.getLogger(SourcesToStartingPoints.getClass)

  def sourceTravsToStartingPoints[NodeType](sourceTravs: IterableOnce[NodeType]*): List[StartingPointWithSource] = {
    val executorService = Executors.newWorkStealingPool()
    try {
      val sources = sourceTravs
        .flatMap(_.iterator)
        .collect { case n: StoredNode => n }
        .dedup
        .toList
      sources.headOption
        .map(src => {
          // We need to get Cpg wrapper from graph. Hence we are taking head element from source iterator.
          // This will also ensure if the source list is empty then these tasks are invoked.
          val cpg                           = Cpg(src.graph)
          val (startingPoints, methodTasks) = calculateStartingPoints(sources, executorService)
          val startingPointFromUsageInOtherClasses =
            calculateStatingPointsWithUsageInOtherClasses(methodTasks, cpg, executorService)
          (startingPoints ++ startingPointFromUsageInOtherClasses)
            .sortBy(_.source.id)
        })
        .getOrElse(Nil)
    } catch {
      case e: RejectedExecutionException =>
        log.error("Unable to execute 'SourceTravsToStartingPoints` task", e); List()
    } finally {
      executorService.shutdown()
    }
  }

  /** This will process and identify the starting points except the usage in other classes. This run will identify the
    * required tasks for calculating starting points with usage in other classes.
    *
    * @param sources
    *   \- Sources list
    * @param executorService
    *   \- Shared executor service to process the task in parallel
    * @return
    *   List of StartingPointWithSource and List of tasks for calculating starting points with usage in other classes
    */
  private def calculateStartingPoints(
    sources: List[StoredNode],
    executorService: ExecutorService
  ): (List[StartingPointWithSource], List[UsageInput]) = {
    val allExceptUsageInOtherClasses       = SourceStartingPointResultAggregator(sources.size)
    val allExceptUsageInOtherClassesThread = new Thread(allExceptUsageInOtherClasses)
    allExceptUsageInOtherClassesThread.setName("All except usage in other classes result aggregator")
    allExceptUsageInOtherClassesThread.start()
    sources.foreach(src =>
      executorService.submit(new SourceToStartingPoints(src, allExceptUsageInOtherClasses.resultQueue))
    )
    allExceptUsageInOtherClassesThread.join()
    (allExceptUsageInOtherClasses.finalResult.toList, allExceptUsageInOtherClasses.methodTasks.toList)
  }

  /** This will calculate starting points by finding the usage in other classes.
    *
    * @param methodTasks
    *   \- Inputs required for processing
    * @param cpg
    *   \- cpg to get list of methods
    * @param executorService
    *   \- Shared executor service to process the task in parallel
    * @return
    *   List of StartingPointWithSource
    */
  private def calculateStatingPointsWithUsageInOtherClasses(
    methodTasks: List[UsageInput],
    cpg: Cpg,
    executorService: ExecutorService
  ): List[StartingPointWithSource] = {
    val methods                   = cpg.method.l
    val usageInOtherClasses       = SourceStartingPointResultAggregator(methods.size)
    val usageInOtherClassesThread = new Thread(usageInOtherClasses)
    usageInOtherClassesThread.setName("Usage in other classes result aggregator")
    usageInOtherClassesThread.start()
    methods.foreach(m =>
      executorService.submit(new SourceToStartingPointsInMethod(m, methodTasks, usageInOtherClasses.resultQueue))
    )
    usageInOtherClassesThread.join()
    usageInOtherClasses.finalResult.toList
  }
}

/** Independent thread to collect and aggregate the results (StartingPointWithSource) from all the tasks. This will
  * avoid the sequential wait for aggregating results from the queue.
  *
  * @param totalNoTasks
  *   \- number of tasks for the exit condition.
  */
class SourceStartingPointResultAggregator(private var totalNoTasks: Int) extends Runnable {
  val logger      = LoggerFactory.getLogger(this.getClass)
  val finalResult = ListBuffer[StartingPointWithSource]()
  val methodTasks = ListBuffer[UsageInput]()
  val resultQueue = LinkedBlockingQueue[ResultSummary]()
  override def run(): Unit = {
    var terminate = false
    while (!terminate) {
      val taskResult = resultQueue.take()
      finalResult ++= taskResult.result
      methodTasks ++= taskResult.methodTasks
      totalNoTasks -= 1
      if (totalNoTasks == 0) {
        logger.debug("Shutting down SourceStartingPointResultAggregator thread")
        terminate = true
      }
    }
  }
}

class SourceToStartingPointsInMethod(
  m: Method,
  usageInputs: List[UsageInput],
  resultQueue: LinkedBlockingQueue[ResultSummary]
) extends BaseSourceToStartingPoints {
  override def call(): Unit = {
    // Handling of the error situation. This will make sure aggregator thread will exit.
    val result = Try(usageInOtherClasses(m, usageInputs)) match {
      case Failure(e) =>
        logger.error("Unable to complete 'SourceToStartingPointsInMethod' task", e)
        List[StartingPointWithSource]()
      case Success(result) => result
    }
    resultQueue.put(ResultSummary(result, List()))
  }

  private def usageInOtherClasses(m: Method, usageInputs: List[UsageInput]): List[StartingPointWithSource] = {
    usageInputs.flatMap { case UsageInput(src, typeDecl, astNode) =>
      m.fieldAccess
        .or(
          _.argument(1).isIdentifier.typeFullNameExact(typeDecl.fullName),
          _.argument(1).isTypeRef.typeFullNameExact(typeDecl.fullName)
        )
        .where { x =>
          astNode match {
            case identifier: Identifier =>
              x.argument(2).isFieldIdentifier.canonicalNameExact(identifier.name)
            case fieldIdentifier: FieldIdentifier =>
              x.argument(2).isFieldIdentifier.canonicalNameExact(fieldIdentifier.canonicalName)
            case _ => Iterator.empty
          }
        }
        .takeWhile(notLeftHandOfAssignment)
        .headOption
        .map(s => StartingPointWithSource(s, src))
    }
  }
}

class SourceToStartingPoints(src: StoredNode, resultQueue: LinkedBlockingQueue[ResultSummary])
    extends BaseSourceToStartingPoints {
  override def call(): Unit = {
    // Handling of the error situation. This will make sure aggregator thread will exit.
    val (result, usageInputs) = Try(sourceToStartingPoints(src)) match {
      case Failure(e) =>
        logger.error("Unable to complete 'SourceToStartingPoints' task", e)
        (Nil, Nil)
      case Success(result) => result
    }
    resultQueue.put(ResultSummary(result.map(s => StartingPointWithSource(s, src)), usageInputs))
  }
}

/** The code below deals with member variables, and specifically with the situation where literals that initialize
  * static members are passed to `reachableBy` as sources. In this case, we determine the first usages of this member in
  * each method, traversing the AST from left to right. This isn't fool-proof, e.g., goto-statements would be
  * problematic, but it works quite well in practice.
  */
abstract class BaseSourceToStartingPoints extends Callable[Unit] {
  val logger = LoggerFactory.getLogger(this.getClass)

  protected def sourceToStartingPoints(src: StoredNode): (List[CfgNode], List[UsageInput]) = {
    src match {
      case methodReturn: MethodReturn =>
        // n.b. there's a generated `callIn` step that we really want to use, but it's shadowed by `MethodTraversal.callIn`
        (methodReturn.method._callIn.cast[Call].l, Nil)
      case lit: Literal =>
        val usageInput = targetsToClassIdentifierPair(literalToInitializedMembers(lit), src)
        val uses       = usages(usageInput)
        val globals = globalFromLiteral(lit, recursive = false).flatMap {
          case x: Identifier if x.isModuleVariable => moduleVariableToFirstUsagesAcrossProgram(x)
          case x                                   => x :: Nil
        }
        (lit :: (uses ++ globals), usageInput)
      case member: Member =>
        val usageInput = targetsToClassIdentifierPair(List(member), src)
        (usages(usageInput), usageInput)
      case x: Identifier =>
        val fieldAndIndexAccesses = withFieldAndIndexAccesses(x :: Nil)
        val capturedReferences = x.refsTo.capturedByMethodRef.referencedMethod.flatMap(firstUsagesForName(x.name, _)).l

        (
          (x :: fieldAndIndexAccesses ++ capturedReferences) flatMap {
            case x: Call => handleCallNode(x) // Handle the case if this is an arg to another call
            case x       => x :: Nil
          },
          Nil
        )
      case x: Call    => (handleCallNode(x), Nil)
      case x: CfgNode => (x :: Nil, Nil)
      case _          => (Nil, Nil)
    }
  }

  private def handleCallNode(callNode: Call): List[CfgNode] = callNode :: callNode._receiverIn.collectAll[CfgNode].l

  private def withFieldAndIndexAccesses(nodes: List[CfgNode]): List[CfgNode] =
    nodes.flatMap {
      case moduleVar: Identifier if moduleVar.isModuleVariable =>
        moduleVar :: moduleVariableToFirstUsagesAcrossProgram(moduleVar)
      case identifier: Identifier => identifier :: fieldAndIndexAccesses(identifier)
      case x                      => x :: Nil
    }

  private def fieldAndIndexAccesses(identifier: Identifier): List[CfgNode] =
    identifier.method._identifierViaContainsOut
      .nameExact(identifier.name)
      .inCall
      .collect { case c if isFieldAccess(c.name) => c }
      .l

  /** Finds the first usages of this module variable across all importing modules.
    *
    * TODO: This is wrapped in a try-catch because of the deprecated Ruby frontend crashing this process due to a
    * missing `.method` parent node in the contains graph.
    */
  private def moduleVariableToFirstUsagesAcrossProgram(moduleVar: Identifier): List[CfgNode] = Try {
    moduleVar.start.moduleVariables.references
      .groupBy(_.method)
      .map {
        case (sameModule, _) if moduleVar.method == sameModule => fieldAndIndexAccesses(moduleVar)
        case (_, references)                                   => references.filterNot(notLeftHandOfAssignment)
      }
      .flatMap(_.sortBy(i => (i.lineNumber, i.columnNumber)).headOption)
      .toList
  }.getOrElse(List.empty)

  private def usages(usageInput: List[UsageInput]): List[CfgNode] = {
    usageInput.flatMap { case UsageInput(_, typeDecl, astNode) =>
      val nonConstructorMethods = methodsRecursively(typeDecl).iterator
        .whereNot(_.nameExact(Defines.StaticInitMethodName, Defines.ConstructorMethodName, "__init__"))
        .l
      nonConstructorMethods.flatMap { m => firstUsagesOf(astNode, m, typeDecl) }
    }
  }

  /** For given method, determine the first usage of the given expression.
    */
  private def firstUsagesOf(astNode: AstNode, m: Method, typeDecl: TypeDecl): List[Expression] = {
    astNode match {
      case member: Member =>
        firstUsagesForName(member.name, m)
      case identifier: Identifier =>
        firstUsagesForName(identifier.name, m)
      case fieldIdentifier: FieldIdentifier =>
        val fieldIdentifiers = m.ast.isFieldIdentifier.sortBy(x => (x.lineNumber, x.columnNumber)).l
        fieldIdentifiers
          .canonicalNameExact(fieldIdentifier.canonicalName)
          .inFieldAccess
          // TODO `isIdentifier` seems to limit us here
          .where(_.argument(1).isIdentifier.or(_.nameExact("this", "self"), _.typeFullNameExact(typeDecl.fullName)))
          .takeWhile(notLeftHandOfAssignment)
          .l
      case _ => List()
    }
  }

  private def firstUsagesForName(name: String, m: Method): List[Expression] = {
    val identifiers      = m._identifierViaContainsOut.l
    val identifierUsages = identifiers.nameExact(name).takeWhile(notLeftHandOfAssignment).l
    val fieldIdentifiers = m.fieldAccess.fieldIdentifier.sortBy(x => (x.lineNumber, x.columnNumber)).l
    val thisRefs         = Seq("this", "self") ++ m.typeDecl.name.headOption.toList
    val fieldAccessUsages = fieldIdentifiers.isFieldIdentifier
      .canonicalNameExact(name)
      .inFieldAccess
      .where(_.argument(1).codeExact(thisRefs*))
      .takeWhile(notLeftHandOfAssignment)
      .l
    (identifierUsages ++ fieldAccessUsages).sortBy(x => (x.lineNumber, x.columnNumber)).headOption.toList
  }

  /** For a literal, determine if it is used in the initialization of any member variables. Return list of initialized
    * members. An initialized member is either an identifier or a field-identifier.
    */
  private def literalToInitializedMembers(lit: Literal): List[CfgNode] =
    lit.inAssignment
      .or(
        _.method.nameExact(Defines.StaticInitMethodName, Defines.ConstructorMethodName, "__init__"),
        // in language such as Python, where assignments for members can be directly under a type decl
        _.method.typeDecl
      )
      .target
      .flatMap {
        case identifier: Identifier
            // If these are the same, then the parent method is the module-level type
            if identifier.method.typeDecl.fullName.contains(identifier.method.fullName) ||
              // If a member shares the name of the identifier then we consider this as a member
              lit.method.typeDecl.member.name.toSet.contains(identifier.name) =>
          identifier :: Nil
        case call: Call if isFieldAccess(call.name) => call.ast.isFieldIdentifier.l
        case _                                      => Nil
      }
      .l

  private def methodsRecursively(typeDecl: TypeDecl): List[Method] = {
    def methods(x: AstNode): List[Method] = {
      x match {
        case m: Method => m :: m.astMinusRoot.isMethod.flatMap(methods).l
        case _         => Nil
      }
    }
    typeDecl.method.flatMap(methods).l
  }

  private def isTargetInAssignment(identifier: Identifier): List[Identifier] = {
    identifier.start.argumentIndex(1).where(_.inAssignment).l
  }

  protected def notLeftHandOfAssignment(x: Expression): Boolean = {
    !(x.argumentIndex == 1 && x.inCall.exists(y => allAssignmentTypes.contains(y.name)))
  }

  private def targetsToClassIdentifierPair(targets: List[AstNode], src: StoredNode): List[UsageInput] = {
    targets.flatMap {
      case expr: Expression =>
        expr.method.typeDecl.map { typeDecl => UsageInput(src, typeDecl, expr) }
      case member: Member =>
        member.typeDecl.map { typeDecl => UsageInput(src, typeDecl, member) }
    }
  }

}
