package io.joern.joerncli.slicing

import io.joern.joerncli.JoernSlice.Config
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.{Operators, PropertyNames}
import io.shiftleft.semanticcpg.language._
import overflowdb.PropertyKey

import scala.collection.parallel.CollectionConverters.ImmutableIterableIsParallelizable
import scala.jdk.CollectionConverters.IteratorHasAsScala

object UsageSlicing {

  private val resolver = NoResolve

  /** Generates object slices from the given CPG.
    *
    * @param cpg
    *   the CPG to slice.
    * @return
    *   a set of object slices.
    */
  def calculateUsageSlice(cpg: Cpg, config: Config): ProgramSlice = {
    def getAssignmentDecl = cpg.file(config.sourceFile).assignment.argument(1).isIdentifier.refsTo

    def getParameterDecl = cpg.file(config.sourceFile).ast.isParameter

    def getDeclIdentifiers = getAssignmentDecl ++ getParameterDecl

    ProgramUsageSlice(
      getDeclIdentifiers
        .to(LazyList)
        .par
        .filter(a => atLeastNCalls(a, config.minNumCalls))
        .flatMap(trackUsage)
        .groupBy { case (scope, _) => scope }
        .mapValues(_.l.map { case (_, slice) => slice }.toSet)
        .toMap
        .l
        .toMap,
      userDefinedTypes(cpg)
    )
  }

  private def getInCallsForReferencedIdentifiers(decl: Declaration): List[Call] = decl
    .flatMap {
      case local: Local             => local.referencingIdentifiers
      case param: MethodParameterIn => param.referencingIdentifiers
      case _                        => Seq()
    }
    .inCall
    .dedup
    .toList

  /** Returns true if the given declaration is found to have at least n non-operator calls within its referenced
    * identifiers' scope.
    *
    * @param decl
    *   the declaration to check.
    * @param n
    *   number of calls.
    * @return
    *   true if the call count condition is satisfied.
    */
  private def atLeastNCalls(decl: Declaration, n: Int): Boolean =
    getInCallsForReferencedIdentifiers(decl)
      .filterNot(s => s.name.startsWith("<operator>"))
      .size >= n

  /** Discovers internally defined types.
    *
    * @param cpg
    *   the CPG to query for types.
    * @return
    *   a list of user defined types.
    */
  def userDefinedTypes(cpg: Cpg): List[UserDefinedType] = {

    def generateUDT(typeDecl: TypeDecl): UserDefinedType = {
      UserDefinedType(
        typeDecl.fullName,
        typeDecl.member.map(DefComponent.fromNode).l,
        typeDecl.method
          .map(m => ObservedCall(None, m.name, m.parameter.map(_.typeFullName).toList, m.methodReturn.typeFullName))
          .l
      )
    }

    cpg.typeDecl.filterNot(_.isExternal).map(generateUDT).l
  }

  /** Given a node, if it is a call, will traverse the AST parent's children for a node with argument index 0.
    *
    * @param node
    *   the node, which should be a call
    * @return
    *   the call receiver, if found.
    */
  private def getCallReceiver(node: StoredNode): Option[StoredNode] = {
    val argKey = new PropertyKey[Int](PropertyNames.ARGUMENT_INDEX)
    node match {
      case x: Call if Operators.fieldAccess.equals(x.name) =>
        x.astParent._argumentOut.asScala.find(n => n.property(argKey, -1) == 0)
      case x: Call => x.argumentOut.find(n => n.property(argKey, -1) == 0)
      case _       => None
    }
  }

  private def trackUsage(tgt: Declaration): Option[(String, ObjectUsageSlice)] = {

    /** Will attempt to get the API call from the expression if this is a procedure call.
      *
      * @param expr
      *   the expression to extract the API call from.
      * @return
      *   an API call if present.
      */
    def exprToObservedCall(expr: Expression): Option[ObservedCall] = {
      val nameKey            = new PropertyKey[String](PropertyNames.NAME)
      val isMemberInvocation = expr.property(nameKey, Operators.fieldAccess).equals(Operators.fieldAccess)
      // Handle the case where a call is an invocation of a field member (lambda) or function/method call
      val callName: Option[String] =
        if (isMemberInvocation)
          expr.argumentOut.flatMap {
            case x: FieldIdentifier => Option(x.code)
            case x: Call            => Option(x.name)
            case _                  => None
          }.headOption
        else
          Option(expr.property(nameKey))

      if (callName.isEmpty) return None

      val receiverName = getCallReceiver(expr).flatMap(rec => Option(rec.property(nameKey)))

      val params = (if (isMemberInvocation) expr.astParent.astChildren else expr.argumentOut)
        .collect { case n: AstNode if n.property(PropertyNames.ARGUMENT_INDEX, -1) > 0 => n }
        .flatMap {
          case _: MethodRef => Option("LAMBDA")
          case x: StoredNode =>
            Option(
              x.property(
                PropertyNames.TYPE_FULL_NAME,
                x.property(PropertyNames.DYNAMIC_TYPE_HINT_FULL_NAME, Seq("UNKNOWN")).headOption
              )
            )
        }
        .collect { case x: String => x }
        .toList
      // Not sure how we can get the return type unless it's typescript or we can resolve the callee?
      val returnType = expr.argumentOut
        .flatMap {
          case x: Call => x.callee(resolver).methodReturn.typeFullName.headOption
          case _       => None
        }
        .headOption
        .getOrElse("ANY")

      Option(ObservedCall(receiverName, callName.get, params, returnType))
    }

    def partitionInvolvementInCalls: (List[ObservedCall], List[(ObservedCall, Int)]) = {
      val (invokedCalls, argToCalls) = getInCallsForReferencedIdentifiers(tgt)
        .filterNot(_.name.startsWith("<operator>"))
        .map(c => (c, c.argument.find(p => p.code.equals(tgt.name)).map(_.argumentIndex).getOrElse(-1)))
        .partition { case (_, argIdx) => argIdx == 0 }
      (
        invokedCalls.map(_._1).flatMap((expr: Expression) => exprToObservedCall(expr)).reverse,
        argToCalls.flatMap { case (c: Call, argAt: Int) =>
          exprToObservedCall(c).map(oc => (oc, argAt))
        }.reverse
      )
    }

    val defNode = tgt match {
      case local: Local =>
        local.referencingIdentifiers.inCall.astParent.assignment
          .where(_.argument(1).code(tgt.name))
          .argument(2)
          .headOption
      case x => Some(x)
    }

    (tgt, defNode, partitionInvolvementInCalls) match {
      // Case 1: Generated by variable assignment
      case (local: Local, Some(genCall: Call), (invokedCalls, argToCalls)) if !genCall.name.equals("require") =>
        Option(
          (
            local.method.fullName.head,
            ObjectUsageSlice(
              targetObj = DefComponent.fromNode(local),
              definedBy = Option(DefComponent.fromNode(genCall)),
              invokedCalls = invokedCalls,
              argToCalls = argToCalls
            )
          )
        )
      // Case 2: Generated by incoming parameter
      case (param: MethodParameterIn, _, (invokedCalls, argToCalls)) if !param.name.matches("(this|self)") =>
        Option(
          (
            param.method.fullName,
            ObjectUsageSlice(
              targetObj = DefComponent.fromNode(param),
              definedBy = Option(DefComponent.fromNode(param)),
              invokedCalls = invokedCalls,
              argToCalls = argToCalls
            )
          )
        )
      case _ => None
    }
  }

}
