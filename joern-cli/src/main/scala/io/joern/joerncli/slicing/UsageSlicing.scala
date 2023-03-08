package io.joern.joerncli.slicing

import io.joern.joerncli.JoernSlice.Config
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.{Operators, PropertyNames}
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.Traversal

import java.util.regex.Pattern
import scala.collection.parallel.CollectionConverters.ImmutableIterableIsParallelizable

object UsageSlicing {

  private val resolver               = NoResolve
  private val constructorTypeMatcher = Pattern.compile(".*new (\\w+)\\(.*")

  /** Generates object slices from the given CPG.
    *
    * @param cpg
    *   the CPG to slice.
    * @return
    *   a set of object slices.
    */
  def calculateUsageSlice(cpg: Cpg, config: Config): ProgramSlice = {
    def getAssignmentDecl: Traversal[Declaration] = (config.sourceFile match {
      case Some(fileName) => cpg.file.nameExact(fileName).assignment
      case None           => cpg.assignment
    }).argument(1).isIdentifier.refsTo

    def getParameterDecl: Traversal[MethodParameterIn] = config.sourceFile match {
      case Some(fileName) => cpg.file.nameExact(fileName).ast.isParameter
      case None           => cpg.parameter
    }

    def getDeclIdentifiers: Traversal[Declaration] = getAssignmentDecl ++ getParameterDecl

    def typeMap = cpg.typeDecl.map(f => (f.name, f.fullName)).toMap

    ProgramUsageSlice(
      getDeclIdentifiers
        .to(LazyList)
        .par
        .filter(a => atLeastNCalls(a, config.minNumCalls) && !a.name.startsWith("_tmp_"))
        .flatMap(a => trackUsage(a, typeMap))
        .groupBy { case (scope, _) => scope }
        .mapValues(_.l.map { case (_, slice) => slice }.toSet)
        .toMap
        .l
        .toMap,
      userDefinedTypes(cpg)
    )
  }

  private def getInCallsForReferencedIdentifiers(decl: Declaration): List[Call] = {
    // Cross closure boundaries
    val capturedVars = decl.capturedByMethodRef.referencedMethod.ast.isIdentifier.nameExact(decl.name)
    decl
      .flatMap {
        case local: Local             => local.referencingIdentifiers ++ capturedVars
        case param: MethodParameterIn => param.referencingIdentifiers ++ capturedVars
        case _                        => Seq()
      }
      .inCall
      .flatMap {
        case c if c.name.equals(Operators.assignment) && c.ast.isCall.name(Operators.alloc).nonEmpty => Some(c)
        case c if !c.name.startsWith("<operator>")                                                   => Some(c)
        case _                                                                                       => None
      }
      .dedup
      .toList
  }

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
    getInCallsForReferencedIdentifiers(decl).size >= n

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
    cpg.typeDecl
      .filterNot(t => t.isExternal || t.name.matches("(:program|<module>|<init>|<meta>|<body>)"))
      .map(generateUDT)
      .filter(udt => udt.fields.nonEmpty || udt.procedures.nonEmpty)
      .l
  }

  /** Given a node, if it is a call, will traverse the AST parent's children for a node with argument index 0.
    *
    * @param node
    *   the node, which should be a call
    * @return
    *   the call receiver, if found.
    */
  private def getCallReceiver(node: Call): Option[Identifier] =
    node match {
      case x: Call if Operators.fieldAccess.equals(x.name) => x.inCall.argument.isIdentifier.find(_.argumentIndex == 0)
      case x: Call if Operators.alloc.equals(x.name) && x.inCall.ast.isIdentifier.name(".*tmp.*").nonEmpty =>
        x.parentBlock.inAssignment.argument.isIdentifier.find(_.argumentIndex == 0)
      case x: Call if Operators.alloc.equals(x.name) =>
        x.inCall.argument.isIdentifier.find(_.argumentIndex == 0)
      case x: Call if Operators.assignment.equals(x.name) && x.ast.isCall.name.exists(_.equals(Operators.alloc)) =>
        x.argument.isIdentifier.find(_.argumentIndex == 1)
      case x: Call => x.argument.isIdentifier.find(_.argumentIndex == 0)
      case _       => None
    }

  private def trackUsage(tgt: Declaration, typeMap: Map[String, String]): Option[(String, ObjectUsageSlice)] = {

    /** Will attempt to get the API call from the expression if this is a procedure call.
      *
      * @param baseCall
      *   the expression to extract the API call from.
      * @return
      *   an API call if present.
      */
    def exprToObservedCall(baseCall: Call): Option[ObservedCall] = {
      val isMemberInvocation = baseCall.name.equals(Operators.fieldAccess)
      val isConstructor =
        baseCall.name.equals(Operators.alloc) || baseCall.ast.isCall.nameExact(Operators.alloc).nonEmpty
      // Handle the case where a call is an invocation of a field member (lambda) or function/method call
      val callName: Option[String] =
        if (isMemberInvocation)
          baseCall.argumentOut.flatMap {
            case x: FieldIdentifier => Option(x.code)
            case x: Call            => Option(x.name)
            case _                  => None
          }.headOption
        else if (isConstructor) {
          val m = constructorTypeMatcher.matcher(baseCall.code)
          if (m.find()) Option(m.group(1))
          else Option(baseCall.code.stripPrefix("new ").takeWhile(!_.equals('(')))
        } else
          Option(baseCall.name)

      if (callName.isEmpty) return None

      val receiverName = getCallReceiver(baseCall).map(_.name)

      val params = (if (isMemberInvocation) baseCall.inCall.argument
                    else if (isConstructor)
                      baseCall.ast.isCall
                        .nameExact("<operator>.new")
                        .lastOption
                        .map(_.argument)
                        .getOrElse(Traversal.empty)
                    else baseCall.argument)
        .collect { case n: Expression if n.argumentIndex > 0 => n }
        .flatMap {
          case _: MethodRef => Option("LAMBDA")
          case x =>
            Option(
              x.property(
                PropertyNames.TYPE_FULL_NAME,
                x.property(PropertyNames.DYNAMIC_TYPE_HINT_FULL_NAME, Seq("ANY")).headOption
              )
            )
        }
        .collect { case x: String => x }
        .toList
      // Not sure how we can get the return type unless it's typescript or we can resolve the callee?
      val returnType = baseCall.argumentOut
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
        .sortBy(f => (f.lineNumber, f.columnNumber))
        .flatMap(c => c.argument.find(p => p.code.equals(tgt.name)).map(x => (c, x.argumentIndex)))
        .partition { case (_, argIdx) => argIdx == 0 }
      (
        invokedCalls.map(_._1).isCall.flatMap(exprToObservedCall).toList,
        argToCalls.flatMap { case (c: Call, argAt: Int) =>
          exprToObservedCall(c).map(oc => (oc, argAt))
        }
      )
    }

    val defNode = tgt match {
      case local: Local =>
        local.referencingIdentifiers.inCall.astParent.assignment
          .where(_.argument(1).code(tgt.name))
          .argument(2)
          .headOption match {
          // In the case of a constructor, we should get the "new" call
          case Some(block: Block) =>
            block.ast.isCall.nameExact("<operator>.new").lastOption
          case x => x
        }
      case x => Some(x)
    }

    /** Creates a def component with the workaround of correcting the type full name if it is only a type name.
      */
    def createDefComponent(node: StoredNode) = {
      val df = DefComponent.fromNode(node)
      df.copy(typeFullName = typeMap.getOrElse(df.typeFullName, df.typeFullName))
    }

    (tgt, defNode, partitionInvolvementInCalls) match {
      // Case 1: Generated by variable assignment
      case (local: Local, Some(genCall: Call), (invokedCalls, argToCalls))
          if !genCall.name.matches("(require|import)") =>
        Option(
          (
            local.method.fullName.head,
            ObjectUsageSlice(
              targetObj = createDefComponent(local),
              definedBy = Option(createDefComponent(genCall)),
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
              targetObj = createDefComponent(param),
              definedBy = Option(createDefComponent(param)),
              invokedCalls = invokedCalls,
              argToCalls = argToCalls
            )
          )
        )
      case _ => None
    }
  }

}
