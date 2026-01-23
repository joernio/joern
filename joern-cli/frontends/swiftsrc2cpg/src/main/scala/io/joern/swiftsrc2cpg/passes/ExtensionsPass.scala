package io.joern.swiftsrc2cpg.passes

import io.joern.swiftsrc2cpg.astcreation.SwiftSrcGlobal
import io.shiftleft.codepropertygraph.generated.{Cpg, DispatchTypes, EdgeTypes, PropertyNames}
import io.shiftleft.codepropertygraph.generated.nodes.{Expression, NewMember, TypeDecl}
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language.*

/** Applies information discovered from Swift extensions to the CPG.
  *
  * This pass augments existing type declarations with extension-defined members, rewrites extension-method-style calls
  * to static dispatch, and updates inheritance information collected from extensions.
  *
  * @param cpg
  *   The CPG to update.
  * @param extensionMembers
  *   Map from extension target type `fullName` to members discovered in extensions.
  * @param extensionFullNameMapping
  *   Map from original call `methodFullName` to the corresponding extension method `fullName`.
  * @param memberPropertyMapping
  *   Map from derived member fullName candidates (e.g., `T.m` / `T.m.getter`) to computed-property getter
  *   `methodFullName`s.
  * @param inheritsMapping
  *   Map from type declaration `fullName` to inherited type names/fullNames discovered via extensions.
  */
class ExtensionsPass(
  cpg: Cpg,
  extensionMembers: Map[String, List[SwiftSrcGlobal.MemberInfo]],
  extensionFullNameMapping: Map[String, String],
  memberPropertyMapping: Map[String, String],
  inheritsMapping: Map[String, Set[String]]
) extends CpgPass(cpg) {

  override def run(diffGraph: DiffGraphBuilder): Unit = {
    handleExtensionMembers(diffGraph)
    handleExtensionCalls(diffGraph)
    handleMemberPropertyGetterCalls(diffGraph)
    handleInherits(diffGraph)
  }

  /** Creates a pass that rewrites computed-property member accesses to use the corresponding setter calls. We need to
    * separate this from the main pass as setter handling needs to happen after getter handling. (For the case where a
    * setter is called on the result of a getter, i.e. `foo.bar.baz = ...`.)
    *
    * @return
    *   A pass that rewrites computed-property member accesses to use the corresponding setter calls.
    */
  def setters: ExtensionsPass =
    new ExtensionsPass(cpg, extensionMembers, extensionFullNameMapping, memberPropertyMapping, inheritsMapping) {
      override def run(diffGraph: DiffGraphBuilder): Unit = {
        handleMemberPropertySetterCalls(diffGraph)
      }
    }

  /** Lookup a `TypeDecl` by its expected `fullName`, falling back to a best-effort simple-name lookup.
    *
    * The fallback exists for cases where type `fullName`s are not fully accurate due to missing compiler support.
    */
  private def findTypeDecl(fullName: String): Option[TypeDecl] = {
    // If we cannot find the typeDecl by fullName, try looking it up by name (last part of fullName).
    // This is a fallback / the best effort approach for cases where the fullName is not accurate due to missing compiler support.
    cpg.typeDecl.fullNameExact(fullName).headOption.orElse {
      val simpleName = fullName.split('.').last
      cpg.typeDecl.nameExact(simpleName).headOption
    }
  }

  /** Create `MEMBER` nodes for the given extension members and attach them to the provided type declaration. */
  private def addMembers(
    diffGraph: DiffGraphBuilder,
    decl: TypeDecl,
    members: List[SwiftSrcGlobal.MemberInfo]
  ): Unit = {
    members.foreach { member =>
      val memberNode = NewMember()
        .name(member.name)
        .code(member.code)
        .typeFullName(member.typeFullName)
      diffGraph.addNode(memberNode)
      diffGraph.addEdge(decl, memberNode, EdgeTypes.AST)
    }
  }

  /** Adds all members discovered from Swift extensions (as computed properties) to their respective type declarations
    * in the CPG.
    */
  private def handleExtensionMembers(diffGraph: DiffGraphBuilder): Unit = {
    extensionMembers.foreach { case (fullName, members) =>
      if (members.nonEmpty) {
        findTypeDecl(fullName).foreach { typeDecl =>
          addMembers(diffGraph, typeDecl, members)
        }
      }
    }
  }

  /** Resolves extension-method-style calls and rewrites them to use static dispatch to the corresponding extension
    * method fullName.
    *
    * The functions scans call nodes in the provided CPG and for each call methodFullName present in
    * `extensionFullNameMapping` it:
    *   - updates the call's dispatch type to STATIC_DISPATCH
    *   - replaces the call's methodFullName with the mapped extension method full name
    *   - removes the receiver edge from the call node
    */
  private def handleExtensionCalls(diffGraph: DiffGraphBuilder): Unit = {
    val callCandidates = cpg.call
      .methodFullNameNot(io.joern.x2cpg.Defines.DynamicCallUnknownFullName)
      .filterNot(_.methodFullName.startsWith("<operator"))

    callCandidates.foreach { call =>
      extensionFullNameMapping.get(call.methodFullName).foreach { methodFullNameExt =>
        diffGraph.setNodeProperty(call, PropertyNames.DispatchType, DispatchTypes.STATIC_DISPATCH)
        diffGraph.setNodeProperty(call, PropertyNames.MethodFullName, methodFullNameExt)
        call.outE(EdgeTypes.RECEIVER).foreach(diffGraph.removeEdge)
      }
    }
  }

  /** Extract an expression's `TYPE_FULL_NAME` property, handling both boxed and unboxed property encodings. We cannot
    * use `.evalType` as the `TypeEvalPass` did not run yet.
    */
  private def typeFullNameOf(node: Expression): Option[String] = {
    node.properties.get(PropertyNames.TypeFullName) match {
      case Some(Some(tpe: String)) => Some(tpe)
      case Some(tpe: String)       => Some(tpe)
      case other                   => None
    }
  }

  /** Rewrites computed property member accesses to the corresponding getter call when resolvable.
    *
    * For each `fieldAccess` call node:
    *   - reads the base expression's `TYPE_FULL_NAME` and the member name from the access arguments
    *   - derives candidate member fullNames (e.g., `T.m` and `T.m.getter`)
    *   - if a match exists in `memberPropertyMapping`, updates the access to reference the getter by setting:
    *     - `NAME` and `METHOD_FULL_NAME` to the resolved getter
    *     - `DISPATCH_TYPE` to `DYNAMIC_DISPATCH`
    *     - ensures a `RECEIVER` edge from the access to the base expression and fixes argument indices/orders if its
    *       not from an extension
    */
  private def handleMemberPropertyGetterCalls(diffGraph: DiffGraphBuilder): Unit = {
    for {
      fieldAccess <- cpg.fieldAccess
      if !fieldAccess.astIn.isCall.isAssignment.target.contains(fieldAccess)
      baseNode            <- fieldAccess.arguments(1).headOption
      fieldIdentifierNode <- fieldAccess.arguments(2).headOption
      baseFullName        <- typeFullNameOf(baseNode)
    } {
      val memberName = fieldIdentifierNode.code
      Seq(s"$memberName.getter", memberName).foreach { memberName =>
        val memberFullName = s"$baseFullName.$memberName"
        memberPropertyMapping.get(memberFullName).foreach { propertyFullName =>
          diffGraph.setNodeProperty(fieldAccess, PropertyNames.Name, memberName)
          diffGraph.setNodeProperty(fieldAccess, PropertyNames.MethodFullName, propertyFullName)
          diffGraph.setNodeProperty(baseNode, PropertyNames.ArgumentIndex, 0)
          diffGraph.setNodeProperty(baseNode, PropertyNames.Order, 1)

          if (!propertyFullName.contains("<extension>")) {
            // Ensure receiver edge from fieldAccess to baseNode only if the access is not from an extension.
            diffGraph.setNodeProperty(fieldAccess, PropertyNames.DispatchType, DispatchTypes.DYNAMIC_DISPATCH)
            diffGraph.addEdge(fieldAccess, baseNode, EdgeTypes.RECEIVER)
          }

          diffGraph.removeNode(fieldIdentifierNode)
        }
      }
    }
  }

  /** Rewrites computed-property setter accesses (assignments to a property) to the corresponding setter call if
    * resolvable.
    *
    * For each assignment call node:
    *   - identifies the `fieldAccess` on the assignment target and extracts the base expression and member identifier
    *   - derives the setter member fullName candidate as `T.m.setter`
    *   - if a match exists in `memberPropertyMapping`, updates the assignment call to reference the setter by setting:
    *     - `NAME` and `METHOD_FULL_NAME` to the resolved setter
    *     - `DISPATCH_TYPE` to `DYNAMIC_DISPATCH` (only when not an extension)
    *     - fixes argument indices/orders and ensures `RECEIVER`, `AST`, and `ARGUMENT` edges are consistent
    *     - removes intermediate `fieldAccess` and member identifier nodes/edges that are no longer needed
    */
  private def handleMemberPropertySetterCalls(diffGraph: DiffGraphBuilder): Unit = {
    for {
      assignmentCall      <- cpg.assignment
      sourceNode          <- assignmentCall.source
      fieldAccess         <- assignmentCall.target.fieldAccess
      baseNode            <- fieldAccess.arguments(1).headOption
      fieldIdentifierNode <- fieldAccess.arguments(2).headOption
      baseFullName        <- typeFullNameOf(baseNode)
    } {
      val memberName     = s"${fieldIdentifierNode.code}.setter"
      val memberFullName = s"$baseFullName.$memberName"
      memberPropertyMapping.get(memberFullName).foreach { propertyFullName =>
        diffGraph.setNodeProperty(assignmentCall, PropertyNames.Name, memberName)
        diffGraph.setNodeProperty(assignmentCall, PropertyNames.MethodFullName, propertyFullName)
        diffGraph.setNodeProperty(baseNode, PropertyNames.ArgumentIndex, 0)
        diffGraph.setNodeProperty(baseNode, PropertyNames.Order, 1)

        if (!propertyFullName.contains("<extension>")) {
          // Ensure receiver edge from fieldAccess to baseNode only if the access is not from an extension.
          diffGraph.setNodeProperty(assignmentCall, PropertyNames.DispatchType, DispatchTypes.DYNAMIC_DISPATCH)
          diffGraph.addEdge(assignmentCall, baseNode, EdgeTypes.RECEIVER)
        }

        diffGraph.addEdge(assignmentCall, baseNode, EdgeTypes.AST)
        diffGraph.addEdge(assignmentCall, baseNode, EdgeTypes.ARGUMENT)
        diffGraph.setNodeProperty(sourceNode, PropertyNames.ArgumentIndex, 1)
        diffGraph.setNodeProperty(sourceNode, PropertyNames.Order, 2)

        fieldAccess.outE(EdgeTypes.AST).foreach(diffGraph.removeEdge)
        fieldAccess.outE(EdgeTypes.ARGUMENT).foreach(diffGraph.removeEdge)
        diffGraph.removeNode(fieldAccess)
        diffGraph.removeNode(fieldIdentifierNode)
      }
    }
  }

  /** Applies inheritance relationships discovered from Swift extensions to type declarations in the CPG. */
  private def handleInherits(diffGraph: DiffGraphBuilder): Unit = {
    inheritsMapping.foreach { case (typeDeclFullName, inheritsFullNames) =>
      if (inheritsFullNames.nonEmpty) {

        /** Resolve extension target names to TypeDecl fullNames if a matching `TypeDecl` exists.
          *
          * For each name in the set of extension target type names we look up a matching `TypeDecl` using `nameExact`.
          * If found, use its `fullName`; otherwise keep the original name. This ensures we add consistent fullNames to
          * the `inheritsFrom` property and create `INHERITS_FROM` edges to the correct `typ` nodes.
          *
          * The value is `lazy` to avoid performing lookups when the `for` loop below does not find a `TypeDecl` for
          * `typeDeclFullName`.
          */
        lazy val inheritsFullNamesResolved = inheritsFullNames.map { name =>
          cpg.typeDecl.nameExact(name).loneElementOption match {
            case Some(td) => td.fullName
            case None     => name
          }
        }

        for {
          typeDecl <- cpg.typeDecl.fullNameExact(typeDeclFullName)
        } setInherits(diffGraph, typeDecl, inheritsFullNamesResolved)
      }
    }
  }

  /** Merge the provided inherited type fullNames with existing ones and set them on the type declaration. */
  private def setInherits(diffGraph: DiffGraphBuilder, typeDecl: TypeDecl, inheritFullNames: Set[String]): Unit = {
    val existingInherits = typeDecl.inheritsFromTypeFullName
    val allInherits      = (existingInherits ++ inheritFullNames).distinct.sorted
    diffGraph.setNodeProperty(typeDecl, PropertyNames.InheritsFromTypeFullName, allInherits)
  }

}
