package io.joern.x2cpg.passes.frontend

import io.joern.x2cpg.passes.base.MethodStubCreator
import io.joern.x2cpg.passes.frontend.XTypeRecovery.isDummyType
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, NodeTypes, PropertyNames}
import io.shiftleft.passes.CpgPass
import io.shiftleft.proto.cpg.Cpg.DispatchTypes
import io.shiftleft.semanticcpg.language.*

import java.util.regex.Pattern
import scala.collection.mutable

/** Attempts to set the <code>methodFullName</code> and link to callees using the recovered type information from
  * [[XTypeRecovery]]. Note that some methods may not be present as they could be external and have been dynamically
  * discovered, thus the [[io.joern.x2cpg.passes.base.MethodStubCreator]] would have missed it.
  *
  * @param cpg
  *   the target code property graph.
  */
abstract class XTypeHintCallLinker(cpg: Cpg) extends CpgPass(cpg) {

  implicit protected val resolver: NoResolve.type = NoResolve
  private val fileNamePattern                     = Pattern.compile("^(.*(.py|.js|.rb)).*$")
  protected val pathSep: String                   = "."

  protected def calls: Iterator[Call] = cpg.call
    .nameNot("<operator>.*", "<operators>.*")
    .filter(c => calleeNames(c).nonEmpty && c.callee.isEmpty)

  protected def calleeNames(c: Call): Seq[String] =
    c.dynamicTypeHintFullName.filterNot(_.equals("ANY")).distinct

  protected def callees(names: Seq[String]): List[Method] = cpg.method.fullNameExact(names*).toList

  override def run(builder: DiffGraphBuilder): Unit = linkCalls(builder)

  protected def linkCalls(builder: DiffGraphBuilder): Unit = {
    val callerAndCallees = calls.map(call => (call, calleeNames(call))).toList
    val methodMap        = mapMethods(callerAndCallees, builder)
    linkCallsToCallees(callerAndCallees, methodMap, builder)
    linkSpeculativeNamespaceNodes(methodMap.view.values.collectAll[NewMethod], builder)
  }

  protected def mapMethods(
    callerAndCallees: List[(Call, Seq[String])],
    builder: DiffGraphBuilder
  ): Map[String, MethodBase] = {
    val methodMap  = mutable.HashMap.empty[String, MethodBase]
    val newMethods = mutable.HashMap.empty[String, NewMethod]
    callerAndCallees.foreach { case (call, methodNames) =>
      val ms = callees(methodNames)
      if (ms.nonEmpty) {
        ms.foreach { m => methodMap.put(m.fullName, m) }
      } else {
        val mNames = ms.map(_.fullName).toSet
        methodNames
          .filterNot(mNames.contains)
          .map(fullName => newMethods.getOrElseUpdate(fullName, createMethodStub(fullName, call, builder)))
          .foreach { m => methodMap.put(m.fullName, m) }
      }
    }
    methodMap.toMap
  }

  protected def linkCallsToCallees(
    callerAndCallees: List[(Call, Seq[String])],
    methodMap: Map[String, MethodBase],
    builder: DiffGraphBuilder
  ): Unit = {
    // Link edges to method nodes
    callerAndCallees.foreach { case (call, methodNames) =>
      methodNames
        .flatMap(methodMap.get)
        .filter(m => call.callee(NoResolve).fullNameExact(m.fullName).isEmpty)
        .foreach { m => linkCallToCallee(call, m, builder) }
      setCallees(call, methodNames, builder)
    }
  }

  def linkCallToCallee(call: Call, method: MethodBase, builder: DiffGraphBuilder): Unit = {
    builder.addEdge(call, method, EdgeTypes.CALL)
    method match {
      case m: Method if m.methodReturn.possibleTypes.headOption.exists(_ != "ANY") =>
        val typeFullName = m.methodReturn.possibleTypes.headOption.getOrElse(m.methodReturn.typeFullName)
        builder.setNodeProperty(call, PropertyNames.TYPE_FULL_NAME, typeFullName)
      case m: Method =>
        builder.setNodeProperty(call, PropertyNames.TYPE_FULL_NAME, m.methodReturn.typeFullName)
      case _ =>
    }
  }

  protected def setCallees(call: Call, methodNames: Seq[String], builder: DiffGraphBuilder): Unit = {
    val nonDummyTypes = methodNames.filterNot(isDummyType)
    if (methodNames.sizeIs == 1) {
      builder.setNodeProperty(call, PropertyNames.METHOD_FULL_NAME, methodNames.head)
      builder.setNodeProperty(
        call,
        PropertyNames.DYNAMIC_TYPE_HINT_FULL_NAME,
        call.dynamicTypeHintFullName.diff(methodNames)
      )
    } else if (methodNames.sizeIs > 1 && methodNames != nonDummyTypes) {
      setCallees(call, nonDummyTypes, builder)
    }
  }

  protected def createMethodStub(methodName: String, call: Call, builder: DiffGraphBuilder): NewMethod = {
    // In the case of Python/JS we can use name info to check if, despite the method name might be incorrect, that we
    // label the method correctly as internal by finding that the method should belong to an internal file
    val matcher  = fileNamePattern.matcher(methodName)
    val basePath = cpg.metaData.root.head
    val isExternal = if (matcher.matches()) {
      val fileName = matcher.group(1)
      cpg.file.nameExact(s"$basePath$fileName").isEmpty
    } else {
      true
    }
    val name =
      if (methodName.contains(pathSep) && methodName.length > methodName.lastIndexOf(pathSep) + 1)
        methodName.substring(methodName.lastIndexOf(pathSep) + pathSep.length)
      else methodName
    createMethodStub(name, methodName, call.argumentOut.size, isExternal, builder)
  }

  /** Try to extract a type full name from the method full name, if one exists in the CPG then we are lucky and we use
    * it, else we ignore for now.
    */
  protected def createMethodStub(
    name: String,
    fullName: String,
    argSize: Int,
    isExternal: Boolean,
    builder: DiffGraphBuilder
  ): NewMethod = {
    val nameIdx = fullName.lastIndexOf(name)
    val default = (NodeTypes.NAMESPACE_BLOCK, XTypeHintCallLinker.namespace)
    val (astParentType, astParentFullName) =
      if (!fullName.isBlank && !fullName.startsWith("<operator") && nameIdx > 0) {
        cpg.typeDecl
          .fullNameExact(fullName.substring(0, nameIdx - 1))
          .map(t => t.label -> t.fullName)
          .headOption
          .getOrElse(default)
      } else {
        default
      }

    MethodStubCreator
      .createMethodStub(
        name,
        fullName,
        "",
        DispatchTypes.DYNAMIC_DISPATCH.name(),
        argSize,
        builder,
        isExternal,
        astParentType,
        astParentFullName
      )
  }

  /** Once we have connected methods that were speculatively generated and managed to correctly link to methods already
    * in the CPG, we link the rest to the "speculativeMethods" namespace as a way to show that these may not actually
    * exist.
    */
  protected def linkSpeculativeNamespaceNodes(newMethods: IterableOnce[NewMethod], builder: DiffGraphBuilder): Unit = {
    val speculativeNamespace =
      NewNamespaceBlock().name(XTypeHintCallLinker.namespace).fullName(XTypeHintCallLinker.namespace)

    builder.addNode(speculativeNamespace)
    newMethods.iterator
      .filter(_.astParentFullName == XTypeHintCallLinker.namespace)
      .foreach(m => builder.addEdge(speculativeNamespace, m, EdgeTypes.AST))
  }
}

object XTypeHintCallLinker {

  /** The shared namespace for all methods generated from the type recovery that may not exist with this exact full name
    * in reality.
    */
  val namespace: String = "<speculatedMethods>"

}
