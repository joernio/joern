package io.joern.x2cpg.passes.frontend

import io.joern.x2cpg.passes.base.MethodStubCreator
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Method, MethodBase, NewMethod}
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, PropertyNames}
import io.shiftleft.passes.CpgPass
import io.shiftleft.proto.cpg.Cpg.DispatchTypes
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.Traversal

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

  implicit private val resolver: NoResolve.type = NoResolve
  private val fileNamePattern                   = Pattern.compile("^(.*(.py|.js)).*$")
  protected val pathSep: Char                   = '.'

  protected def calls: Traversal[Call] = cpg.call
    .nameNot("<operator>.*", "<operators>.*")
    .filter(c => calleeNames(c).nonEmpty && c.callee.isEmpty)

  protected def calleeNames(c: Call): Seq[String] =
    (c.dynamicTypeHintFullName :+ c.typeFullName).filterNot(_.equals("ANY")).distinct

  protected def callees(names: Seq[String]): List[Method] = cpg.method.fullNameExact(names: _*).toList

  override def run(builder: DiffGraphBuilder): Unit = linkCalls(builder)

  protected def linkCalls(builder: DiffGraphBuilder): Unit = {
    val mycalls = cpg.call.nameNot("<operator>.*", "<operators>.*").l
    val mycalls1 = mycalls.filter(c => {
    val c1 = calleeNames(c).nonEmpty
    val c2 = c.callee.isEmpty
    val c3 = !c.methodFullName.contains("<inserted>")
    c1 && c2 && c3
    })
    val callerAndCallees = mycalls.map(call => (call, calleeNames(call))).toList
    val methodMap        = mapMethods(callerAndCallees, builder)
    linkCallsToCallees(callerAndCallees, methodMap, builder)
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
        .foreach { m => builder.addEdge(call, m, EdgeTypes.CALL) }
      if (methodNames.sizeIs == 1) builder.setNodeProperty(call, PropertyNames.METHOD_FULL_NAME, methodNames.head)
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
        methodName.substring(methodName.lastIndexOf(pathSep) + 1)
      else methodName
    MethodStubCreator
      .createMethodStub(
        name,
        methodName,
        "",
        DispatchTypes.DYNAMIC_DISPATCH.name(),
        call.argumentOut.size,
        builder,
        isExternal
      )
  }

}
