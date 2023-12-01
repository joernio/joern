package io.shiftleft.semanticcpg.language

import io.shiftleft.codepropertygraph.generated.nodes.{CallRepr, Method}

import scala.collection.mutable
import scala.jdk.CollectionConverters.*

trait ICallResolver {

  def getUnresolvedMethodFullNames(callsite: CallRepr): Iterable[String] = {
    triggerCallsiteResolution(callsite)
    getUnresolvedMethodFullNamesInternal(callsite)
  }

  def getUnresolvedMethodFullNamesInternal(callsite: CallRepr): Iterable[String]

  /** Get methods called at the given callsite. This internally calls triggerCallsiteResolution.
    */
  def getCalledMethods(callsite: CallRepr): Iterable[Method] = {
    triggerCallsiteResolution(callsite)
    val combined = mutable.ArrayBuffer.empty[Method]
    callsite._callOut.foreach(method => combined.append(method.asInstanceOf[Method]))
    combined.appendAll(getResolvedCalledMethods(callsite))

    combined
  }

  /** Same as getCalledMethods but with traversal return type.
    */
  def getCalledMethodsAsTraversal(callsite: CallRepr): Iterator[Method] =
    getCalledMethods(callsite).iterator

  /** Get callsites of the given method. This internally calls triggerMethodResolution.
    */
  def getMethodCallsites(method: Method): Iterable[CallRepr] = {
    triggerMethodCallsiteResolution(method)
    // The same call sites of a method can be found via static and dynamic lookup.
    // This is for example the case for Java virtual call sites which are statically assert
    // a certain method which could be overriden. If we are looking for the call sites of
    // such a statically asserted method, we find it twice and thus deduplicate here.
    val combined = mutable.LinkedHashSet.empty[CallRepr]
    method._callIn.foreach(call => combined.add(call.asInstanceOf[CallRepr]))
    combined.addAll(getResolvedMethodCallsites(method))

    combined.toBuffer
  }

  /** Same as getMethodCallsites but with traversal return type.
    */
  def getMethodCallsitesAsTraversal(method: Method): Iterator[CallRepr] =
    getMethodCallsites(method).iterator

  /** Starts data flow tracking to find all method which could be called at the given callsite. The result is stored in
    * the resolver internal cache.
    */
  def triggerCallsiteResolution(callsite: CallRepr): Unit

  /** Starts data flow tracking to find all callsites which could call the given method. The result is stored in the
    * resolver internal cache.
    */
  def triggerMethodCallsiteResolution(method: Method): Unit

  /** Retrieve results of triggerCallsiteResolution.
    */
  def getResolvedCalledMethods(callsite: CallRepr): Iterable[Method]

  /** Retrieve results of triggerMethodResolution.
    */
  def getResolvedMethodCallsites(method: Method): Iterable[CallRepr]
}

object NoResolve extends ICallResolver {
  def triggerCallsiteResolution(callsite: CallRepr): Unit = {}

  def triggerMethodCallsiteResolution(method: Method): Unit = {}

  override def getResolvedCalledMethods(callsite: CallRepr): Iterable[Method] = {
    Iterable.empty
  }

  override def getResolvedMethodCallsites(method: Method): Iterable[CallRepr] = {
    Iterable.empty
  }

  override def getUnresolvedMethodFullNamesInternal(callsite: CallRepr): Iterable[String] = {
    Iterable.empty
  }
}
