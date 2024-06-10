package io.shiftleft.semanticcpg.language

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{ConfigFile, Literal, Local, Method}

/** Language extensions for android. */
package object android {

  implicit def toNodeTypeStartersFlows(cpg: Cpg): NodeTypeStarters =
    new NodeTypeStarters(cpg)

  implicit def singleToLocalExt[A <: Local](a: A): LocalTraversal =
    new LocalTraversal(Iterator.single(a))

  implicit def iterOnceToLocalExt[A <: Local](a: IterableOnce[A]): LocalTraversal =
    new LocalTraversal(a.iterator)

  implicit def singleToConfigFileExt[A <: ConfigFile](a: A): ConfigFileTraversal =
    new ConfigFileTraversal(Iterator.single(a))

  implicit def iterOnceToConfigFileExt[A <: ConfigFile](a: IterableOnce[A]): ConfigFileTraversal =
    new ConfigFileTraversal(a.iterator)

  implicit def singleToMethodExt[A <: Method](a: A): MethodTraversal =
    new MethodTraversal(Iterator.single(a))

  implicit def iterOnceToMethodExt[A <: Method](a: IterableOnce[A]): MethodTraversal =
    new MethodTraversal(a.iterator)

}
