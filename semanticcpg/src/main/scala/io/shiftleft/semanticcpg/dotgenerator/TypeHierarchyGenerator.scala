package io.shiftleft.semanticcpg.dotgenerator

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{StoredNode, Type, TypeDecl}
import io.shiftleft.semanticcpg.dotgenerator.DotSerializer.{Edge, Graph}
import io.shiftleft.semanticcpg.language._

import scala.collection.mutable

class TypeHierarchyGenerator {

  def generate(cpg: Cpg): Graph = {
    val subgraph         = mutable.HashMap.empty[String, Seq[StoredNode]]
    val vertices         = cpg.typeDecl.l
    val typeToIsExternal = vertices.map { t => t.fullName -> t.isExternal }.toMap
    val edges = for {
      srcTypeDecl <- vertices
      srcType     <- srcTypeDecl._typeViaRefIn.l
      _ = storeInSubgraph(srcType, subgraph, typeToIsExternal)
      tgtType <- srcTypeDecl.inheritsFromOut
    } yield {
      storeInSubgraph(tgtType, subgraph, typeToIsExternal)
      Edge(tgtType, srcType)
    }
    Graph(vertices.flatMap(_._typeViaRefIn.l), edges.distinct, subgraph.toMap)
  }

  def storeInSubgraph(
    typ: Type,
    subgraph: mutable.Map[String, Seq[StoredNode]],
    typeToIsExternal: Map[String, Boolean]
  ): Unit = {
    if (!typeToIsExternal(typ.fullName)) {
      /*
        We parse the namespace information instead of looking at the namespace node
        as types such as inner classes may not be attached to a namespace block
       */
      val namespace =
        if (typ.fullName.contains("."))
          typ.fullName.stripSuffix(s".${typ.name}")
        else
          typ.fullName.stripSuffix(s"${typ.name}")
      subgraph.put(namespace, subgraph.getOrElse(namespace, Seq()) ++ Seq(typ))
    } else {
      subgraph.put("<global>", subgraph.getOrElse("<global>", Seq()) ++ Seq(typ))
    }
  }

}
