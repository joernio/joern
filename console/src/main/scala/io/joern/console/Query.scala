package io.shiftleft.console

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.StoredNode
import overflowdb.traversal.Traversal

case class CodeExamples(positive: List[String], negative: List[String])

case class Query(name: String,
                 author: String,
                 title: String,
                 description: String,
                 score: Double,
                 traversal: Cpg => Traversal[StoredNode],
                 traversalAsString: String = "",
                 tags: List[String] = List(),
                 language: String = "",
                 codeExamples: CodeExamples = CodeExamples(List(), List()))

object Query {
  def make(name: String,
           author: String,
           title: String,
           description: String,
           score: Double,
           traversalWithStrRep: TraversalWithStrRep,
           tags: List[String] = List(),
           codeExamples: CodeExamples = CodeExamples(List(), List())): Query = {
    Query(
      name = name,
      author = author,
      title = title,
      description = description,
      score = score,
      traversal = traversalWithStrRep.traversal,
      traversalAsString = traversalWithStrRep.strRep,
      tags = tags,
      codeExamples = codeExamples
    )
  }
}

case class TraversalWithStrRep(traversal: Cpg => Traversal[StoredNode], strRep: String = "")
