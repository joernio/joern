package io.joern.console

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.StoredNode

case class CodeSnippet(content: String, filename: String)
case class MultiFileCodeExamples(positive: List[List[CodeSnippet]], negative: List[List[CodeSnippet]])
case class CodeExamples(positive: List[String], negative: List[String])

case class Query(
  name: String,
  author: String,
  title: String,
  description: String,
  score: Double,
  traversal: Cpg => Iterator[? <: StoredNode],
  traversalAsString: String = "",
  tags: List[String] = List(),
  language: String = "",
  codeExamples: CodeExamples = CodeExamples(List(), List()),
  multiFileCodeExamples: MultiFileCodeExamples = MultiFileCodeExamples(List(), List())
)

object Query {
  def make(
    name: String,
    author: String,
    title: String,
    description: String,
    score: Double,
    traversalWithStrRep: TraversalWithStrRep,
    tags: List[String] = List(),
    codeExamples: CodeExamples = CodeExamples(List(), List()),
    multiFileCodeExamples: MultiFileCodeExamples = MultiFileCodeExamples(List(), List())
  ): Query = {
    Query(
      name = name,
      author = author,
      title = title,
      description = description,
      score = score,
      traversal = traversalWithStrRep.traversal,
      traversalAsString = traversalWithStrRep.strRep,
      tags = tags,
      codeExamples = codeExamples,
      multiFileCodeExamples = multiFileCodeExamples
    )
  }
}

case class TraversalWithStrRep(traversal: Cpg => Iterator[? <: StoredNode], strRep: String = "")
