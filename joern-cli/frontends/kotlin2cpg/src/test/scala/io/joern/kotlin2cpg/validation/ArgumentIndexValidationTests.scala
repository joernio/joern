package io.joern.kotlin2cpg.validation

import io.joern.kotlin2cpg.TestContext
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.edges.Ast
import io.shiftleft.codepropertygraph.generated.nodes.{Call, ClosureBinding, FieldIdentifier, Identifier, Literal}
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.jIteratortoTraversal

class ArgumentIndexValidationTests extends AnyFreeSpec with Matchers {
}
