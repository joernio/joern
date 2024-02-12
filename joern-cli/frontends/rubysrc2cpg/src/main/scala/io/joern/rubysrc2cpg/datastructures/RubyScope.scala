package io.joern.rubysrc2cpg.datastructures

import io.joern.rubysrc2cpg.datastructures.{RubyField, RubyMethod, RubyProgramSummary, RubyType}
import io.joern.x2cpg.datastructures.{Scope, TypedScope, TypedScopeElement}
import io.shiftleft.codepropertygraph.generated.nodes.NewNode

class RubyScope(summary: RubyProgramSummary)
    extends Scope[String, NewNode, TypedScopeElement]
    with TypedScope[RubyMethod, RubyField, RubyType](summary)
