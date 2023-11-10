package io.joern.swiftsrc2cpg.astcreation

import io.joern.swiftsrc2cpg.parser.SwiftNodeSyntax.SwiftToken
import io.joern.x2cpg.Ast
import io.joern.x2cpg.ValidationMode

trait AstForSwiftTokenCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  protected def astForSwiftToken(swiftToken: SwiftToken): Ast = Ast() // Swift token are never an element of the CPG AST

}
