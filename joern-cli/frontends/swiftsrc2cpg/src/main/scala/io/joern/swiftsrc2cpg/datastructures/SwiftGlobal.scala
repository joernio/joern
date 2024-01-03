package io.joern.swiftsrc2cpg.datastructures

import io.joern.x2cpg.datastructures.Global
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.nodes.NewBlock
import io.shiftleft.codepropertygraph.generated.nodes.NewTypeDecl

import java.util.concurrent.ConcurrentHashMap

class SwiftGlobal extends Global {

  case class ConstructorBlocks(constructorBlock: Ast, staticConstructorBlock: Ast)

  // for Swift extension handling; see:
  // https://docs.swift.org/swift-book/documentation/the-swift-programming-language/extensions
  val seenTypeDecls: ConcurrentHashMap[NewTypeDecl, ConstructorBlocks] = new ConcurrentHashMap()

}
