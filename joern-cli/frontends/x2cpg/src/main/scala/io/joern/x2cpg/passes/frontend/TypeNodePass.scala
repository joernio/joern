package io.joern.x2cpg.passes.frontend

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.NewType
import io.shiftleft.passes.{KeyPool, SimpleCpgPass}

/** Creates a `TYPE` node for each type in `usedTypes`
  */
class TypeNodePass(usedTypes: List[String], cpg: Cpg, keyPool: Option[KeyPool] = None)
    extends SimpleCpgPass(cpg, "types", keyPool) {

  // Lambda typeDecl type names fit the structure
  // `a.b.c.d.ClassName.lambda$method$name:returnType(paramTypes)`
  // so this regex works by greedily matching the package and class names
  // at the start and cutting off the matched group before the signature.
  private val lambdaTypeRegex = raw".*\.(.*):.*\(.*\)".r

  override def run(diffGraph: DiffGraphBuilder): Unit = {

    diffGraph.addNode(
      NewType()
        .name("ANY")
        .fullName("ANY")
        .typeDeclFullName("ANY")
    )

    usedTypes.sorted.foreach { typeName =>
      val shortName = typeName match {
        case lambdaTypeRegex(methodName) => methodName
        case _                           => typeName.split('.').lastOption.getOrElse(typeName)
      }
      val node = NewType()
        .name(shortName)
        .fullName(typeName)
        .typeDeclFullName(typeName)
      diffGraph.addNode(node)
    }
  }
}
