package io.shiftleft.semanticcpg.language.importresolver

import io.shiftleft.codepropertygraph.generated.nodes.Tag

import scala.language.implicitConversions

trait Implicits {

  implicit def toEvaluatedImportAsTagExt(node: Tag): ResolvedImportAsTagExt = new ResolvedImportAsTagExt(node)

  implicit def toEvaluatedImportAsTagTraversal(steps: IterableOnce[Tag]): ResolvedImportAsTagTraversal =
    new ResolvedImportAsTagTraversal(steps.iterator)

}
