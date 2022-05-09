package io.joern.formats

import io.shiftleft.codepropertygraph.generated.{edges, nodes}
import io.shiftleft.codepropertygraph.generated.Cpg.convertPropertyForPersistence

object ImporterMain extends App {

  val importerMain = overflowdb.formats.ImporterMain(
    nodes.Factories.all,
    edges.Factories.all,
    convertPropertyForPersistence(_)
  )
  importerMain(args)
}
