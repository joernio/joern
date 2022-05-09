package io.joern.formats

import io.shiftleft.codepropertygraph.generated.{edges, nodes}

object ImporterMain extends App {

  val importerMain = overflowdb.formats.ImporterMain(nodes.Factories.all, edges.Factories.all)
  importerMain(args)
}
