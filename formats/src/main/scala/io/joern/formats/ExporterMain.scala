package io.joern.formats

import io.shiftleft.codepropertygraph.generated.{edges, nodes}

object ExporterMain extends App {

  val exporterMain = overflowdb.formats.ExporterMain(nodes.Factories.all, edges.Factories.all)
  exporterMain(args)
}
