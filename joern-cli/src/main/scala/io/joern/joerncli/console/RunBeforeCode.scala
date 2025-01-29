package io.joern.joerncli.console

import io.joern.console.{Help, Run}

object RunBeforeCode {

  val shared: Seq[String] =
    Seq(
      "import _root_.io.joern.console.*",
      "import _root_.io.joern.joerncli.console.JoernConsole.*",
      "import _root_.io.shiftleft.codepropertygraph.cpgloading.*",
      "import _root_.io.shiftleft.codepropertygraph.generated.{help => _, _}",
      "import _root_.io.shiftleft.codepropertygraph.generated.nodes.*",
      "import _root_.io.joern.dataflowengineoss.language.*",
      "import _root_.io.shiftleft.semanticcpg.language.*",
      "import scala.jdk.CollectionConverters.*",
      "import _root_.io.shiftleft.semanticcpg.sarif.SarifConfig",
      "implicit val resolver: ICallResolver = NoResolve",
      "implicit val finder: NodeExtensionFinder = DefaultNodeExtensionFinder",
      "implicit val sarifConfig: SarifConfig = SarifConfig(semanticVersion = Option(version))"
    )

  val forInteractiveShell: Seq[String] = {
    shared ++
      Seq("import _root_.io.joern.joerncli.console.Joern.*") ++
      Run.codeForRunCommand().linesIterator ++
      Help.codeForHelpCommand(classOf[io.joern.joerncli.console.JoernConsole]).linesIterator ++
      Seq("ossDataFlowOptions = opts.ossdataflow")
  }

}
