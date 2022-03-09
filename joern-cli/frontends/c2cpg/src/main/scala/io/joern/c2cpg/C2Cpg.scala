package io.joern.c2cpg

import io.joern.c2cpg.passes.{AstCreationPass, HeaderContentPass, PreprocessorPass}
import io.joern.c2cpg.utils.Report
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.Languages
import io.joern.x2cpg.passes.frontend.{MetaDataPass, TypeNodePass}
import io.joern.x2cpg.X2Cpg.newEmptyCpg

class C2Cpg {

  private val report: Report = new Report()

  def runAndOutput(config: Config): Cpg = {
    val cpg = newEmptyCpg(Some(config.outputPath))

    new MetaDataPass(cpg, Languages.NEWC).createAndApply()

    val astCreationPass =
      new AstCreationPass(cpg, AstCreationPass.SourceFiles, config, report)
    astCreationPass.createAndApply()
    val headerAstCreationPass =
      new AstCreationPass(cpg, AstCreationPass.HeaderFiles, config, report)
    headerAstCreationPass.createAndApply()

    val types = astCreationPass.usedTypes() ++ headerAstCreationPass.usedTypes()
    new TypeNodePass(types.distinct, cpg).createAndApply()

    new HeaderContentPass(cpg, config).createAndApply()

    report.print()
    cpg
  }

  def printIfDefsOnly(config: Config): Unit = {
    val stmts = new PreprocessorPass(config).run().mkString(",")
    println(stmts)
  }

}
