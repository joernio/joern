package io.joern.c2cpg

import io.joern.c2cpg.passes.{AstCreationPass, HeaderContentPass, PreprocessorPass}
import io.joern.c2cpg.utils.Report
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.Languages
import io.joern.x2cpg.passes.frontend.{MetaDataPass, TypeNodePass}
import io.joern.x2cpg.X2Cpg.withNewEmptyCpg
import io.joern.x2cpg.X2CpgFrontend

import scala.util.Try

class C2Cpg extends X2CpgFrontend[Config] {

  private val report: Report = new Report()

  def createCpg(config: Config): Try[Cpg] = {
    withNewEmptyCpg(config.outputPath, config) { (cpg, config) =>
      new MetaDataPass(cpg, Languages.NEWC, config.inputPath).createAndApply()
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
    }
  }

  def printIfDefsOnly(config: Config): Unit = {
    val stmts = new PreprocessorPass(config).run().mkString(",")
    println(stmts)
  }

}
