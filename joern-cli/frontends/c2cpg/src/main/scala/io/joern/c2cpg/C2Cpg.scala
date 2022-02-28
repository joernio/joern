package io.joern.c2cpg

import io.joern.c2cpg.Main.Config
import io.joern.c2cpg.passes.{AstCreationPass, HeaderContentPass, PreprocessorPass}
import io.joern.c2cpg.utils.Report
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.passes.{IntervalKeyPool, KeyPoolCreator}
import io.shiftleft.semanticcpg.passes.frontend.{MetaDataPass, TypeNodePass}
import io.joern.x2cpg.X2Cpg.newEmptyCpg

class C2Cpg {

  private val report: Report = new Report()

  def createCpg(config: Config): Cpg = {
    val keyPool              = KeyPoolCreator.obtain(4, minValue = 101)
    val metaDataKeyPool      = new IntervalKeyPool(1, 100)
    val typesKeyPool         = keyPool.head
    val astKeyPool           = keyPool(1)
    val headerKeyPool        = keyPool(2)
    val headerContentKeyPool = keyPool(3)

    val cpg = newEmptyCpg(Some(config.outputPath))

    new MetaDataPass(cpg, Languages.NEWC, Some(metaDataKeyPool)).createAndApply()

    val astCreationPass =
      new AstCreationPass(cpg, AstCreationPass.SourceFiles, Some(astKeyPool), config, report)
    astCreationPass.createAndApply()
    val headerAstCreationPass =
      new AstCreationPass(cpg, AstCreationPass.HeaderFiles, Some(headerKeyPool), config, report)
    headerAstCreationPass.createAndApply()

    val types = astCreationPass.usedTypes() ++ headerAstCreationPass.usedTypes()
    new TypeNodePass(types.distinct, cpg, Some(typesKeyPool)).createAndApply()

    new HeaderContentPass(cpg, Some(headerContentKeyPool), config).createAndApply()

    report.print()
    cpg
  }

  def printIfDefsOnly(config: Config): Unit = {
    val stmts = new PreprocessorPass(config).run().mkString(",")
    println(stmts)
  }

}
