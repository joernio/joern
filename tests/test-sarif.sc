// to test, run e.g.
// ./joern --script test-sarif.sc --param cpgFile=workspace/src/cpg.bin --param outFile=test.sarif

@main def exec(cpgFile: String, outFile: String) = {
  importCpg(cpgFile)
  assert(cpg.finding.nonEmpty, "no findings in this cpg - please check the setup")
  cpg.finding.toSarifJson() |> outFile
}
