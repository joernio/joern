// to test, run e.g.
// ./joern --script test-cpg-callotherscript.sc --import test-additionalfuncs.sc --param cpgFile=workspace/src/cpg.bin --param outFile=a.txt

@main def exec(cpgFile: String, outFile: String) = {
  exportMethods(cpgFile, outFile)
}
