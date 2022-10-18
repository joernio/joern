// to test, run e.g.
// ./joern --script test-cpg-callotherscript.sc --import test-additionalfuncs.sc --params cpgFile=workspace/src/cpg.bin,outFile=a.txt

@main def exec(cpgFile: String, outFile: String) = {
  exportMethods(cpgFile, outFile)
}