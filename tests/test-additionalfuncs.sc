// ./joern --import test-additionalfuncs.sc
// joern> exportMethods("workspace/src/cpg.bin", "a.txt")

def exportMethods(cpgFile: String, outFile: String) = {
  loadCpg(cpgFile)
  cpg.method.name.l |> outFile
}
