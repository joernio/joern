@main def exec(cpgFile: String, outFile: String) = {
  loadCpg(cpgFile)
  cpg.method.name.l |> outFile
}
