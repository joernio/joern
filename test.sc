println("hello - outside main")

@main
def main(cpgFile: String) = {
  // val cpgFile: String = "asd"
  println("cpgFile=" + cpgFile)

  os.write(os.Path("/home/mp/Projects/shiftleft/joern/script-output"), "iwashere")

   //loadCpg(cpgFile)
   //cpg.method.name.l |> outFile
}
