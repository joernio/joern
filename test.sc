println("hello - outside main")

@main
def main(cpgFile: String) = {
  println("cpgFile=" + cpgFile)

  os.write(os.Path("/home/mp/Projects/shiftleft/joern/script-output"), "iwashere")
}
