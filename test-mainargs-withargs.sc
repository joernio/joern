import mainargs.main

object Main{
  @main def run(name: String) = {
    println(s"hello, $name")
  }

  def main(args: Array[String]): Unit = mainargs.ParserForMethods(this).runOrExit(args.toSeq)
}