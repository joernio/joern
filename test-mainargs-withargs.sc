import mainargs.{main, arg, ParserForMethods, Flag}

object Main{
  @main
  def run(name: String) = {
    println(s"hello, $name")
  }

  def main(args: Array[String]): Unit = ParserForMethods(this).runOrExit(args.toSeq)
}