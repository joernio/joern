import mainargs.{main, arg, ParserForMethods, Flag}

object Main{
  @main
  def run(): Unit = {
    println("hello!")
  }

  def main(args: Array[String]): Unit = ParserForMethods(this).runOrExit(args)
}