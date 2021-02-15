import io.shiftleft.pythonparser.PyParser

import java.io.FileInputStream

object Main extends App {
  val parser = new PyParser()
  val module = parser.parse(new FileInputStream(args(0)))
  val errors = parser.errors
  if (errors.nonEmpty) {
    errors.foreach(println)
    System.exit(1)
  }
}
