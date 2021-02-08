import io.shiftleft.pythonparser.PyParser

import java.io.FileInputStream

object Main extends App {
  val module = PyParser.parse(new FileInputStream("test.py"))
  println(module)
}
