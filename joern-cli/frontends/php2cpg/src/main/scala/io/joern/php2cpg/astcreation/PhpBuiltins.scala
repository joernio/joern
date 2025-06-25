package io.joern.php2cpg.astcreation

import scala.io.Source

object PhpBuiltins {
  lazy val FuncNames: Set[String] = {
    Source.fromResource("builtin_functions.txt").getLines().toSet
  }
}
