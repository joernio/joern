package io.shiftleft.console

sealed trait SLProduct { def name: String }
case object OcularProduct extends SLProduct { val name: String = "ocular" }
case object JoernProduct extends SLProduct { val name: String = "joern" }
