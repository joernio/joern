package io.shiftleft.pythonparser.ast

sealed trait ioperator extends iast

case object Add extends ioperator {
  override def print: String = {
    "+"
  }
}
case object Sub extends ioperator {
  override def print: String = {
    "-"
  }
}
case object Mult extends ioperator {
  override def print: String = {
    "*"
  }
}
case object MatMult extends ioperator {
  override def print: String = {
    "@"
  }
}
case object Div extends ioperator {
  override def print: String = {
    "/"
  }
}
case object Mod extends ioperator {
  override def print: String = {
    "%"
  }
}
case object Pow extends ioperator {
  override def print: String = {
    "^^"
  }
}
case object LShift extends ioperator {
  override def print: String = {
    "<<"
  }
}
case object RShift extends ioperator {
  override def print: String = {
    ">>"
  }
}
case object BitOr extends ioperator {
  override def print: String = {
    "|"
  }
}
case object BitXor extends ioperator {
  override def print: String = {
    "^"
  }
}
case object BitAnd extends ioperator {
  override def print: String = {
    "&"
  }
}
case object FloorDiv extends ioperator {
  override def print: String = {
    "//"
  }
}
