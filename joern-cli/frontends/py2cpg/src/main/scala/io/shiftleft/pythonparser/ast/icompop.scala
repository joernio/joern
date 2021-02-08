package io.shiftleft.pythonparser.ast

sealed trait icompop extends iast

case object Eq extends icompop {
  override def print: String = {
    "=="
  }
}
case object NotEq extends icompop {
  override def print: String = {
    "!="
  }
}
case object Lt extends icompop {
  override def print: String = {
    "<"
  }
}
case object LtE extends icompop {
  override def print: String = {
    "<="
  }
}
case object Gt extends icompop {
  override def print: String = {
    ">"
  }
}
case object GtE extends icompop {
  override def print: String = {
    ">="
  }
}
case object Is extends icompop {
  override def print: String = {
    "is"
  }
}
case object IsNot extends icompop {
  override def print: String = {
    "is not"
  }
}
case object In extends icompop {
  override def print: String = {
    "in"
  }
}
case object NotIn extends icompop {
  override def print: String = {
    "not in"
  }
}
