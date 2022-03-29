package io.shiftleft.semanticcpg.accesspath

sealed abstract class AccessElement(name: String) extends Comparable[AccessElement] {
  override def toString: String = name
  def kind: Int
  override def hashCode(): Int = kind + name.hashCode

  override def compareTo(other: AccessElement): Int = {
    this.kind.compareTo(other.kind) match {
      case 0         => this.name.compareTo(other.toString)
      case different => different
    }
  }
}

case class ConstantAccess(constant: String) extends AccessElement(constant) {
  override def kind: Int = 0x01010101
}

case object VariableAccess extends AccessElement("?") {
  override def kind: Int = 0x02020202
}

case object VariablePointerShift extends AccessElement("<?>") {
  override def kind: Int = 0x03030303
}

// this will eventually get an optional extent (how many bytes wide is the memory load/store)
case object IndirectionAccess extends AccessElement("*") {
  override def kind: Int = 0x04040404
}

case object AddressOf extends AccessElement("&") {
  override def kind: Int = 0x05050505
}

// this will eventually obtain an optional byteOffset
case class PointerShift(logicalOffset: Int) extends AccessElement(s"<${logicalOffset}>") {
  override def kind: Int = 0x06060606
}
