package io.shiftleft.semanticcpg.typeinfo

import java.io.OutputStream
import scala.util.Try

trait BytesWriter[InputTy] {
  def write(ty: InputTy): Array[Byte]
}
