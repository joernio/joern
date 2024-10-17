package io.shiftleft.semanticcpg.typeinfo

import java.io.OutputStream
import scala.util.Try

/** TODO: will change to match the current TypeStorageLoader */
trait BytesWriter[InputTy] {
  def write(ty: InputTy): Array[Byte]
}
