package io.shiftleft.semanticcpg.typeinfo.loading

import io.shiftleft.semanticcpg.typeinfo.TypeDecl
import java.io.InputStream

trait BytesLoader[ResultTy] {
  def loadFromBytes(data: Array[Byte]): ResultTy
}
