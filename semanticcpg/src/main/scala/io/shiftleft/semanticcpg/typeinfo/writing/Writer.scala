package io.shiftleft.semanticcpg.typeinfo

import java.io.OutputStream
import scala.util.Try

trait Writer[InputTy] {
  def writeToBinaryFormat(ty: InputTy): Try[Array[Byte]]
  def writeToString(ty: InputTy): Try[String]
  def writeToStream(ty: InputTy, os: OutputStream): Try[Unit]
}
