package io.shiftleft.semanticcpg.typeinfo

import java.io.ByteArrayOutputStream
import scala.util.Try

trait Writer:
  def writeToBinaryFormat(ty: TypeDecl): Try[Array[Byte]]
  def writeToString(ty: TypeDecl): Try[String]
