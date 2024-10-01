package io.shiftleft.semanticcpg.typeinfo.loading

import io.shiftleft.semanticcpg.typeinfo.TypeDecl

import java.io.InputStream
import scala.util.Try;

/** ResultTy is probably one of: TypeDecl, Dependency. Maybe add def parseBinary(data: InputStream): Try[ResultTy]
  */
trait Loader[ResultTy] {
  def parse(data: String): Try[ResultTy]
  def parse(data: Array[Byte]): Try[ResultTy]
  def parseString(data: InputStream): Try[ResultTy]
}
