package io.shiftleft.semanticcpg.typeinfo.loading

import io.shiftleft.semanticcpg.typeinfo.TypeDecl

import scala.util.Try;

trait Loader:
  def parse(data: String): Try[TypeDecl]
  def parse(data: Array[Byte]): Try[TypeDecl]
  