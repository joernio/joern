package io.shiftleft.semanticcpg.typeinfo

trait Loader:
  def parse(data: String): Either[String, TypeDecl]