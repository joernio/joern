package io.shiftleft.semanticcpg.typeinfo

import scala.util.Try;

trait Loader:
  def parse(data: String): Try[TypeDecl]