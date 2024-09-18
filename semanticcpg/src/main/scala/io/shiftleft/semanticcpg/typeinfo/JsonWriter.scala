package io.shiftleft.semanticcpg.typeinfo

import io.shiftleft.semanticcpg.typeinfo.Writer
import org.json4s.*
import org.json4s.FieldSerializer.*
import org.json4s.native.JsonMethods.*
import org.json4s.native.{Serialization, prettyJson, renderJValue}

object JsonWriter extends Writer {
  implicit val format: Formats = JsonLoader.format // TODO

  def writeToString(ty: TypeDecl): String = Serialization.writePretty(ty)
}
