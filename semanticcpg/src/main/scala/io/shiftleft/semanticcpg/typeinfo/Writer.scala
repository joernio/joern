package io.shiftleft.semanticcpg.typeinfo

import java.io.ByteArrayOutputStream

trait Writer:
  def writeToString(ty: TypeDecl): String
//  def write(ty: TypeDecl): ByteArrayOutputStream
//  def writeToFile(ty: TypeDecl, filename: String): Unit
