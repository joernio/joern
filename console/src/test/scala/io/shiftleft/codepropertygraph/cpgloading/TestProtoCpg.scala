package io.shiftleft.codepropertygraph.cpgloading

import better.files.File
import io.shiftleft.proto.cpg.Cpg
import io.shiftleft.proto.cpg.Cpg.CpgStruct

import java.io.FileOutputStream

object TestProtoCpg {

  def createTestProtoCpg: File = {
    val outDir = better.files.File.newTemporaryDirectory("cpgloadertests")
    val outStream = new FileOutputStream((outDir / "1.proto").pathAsString)
    CpgStruct
      .newBuilder()
      .addNode(
        CpgStruct.Node
          .newBuilder()
          .setKey(1)
          .setType(CpgStruct.Node.NodeType.valueOf("METHOD"))
          .addProperty(
            CpgStruct.Node.Property
              .newBuilder()
              .setName(Cpg.NodePropertyName.valueOf("FULL_NAME"))
              .setValue(Cpg.PropertyValue
                .newBuilder()
                .setStringValue("foo")
                .build())
              .build
          )
          .build())
      .build()
      .writeTo(outStream)
    outStream.close()

    val zipFile = better.files.File.newTemporaryFile("cpgloadertests", ".bin.zip")
    outDir.zipTo(zipFile)
    outDir.delete()
    zipFile
  }

}
