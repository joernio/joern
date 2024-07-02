package io.joern.scanners.kotlin

import io.joern.scanners.*
import io.joern.console.*
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.joern.dataflowengineoss.language.*
import io.joern.macros.QueryMacros.*
import io.shiftleft.semanticcpg.language.*

object PathTraversals extends QueryBundle {
  implicit val engineContext: EngineContext = EngineContext(Semantics.empty)
  implicit val resolver: ICallResolver      = NoResolve

  @q
  def unzipDirectoryTraversal(): Query =
    Query.make(
      name = "unzip-directory-traversal",
      author = Crew.claudiu,
      title = "Zip entries not checked before unzipping",
      description = "-",
      score = 4,
      withStrRep({ cpg =>
        val zipEntryTypeFullNames =
          List("org.apache.commons.compress.archivers.ArchiveEntry", "java.util.zip.ZipEntry")
        def zipEntryParams =
          cpg.method.parameter.typeFullNameExact(zipEntryTypeFullNames*)
        def fileOutStreamInit =
          cpg.method.fullNameExact("java.io.FileOutputStream.<init>:void(java.io.File)").callIn
        def pathStartsWithCalls =
          cpg.method.fullNameExact("java.nio.file.Path.startsWith:boolean(java.nio.file.Path)").callIn

        def uncheckedZipEntryParameters = zipEntryParams.filter { param =>
          pathStartsWithCalls.argument(0).reachableByFlows(param).isEmpty
        }
        fileOutStreamInit.filter { call =>
          call.argument.reachableByFlows(uncheckedZipEntryParameters).nonEmpty
        }
      }),
      tags = List(QueryTags.pathTraversal, QueryTags.android),
      multiFileCodeExamples = MultiFileCodeExamples(
        positive = List(
          List(
            CodeSnippet(
              """
                |import java.io.*
                |import java.nio.file.Paths
                |import java.util.zip.ZipFile
                |
                |fun unzip(zipFilePath: File, destDirectory: String) {
                |    val BUFFER_SIZE = 4096
                |    ZipFile(zipFilePath).use { zip ->
                |        zip.entries().asSequence().forEach { entry ->
                |            val zipEntryInputStream = zip.getInputStream(entry)
                |
                |            val fileForEntry = File(destDirectory, entry.getName())
                |            val entryOutStream = FileOutputStream(fileForEntry)
                |
                |            val bos = BufferedOutputStream(entryOutStream)
                |            val bytesIn = ByteArray(BUFFER_SIZE)
                |            var read: Int
                |            while (zipEntryInputStream.read(bytesIn).also { read = it } != -1) {
                |                bos.write(bytesIn, 0, read)
                |            }
                |            bos.close()
                |
                |            zipEntryInputStream.close()
                |        }
                |    }
                |}
                |
                |fun main() {
                |    // to make a slippery zip:
                |    // 1. create a file /tmp/zip/extract/slip.txt
                |    // 2. inside /tmp/zip/extract run `zip slip.zip ../slip.txt`
                |    val zp = File("/tmp/zip/slip.zip")
                |    unzip(zp, "/tmp/zip/extract")
                |}
                |""".stripMargin,
              "Positive.kt"
            )
          )
        ),
        negative = List(
          List(
            CodeSnippet(
              """
              |import java.io.*
              |import java.nio.file.Paths
              |import java.util.zip.ZipFile
              |
              |fun unzip(zipFilePath: File, destDirectory: String) {
              |    val BUFFER_SIZE = 4096
              |    ZipFile(zipFilePath).use { zip ->
              |        zip.entries().asSequence().forEach { entry ->
              |            val zipEntryInputStream = zip.getInputStream(entry)
              |
              |            val fileForEntry = File(destDirectory, entry.getName())
              |            if (!fileForEntry.toPath().normalize().startsWith(Paths.get(destDirectory))) {
              |                throw Exception("Whatever's in this zip, it's not good.")
              |            }
              |            val entryOutStream = FileOutputStream(fileForEntry)
              |
              |            val bos = BufferedOutputStream(entryOutStream)
              |            val bytesIn = ByteArray(BUFFER_SIZE)
              |            var read: Int
              |            while (zipEntryInputStream.read(bytesIn).also { read = it } != -1) {
              |                bos.write(bytesIn, 0, read)
              |            }
              |            bos.close()
              |
              |            zipEntryInputStream.close()
              |        }
              |    }
              |}
              |""".stripMargin,
              "Negative.kt"
            )
          )
        )
      )
    )
}
