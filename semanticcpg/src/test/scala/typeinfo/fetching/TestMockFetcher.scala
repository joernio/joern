package io.shiftleft.semanticcpg.typeinfo.fetching

import io.shiftleft.semanticcpg.typeinfo.fetching._

import java.io.{InputStream}

/** For use in testing, this is a fetcher that is just an immutable wrapper around an
 * immutable `Map[String, Array[Byte]]` where the String is a server path like
 * `/jvm/$PACKAGE_NAME/$VERSION/$TYPE_NAME.ion` */
class TestMockFetcher private (private val store: Map[String, Array[Byte]]) extends Fetcher {
  def this() = this(Map())
  
  /** First item of each tuple is string of ServerPath and second item is file contents
   * as a string */
  def addFiles(files: (String, String)*): TestMockFetcher = {
    val withByteContent = files.map((fileName, content) => (fileName, content.getBytes("UTF-8")))
    TestMockFetcher(store ++ withByteContent)
  }
  
  override def close(): Unit = ()

  override protected def downloadFiles(paths: List[ServerPath]): List[FetcherResult] = {
    paths.foreach(validateFileInStore)
    paths.map(serverPath => {
      val fileContents = store(serverPath.toString)
      ByteArrayFetcherResult(fileContents)
    })
  }
  
  private def validateFileInStore(path: ServerPath): Unit = {
    if (!store.contains(path.toString)) {
      val storeContents = store.mkString("->")
      val errMsg = s"File with server path $path is not in TestMockFetcher store $storeContents"
      throw new IllegalArgumentException(errMsg)
    }
  }
}
