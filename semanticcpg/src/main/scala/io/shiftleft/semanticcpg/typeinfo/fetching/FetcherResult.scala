package io.shiftleft.semanticcpg.typeinfo.fetching

import java.io.{ByteArrayInputStream, InputStream}
import java.nio.file.{Files, Path}
import scala.util.Using

/** The lifetime of the Fetcher that generates a FetcherResult must be
 * at least as long as the lifetimes of the FetcherResults. */
abstract class FetcherResult {
  /** @param func the inputstream passed to func should not be saved outside the 
   * context of func */
  def withNewInputStream[T](func: InputStream => T): T
}

/** Used in tests by TestMockFetcher */
class ByteArrayFetcherResult(private val bytes: Array[Byte]) extends FetcherResult {
  override def withNewInputStream[T](func: InputStream => T): T = {
    Using.resource(ByteArrayInputStream(bytes))(func)
  }
}

/** GitSparseFetcher uses this class to lazy loads the bytes in a file
 * downloaded using git sparse-checkout, so the GitSparseFetcher must not
 * have been `close()` before this result is used. */
class FileBasedFetcherResult(private val fsPath: Path) extends FetcherResult {
  override def withNewInputStream[T](func: InputStream => T): T = {
    checkFileExists()
    val inputStream = Files.newInputStream(fsPath)
    Using.resource(inputStream)(func)
  }

  /** TODO: API design that doesn't need this */
  private def checkFileExists(): Unit = {
    if (!Files.exists(fsPath)) {
      throw new RuntimeException(s"File at $fsPath doesn't exist, the lifetime of the fetcher must be at least as long as its results.")
    }
  }
}
