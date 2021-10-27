package io.shiftleft.console

import ammonite.runtime.Storage
import ammonite.util.Tag
import os.Path

/**
  * like the default ammonite folder storage (which gives us e.g. command history), but without the CodePredef
  * error when using multiple ocular installations (see https://github.com/ShiftLeftSecurity/product/issues/2082)
  */
class StorageBackend(slProduct: SLProduct) extends Storage.Folder(StorageBackend.consoleHome(slProduct)) {
  override def compileCacheSave(path: String, tag: Tag, data: Storage.CompileCache): Unit = ()
  override def compileCacheLoad(path: String, tag: Tag): Option[Storage.CompileCache] = None
}

object StorageBackend {
  def consoleHome(slProduct: SLProduct): Path =
    os.Path(System.getProperty("user.home")) / ".shiftleft" / slProduct.name
}
