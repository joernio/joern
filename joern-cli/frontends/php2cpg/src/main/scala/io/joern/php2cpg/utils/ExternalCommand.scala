package io.joern.php2cpg.utils

object ExternalCommand extends io.joern.x2cpg.utils.ExternalCommand {

  override val shellPrefix: Seq[String] = if (IsWin) "cmd" :: "/c" :: Nil else "sh" :: "-c" :: Nil

}
