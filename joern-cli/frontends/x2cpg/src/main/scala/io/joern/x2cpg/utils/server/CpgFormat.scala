package io.joern.x2cpg.utils.server

enum CpgFormat(val fileSuffix: String) {
  case ProtoCpgFormat     extends CpgFormat(".bin.zip")
  case FlatgraphCpgFormat extends CpgFormat(".fg")
}
