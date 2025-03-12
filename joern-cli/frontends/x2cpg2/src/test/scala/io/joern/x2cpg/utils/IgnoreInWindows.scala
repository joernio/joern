package io.joern.x2cpg.utils

import org.scalatest.{Ignore, Tag}

object IgnoreInWindows
    extends Tag(if (System.getProperty("os.name").toLowerCase().contains("windows")) {
      classOf[Ignore].getName
    } else {
      ""
    })
