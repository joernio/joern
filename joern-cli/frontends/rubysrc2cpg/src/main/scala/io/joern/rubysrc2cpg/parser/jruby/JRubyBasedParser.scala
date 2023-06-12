package io.joern.rubysrc2cpg.parser.jruby

import org.jruby.ast.Node
import org.jruby.util.ByteList
import org.jruby.{Ruby, RubyInstanceConfig}

import java.io.FileInputStream

object JRubyBasedParser {
  private final val jruby = Ruby.newInstance(new RubyInstanceConfig(true))

  def parseFile(filename: String): Node =
    jruby.parseFile(new FileInputStream(filename), filename, null)

  def parseString(code: String, filename: String): Node = {
    jruby.parse(new ByteList(code.getBytes), filename, null, 1, true)
  }
}
