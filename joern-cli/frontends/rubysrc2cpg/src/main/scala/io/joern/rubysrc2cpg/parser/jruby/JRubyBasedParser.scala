package io.joern.rubysrc2cpg.parser.jruby

import org.jruby.ast.Node
import org.jruby.{Ruby, RubyInstanceConfig}

import java.io.FileInputStream

object JRubyBasedParser {
  private final val jruby = Ruby.newInstance(new RubyInstanceConfig(true))

  def parseFile(filename: String): Node =
    jruby.parseFile(new FileInputStream(filename), filename, jruby.getCurrentContext.getCurrentScope)

  def parseString(code: String, filename: String): Node = {
    jruby.parse(code, filename, jruby.getCurrentContext.getCurrentScope, 1, true)
  }
}
