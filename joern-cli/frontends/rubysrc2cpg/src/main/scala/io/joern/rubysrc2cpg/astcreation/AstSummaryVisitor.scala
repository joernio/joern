package io.joern.rubysrc2cpg.astcreation

import io.joern.rubysrc2cpg.datastructures.RubyProgramSummary

trait AstSummaryVisitor { this: AstCreator =>

  protected var programSummary: RubyProgramSummary = RubyProgramSummary()

  def summarize(): RubyProgramSummary = {
    programSummary = RubyProgramSummary()
    programSummary
  }

  def withSummary(newSummary: RubyProgramSummary): AstCreator = {
    programSummary = newSummary
    this
  }

}
