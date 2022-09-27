package dotty.ammonite.foo

import dotty.tools.dotc.interactive.Completion

trait AmmCompletionExtras {

  def maybeBackticked(input: String, hasBackTick: Boolean): Completion =
    Completion.backtickCompletions(Completion(input, "", Nil), hasBackTick)
  def backtick(completion: Completion): Completion =
    Completion.backtickCompletions(completion, completion.label.startsWith("`"))

}
