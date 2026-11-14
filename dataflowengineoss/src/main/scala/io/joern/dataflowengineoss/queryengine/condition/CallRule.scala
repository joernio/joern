package io.joern.dataflowengineoss.queryengine.condition

import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Expression, Literal}
import io.shiftleft.semanticcpg.language.*

/** A declarative matcher over [[Call]] nodes, used by [[ConditionalCallAnalyzer]] to recognise "bad calls" and "hard
  * stops" while conditionally walking a method's CFG.
  *
  * A rule matches a call iff every set constraint holds simultaneously (logical AND):
  *
  *   - `name` – the call's `name` property equals this string exactly.
  *   - `argument` – the call's argument at `argumentIndex` carries `expectedValue`. How the value is extracted depends
  *     on `expanded` (see below).
  *   - `mask` – bitwise constraint on the resolved argument value. When `argument` is also set, the matcher compares
  *     only the masked bits: `(actual & mask) == (expected & mask)`. When `argument` is absent, the matcher checks that
  *     any bit of `mask` is set in the actual value: `(actual & mask) != 0`. This supports flag-bit arguments such as
  *     `M_WAITOK | M_NOWAIT` where the rule only cares that the `M_WAITOK` bit is present.
  *   - `expanded` – when `true`, the argument at `argumentIndex` (or the call itself, when `argument` is unset) is
  *     expected to be a preprocessor macro expansion. c2cpg models object-like and function-like macro uses as [[Call]]
  *     nodes with `dispatchType = INLINED` and a `macroExpansion` edge to the lowered expression; the matcher follows
  *     that edge to recover the expanded literal value. When `false`, the argument must be a direct [[Literal]] whose
  *     `code` parses to the expected `BigInt`.
  *
  * `argument` and `mask` both need a concrete value at the matched index. Value extraction is best-effort: if the
  * argument is neither a literal nor an inline macro expansion that lowers to a literal, the constraint fails (no
  * match), which is the safe outcome for a security-relevant "bad call" detector.
  *
  * @param name
  *   exact call name to match (e.g. `"malloc"`, `"panic"`).
  * @param argument
  *   optional `(argumentIndex, expectedValue)` pair. `argumentIndex` follows the CPG `ARGUMENT_INDEX` convention used
  *   by c2cpg (first user argument is index 1).
  * @param mask
  *   optional bitmask applied to the resolved argument value.
  * @param expanded
  *   whether the matched argument/call is a macro expansion whose value must be recovered via `macroExpansion`.
  */
final case class CallRule(
  name: String,
  argument: Option[(Int, BigInt)] = None,
  mask: Option[BigInt] = None,
  expanded: Boolean = false
) {

  /** Test whether `call` satisfies this rule.
    *
    * Matching is a short-circuiting AND over the set constraints. The `name` check is cheapest and runs first; the
    * argument/mask checks run only if the name matches.
    */
  def matches(call: Call): Boolean =
    call.name == name && argumentMatches(call) && maskMatches(call)

  private def argumentMatches(call: Call): Boolean = argument match {
    case None                  => true
    case Some((idx, expected)) =>
      valueAt(call, idx) match {
        case None         => false
        case Some(actual) =>
          mask match {
            case None       => actual == expected
            case Some(bits) => (actual & bits) == (expected & bits)
          }
      }
  }

  private def maskMatches(call: Call): Boolean = (mask, argument) match {
    case (None, _)          => true
    case (Some(_), Some(_)) => true // already handled by argumentMatches
    case (Some(bits), None) =>
      // No exact value requested: just require one of the masked bits to be set.
      valueAt(call, 1).exists(actual => (actual & bits) != 0)
  }

  /** Resolve the `BigInt` value of the call's argument at `idx`, honouring the `expanded` flag. Returns `None` when the
    * argument cannot be reduced to an integer literal.
    */
  private def valueAt(call: Call, idx: Int): Option[BigInt] =
    call.argumentOption(idx).flatMap(exprToBigInt)

  private def exprToBigInt(expr: Expression): Option[BigInt] = expr match {
    case lit: Literal if !expanded                                            => parseBigInt(lit.code)
    case call: Call if expanded && call.dispatchType == DispatchTypes.INLINED =>
      call.macroExpansion.collectFirst { case lit: Literal => parseBigInt(lit.code) }.flatten
    case lit: Literal if expanded =>
      // `expanded` is set but the argument is already a bare literal (e.g. macro was fully lowered before CPG
      // creation). Accept it so the rule still fires; the flag is a hint, not a hard rejection.
      parseBigInt(lit.code)
    case _ => None
  }

  private def parseBigInt(code: String): Option[BigInt] = CallRule.parseNumericLiteral(code)

}

object CallRule {

  /** Parse a C-style integer token (`42`, `0x2`, `-0X10`) to [[BigInt]]. Returns [[None]] on failure rather than
    * throwing.
    */
  private[condition] def parseNumericLiteral(code: String): Option[BigInt] =
    try {
      val trimmed = code.trim
      if (trimmed.startsWith("0x") || trimmed.startsWith("0X"))
        Some(BigInt(trimmed.drop(2), 16))
      else if (trimmed.startsWith("-0x") || trimmed.startsWith("-0X"))
        Some(-BigInt(trimmed.drop(3), 16))
      else
        Some(BigInt(trimmed))
    } catch {
      case _: NumberFormatException | _: StringIndexOutOfBoundsException => None
    }

  /** Build a name-only rule. Matches any call with this `name`, regardless of arguments. */
  def apply(name: String): CallRule = new CallRule(name = name)

  /** Build a rule matching `name` whose argument at `index` equals `value` (literal, not macro-expanded). */
  def apply(name: String, index: Int, value: BigInt): CallRule =
    new CallRule(name = name, argument = Some((index, value)), expanded = false)

  /** Build a rule matching `name` whose argument carries the bits in `mask`. See class Scaladoc for the bitwise
    * semantics.
    */
  def apply(name: String, mask: BigInt): CallRule =
    new CallRule(name = name, mask = Some(mask))
}
