package io.joern.rubysrc2cpg.parser

import upickle.default.*

/** The JSON key values, in alphabetical order.
  */
object ParserKeys {
  val Arguments   = "arguments"
  val Base        = "base"
  val Body        = "body"
  val CallName    = "call_name"
  val Children    = "children"
  val FilePath    = "file_path"
  val Key         = "key"
  val Lhs         = "lhs"
  val MetaData    = "meta_data"
  val Name        = "name"
  val RelFilePath = "rel_file_path"
  val Receiver    = "receiver"
  val Rhs         = "rhs"
  val SuperClass  = "superclass"
  val Type        = "type"
  val Value       = "value"
}

enum AstType(val name: String) {
  case And                          extends AstType("and")
  case AndAssign                    extends AstType("and_asgn")
  case Arg                          extends AstType("arg")
  case Args                         extends AstType("args")
  case Array                        extends AstType("array")
  case ArrayPattern                 extends AstType("array_pattern")
  case ArrayPatternWithTail         extends AstType("array_pattern_with_tail")
  case BackRef                      extends AstType("back_ref")
  case Begin                        extends AstType("begin")
  case Block                        extends AstType("block")
  case BlockPass                    extends AstType("block_pass")
  case ClassDefinition              extends AstType("class")
  case ClassVariable                extends AstType("cvar")
  case ClassVariableAssign          extends AstType("cvasgn")
  case ConstVariableAssign          extends AstType("casgn")
  case ConditionalSend              extends AstType("csend")
  case Defined                      extends AstType("defined?")
  case DynamicString                extends AstType("dstr")
  case DynamicSymbol                extends AstType("dsym")
  case ExclusiveFlipFlop            extends AstType("eflipflop")
  case ExclusiveRange               extends AstType("erange")
  case ExecutableString             extends AstType("xstr")
  case False                        extends AstType("false")
  case FindPattern                  extends AstType("find_pattern")
  case Float                        extends AstType("float")
  case ForwardArg                   extends AstType("forward_arg")
  case ForwardArgs                  extends AstType("forward_args")
  case ForwardedArgs                extends AstType("forwarded_args")
  case GlobalVariable               extends AstType("gvar")
  case GlobalVariableAssign         extends AstType("gvasgn")
  case Hash                         extends AstType("hash")
  case HashPattern                  extends AstType("hash_pattern")
  case Identifier                   extends AstType("ident")
  case InclusiveFlipFlop            extends AstType("iflipflop")
  case InclusiveRange               extends AstType("irange")
  case Int                          extends AstType("int")
  case InstanceVariable             extends AstType("ivar")
  case InstanceVariableAssign       extends AstType("ivasgn")
  case KwArg                        extends AstType("kwarg")
  case KwNilArg                     extends AstType("kwnilarg")
  case KwOptArg                     extends AstType("kwoptarg")
  case KwRestArg                    extends AstType("kwrestarg")
  case KwSplat                      extends AstType("kwsplat")
  case LocalVariable                extends AstType("lvar")
  case LocalVariableAssign          extends AstType("lvasgn")
  case MatchPattern                 extends AstType("match_pattern")
  case MatchPatternP                extends AstType("match_pattern_p") // TODO: Is this a real entry?
  case MatchVariable                extends AstType("match_var")
  case MatchWithLocalVariableAssign extends AstType("match_with_lvasgn")
  case MethodDefinition             extends AstType("def")
  case MultipleLeftHandSide         extends AstType("mlhs")
  case Nil                          extends AstType("nil")
  case NthRef                       extends AstType("nth_ref")
  case OperatorAssign               extends AstType("op_asgn")
  case Or                           extends AstType("or")
  case OrAssign                     extends AstType("or_asgn")
  case Pair                         extends AstType("pair")
  case ProcArgument                 extends AstType("procarg0")
  case Rational                     extends AstType("rational")
  case RestArg                      extends AstType("restarg")
  case ScopedConstant               extends AstType("const")
  case Self                         extends AstType("self")
  case Send                         extends AstType("send")
  case ShadowArg                    extends AstType("shadowarg")
  case Splat                        extends AstType("splat")
  case StaticString                 extends AstType("str")
  case StaticSymbol                 extends AstType("sym")
  case Super                        extends AstType("super")
  case TopLevelConstant             extends AstType("cbase")
  case True                         extends AstType("true")
  case UnDefine                     extends AstType("undef")
  case Yield                        extends AstType("yield")
}

object AstType {
  def fromString(input: String): Option[AstType] = AstType.values.find(_.name == input)
}
