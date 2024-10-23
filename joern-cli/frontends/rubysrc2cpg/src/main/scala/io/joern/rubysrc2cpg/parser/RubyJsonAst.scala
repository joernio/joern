package io.joern.rubysrc2cpg.parser

import io.shiftleft.codepropertygraph.generated.Operators
import upickle.default.*

/** The JSON key values, in alphabetical order.
  */
object ParserKeys {
  val Alias          = "alias"
  val Arguments      = "arguments"
  val As             = "as"
  val Base           = "base"
  val Body           = "body"
  val Bodies         = "bodies"
  val Call           = "call"
  val CallName       = "call_name"
  val CaseExpression = "case_expression"
  val Children       = "children"
  val Code           = "code"
  val Collection     = "collection"
  val Condition      = "condition"
  val Conditions     = "conditions"
  val Def            = "def"
  val ElseClause     = "else_clause"
  val ElseBranch     = "else_branch"
  val End            = "end"
  val ExecList       = "exec_list"
  val ExecVar        = "exec_var"
  val FilePath       = "file_path"
  val Guard          = "guard"
  val Key            = "key"
  val Left           = "left"
  val Lhs            = "lhs"
  val MetaData       = "meta_data"
  val Name           = "name"
  val Op             = "op"
  val ParamIdx       = "param_idx"
  val Pattern        = "pattern"
  val RelFilePath    = "rel_file_path"
  val Receiver       = "receiver"
  val Right          = "right"
  val Rhs            = "rhs"
  val Statement      = "statement"
  val Start          = "start"
  val SuperClass     = "superclass"
  val ThenBranch     = "then_branch"
  val Type           = "type"
  val Value          = "value"
  val Values         = "values"
  val Variable       = "variable"
  val WhenClauses    = "when_clauses"
}

enum AstType(val name: String) {
  case Alias                        extends AstType("alias")
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
  case BlockArg                     extends AstType("blockarg")
  case BlockPass                    extends AstType("block_pass")
  case BlockWithNumberedParams      extends AstType("numblock")
  case Break                        extends AstType("break")
  case CaseExpression               extends AstType("case")
  case CaseMatchStatement           extends AstType("case_match")
  case ClassDefinition              extends AstType("class")
  case ClassVariable                extends AstType("cvar")
  case ClassVariableAssign          extends AstType("cvasgn")
  case ConstVariableAssign          extends AstType("casgn")
  case ConditionalSend              extends AstType("csend")
  case Defined                      extends AstType("defined?")
  case DynamicString                extends AstType("dstr")
  case DynamicSymbol                extends AstType("dsym")
  case Ensure                       extends AstType("ensure")
  case ExclusiveFlipFlop            extends AstType("eflipflop")
  case ExclusiveRange               extends AstType("erange")
  case ExecutableString             extends AstType("xstr")
  case False                        extends AstType("false")
  case FindPattern                  extends AstType("find_pattern")
  case Float                        extends AstType("float")
  case ForStatement                 extends AstType("for")
  case ForPostStatement             extends AstType("for_post")
  case ForwardArg                   extends AstType("forward_arg")
  case ForwardArgs                  extends AstType("forward_args")
  case ForwardedArgs                extends AstType("forwarded_args")
  case GlobalVariable               extends AstType("gvar")
  case GlobalVariableAssign         extends AstType("gvasgn")
  case Hash                         extends AstType("hash")
  case HashPattern                  extends AstType("hash_pattern")
  case Identifier                   extends AstType("ident")
  case IfGuard                      extends AstType("if_guard")
  case IfStatement                  extends AstType("if")
  case InclusiveFlipFlop            extends AstType("iflipflop")
  case InclusiveRange               extends AstType("irange")
  case InPattern                    extends AstType("in_pattern")
  case Int                          extends AstType("int")
  case InstanceVariable             extends AstType("ivar")
  case InstanceVariableAssign       extends AstType("ivasgn")
  case KwArg                        extends AstType("kwarg")
  case KwBegin                      extends AstType("kwbegin")
  case KwNilArg                     extends AstType("kwnilarg")
  case KwOptArg                     extends AstType("kwoptarg")
  case KwRestArg                    extends AstType("kwrestarg")
  case KwSplat                      extends AstType("kwsplat")
  case LocalVariable                extends AstType("lvar")
  case LocalVariableAssign          extends AstType("lvasgn")
  case MatchAlt                     extends AstType("match_alt")
  case MatchAs                      extends AstType("match_as")
  case MatchNilPattern              extends AstType("match_nil_pattern")
  case MatchPattern                 extends AstType("match_pattern")
  case MatchPatternP                extends AstType("match_pattern_p")
  case MatchRest                    extends AstType("match_rest")
  case MatchVariable                extends AstType("match_var")
  case MatchWithLocalVariableAssign extends AstType("match_with_lvasgn")
  case MethodDefinition             extends AstType("def")
  case ModuleDefinition             extends AstType("module")
  case MultipleAssignment           extends AstType("masgn")
  case MultipleLeftHandSide         extends AstType("mlhs")
  case Next                         extends AstType("next")
  case Nil                          extends AstType("nil")
  case NthRef                       extends AstType("nth_ref")
  case OperatorAssign               extends AstType("op_asgn")
  case OptionalArgument             extends AstType("optarg")
  case Or                           extends AstType("or")
  case OrAssign                     extends AstType("or_asgn")
  case Pair                         extends AstType("pair")
  case PostExpression               extends AstType("postexe")
  case PreExpression                extends AstType("preexe")
  case ProcArgument                 extends AstType("procarg0")
  case Rational                     extends AstType("rational")
  case Redo                         extends AstType("redo")
  case Retry                        extends AstType("retry")
  case Return                       extends AstType("return")
  case RegexExpression              extends AstType("regexp")
  case RegexOption                  extends AstType("regopt")
  case ResBody                      extends AstType("resbody")
  case RestArg                      extends AstType("restarg")
  case RescueStatement              extends AstType("rescue")
  case ScopedConstant               extends AstType("const")
  case Self                         extends AstType("self")
  case Send                         extends AstType("send")
  case ShadowArg                    extends AstType("shadowarg")
  case SingletonMethodDefinition    extends AstType("defs")
  case SingletonClassDefinition     extends AstType("sclass")
  case Splat                        extends AstType("splat")
  case StaticString                 extends AstType("str")
  case StaticSymbol                 extends AstType("sym")
  case Super                        extends AstType("super")
  case SuperNoArgs                  extends AstType("zsuper")
  case TopLevelConstant             extends AstType("cbase")
  case True                         extends AstType("true")
  case UnDefine                     extends AstType("undef")
  case UnlessExpression             extends AstType("unless")
  case UnlessGuard                  extends AstType("unless_guard")
  case UntilExpression              extends AstType("until")
  case UntilPostExpression          extends AstType("until_post")
  case WhenStatement                extends AstType("when")
  case WhileStatement               extends AstType("while")
  case WhilePostStatement           extends AstType("while_post")
  case Yield                        extends AstType("yield")
}

object AstType {
  def fromString(input: String): Option[AstType] = AstType.values.find(_.name == input)
}

object BinaryOperators {
  private val BinaryOperators: Set[String] =
    Set(
      "+",
      "-",
      "*",
      "/",
      "%",
      "**",
      "==",
      "===",
      "!=",
      "<",
      "<=",
      ">",
      ">=",
      "<=>",
      "&&",
      "and",
      "or",
      "||",
      "&",
      "|",
      "^",
      //      "<<"  -> Operators.shiftLeft,  Note: Generally Ruby abstracts this as an append operator based on the LHS
      ">>"
    )

  def isBinaryOperatorName(op: String): Boolean = BinaryOperators.contains(op)
}

object UnaryOperators {
  private val UnaryOperators: Set[String] =
    Set("!", "not", "~", "+", "-")

  def isUnaryOperatorName(op: String): Boolean = UnaryOperators.contains(op)
}
