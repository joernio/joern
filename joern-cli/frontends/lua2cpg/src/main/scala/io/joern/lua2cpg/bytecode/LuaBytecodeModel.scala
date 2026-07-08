package io.joern.lua2cpg.bytecode

/** Lua 5.1 byte strings decoded with ISO-8859-1, preserving each byte as the same numeric code point. */
type LuaByteStringText = String

final case class LuaBytecodeArtifact(
  path: String,
  inputKind: String,
  profileId: Option[String],
  accepted: Boolean,
  diagnostic: LuaDiagnostic
)

final case class LuaBytecodeProfile(
  luaVersion: String,
  bytecodeVersion: String,
  format: Int,
  endianness: String,
  intSize: Int,
  sizeTSize: Int,
  instructionSize: Int,
  luaNumberSize: Int,
  numberMode: String,
  profileId: String
)

final case class LuaPrototype(
  prototypeId: String,
  parentPrototypeId: Option[String],
  ordinalPath: Vector[Int],
  sourceName: LuaByteStringText,
  firstLine: Long,
  lastLine: Long,
  upvalueCount: Int,
  numParams: Int,
  isVararg: Boolean,
  maxStack: Int,
  instructions: Vector[LuaInstruction],
  constants: Vector[LuaConstant],
  nested: Vector[LuaPrototype],
  lineNumbers: Vector[Long],
  locals: Vector[LuaLocal],
  upvalueNames: Vector[LuaByteStringText]
)

final case class LuaInstruction(
  pc: Int,
  opcode: LuaOpcode,
  mode: LuaInstructionMode,
  a: Int,
  b: Int,
  c: Option[Int]
)

final case class LuaConstant(index: Int, luaType: String, value: LuaConstantValue)

final case class LuaDiagnostic(
  kind: String,
  message: String,
  severity: String,
  successFactsAllowed: Boolean
)

final case class LuaLocal(name: LuaByteStringText, startPc: Long, endPc: Long)

final case class LuaBytecodeDecodeResult(
  artifact: LuaBytecodeArtifact,
  profile: Option[LuaBytecodeProfile],
  root: Option[LuaPrototype]
)

enum LuaInstructionMode(val encodedName: String) {
  case Abc  extends LuaInstructionMode("ABC")
  case ABx  extends LuaInstructionMode("ABx")
  case AsBx extends LuaInstructionMode("AsBx")

  override def toString: String = encodedName
}

enum LuaOpcode(val code: Int, val mode: LuaInstructionMode, val mnemonic: String) {
  case Move      extends LuaOpcode(0, LuaInstructionMode.Abc, "MOVE")
  case LoadK     extends LuaOpcode(1, LuaInstructionMode.ABx, "LOADK")
  case LoadBool  extends LuaOpcode(2, LuaInstructionMode.Abc, "LOADBOOL")
  case LoadNil   extends LuaOpcode(3, LuaInstructionMode.Abc, "LOADNIL")
  case GetUpval  extends LuaOpcode(4, LuaInstructionMode.Abc, "GETUPVAL")
  case GetGlobal extends LuaOpcode(5, LuaInstructionMode.ABx, "GETGLOBAL")
  case GetTable  extends LuaOpcode(6, LuaInstructionMode.Abc, "GETTABLE")
  case SetGlobal extends LuaOpcode(7, LuaInstructionMode.ABx, "SETGLOBAL")
  case SetUpval  extends LuaOpcode(8, LuaInstructionMode.Abc, "SETUPVAL")
  case SetTable  extends LuaOpcode(9, LuaInstructionMode.Abc, "SETTABLE")
  case NewTable  extends LuaOpcode(10, LuaInstructionMode.Abc, "NEWTABLE")
  case Self      extends LuaOpcode(11, LuaInstructionMode.Abc, "SELF")
  case Add       extends LuaOpcode(12, LuaInstructionMode.Abc, "ADD")
  case Sub       extends LuaOpcode(13, LuaInstructionMode.Abc, "SUB")
  case Mul       extends LuaOpcode(14, LuaInstructionMode.Abc, "MUL")
  case Div       extends LuaOpcode(15, LuaInstructionMode.Abc, "DIV")
  case Mod       extends LuaOpcode(16, LuaInstructionMode.Abc, "MOD")
  case Pow       extends LuaOpcode(17, LuaInstructionMode.Abc, "POW")
  case Unm       extends LuaOpcode(18, LuaInstructionMode.Abc, "UNM")
  case Not       extends LuaOpcode(19, LuaInstructionMode.Abc, "NOT")
  case Len       extends LuaOpcode(20, LuaInstructionMode.Abc, "LEN")
  case Concat    extends LuaOpcode(21, LuaInstructionMode.Abc, "CONCAT")
  case Jmp       extends LuaOpcode(22, LuaInstructionMode.AsBx, "JMP")
  case Eq        extends LuaOpcode(23, LuaInstructionMode.Abc, "EQ")
  case Lt        extends LuaOpcode(24, LuaInstructionMode.Abc, "LT")
  case Le        extends LuaOpcode(25, LuaInstructionMode.Abc, "LE")
  case Test      extends LuaOpcode(26, LuaInstructionMode.Abc, "TEST")
  case TestSet   extends LuaOpcode(27, LuaInstructionMode.Abc, "TESTSET")
  case Call      extends LuaOpcode(28, LuaInstructionMode.Abc, "CALL")
  case TailCall  extends LuaOpcode(29, LuaInstructionMode.Abc, "TAILCALL")
  case Return    extends LuaOpcode(30, LuaInstructionMode.Abc, "RETURN")
  case ForLoop   extends LuaOpcode(31, LuaInstructionMode.AsBx, "FORLOOP")
  case ForPrep   extends LuaOpcode(32, LuaInstructionMode.AsBx, "FORPREP")
  case TForLoop  extends LuaOpcode(33, LuaInstructionMode.Abc, "TFORLOOP")
  case SetList   extends LuaOpcode(34, LuaInstructionMode.Abc, "SETLIST")
  case Close     extends LuaOpcode(35, LuaInstructionMode.Abc, "CLOSE")
  case Closure   extends LuaOpcode(36, LuaInstructionMode.ABx, "CLOSURE")
  case Vararg    extends LuaOpcode(37, LuaInstructionMode.Abc, "VARARG")

  override def toString: String = mnemonic
}

object LuaOpcode {
  private val byCode: Map[Int, LuaOpcode] = LuaOpcode.values.map(opcode => opcode.code -> opcode).toMap

  def fromCode(code: Int): Option[LuaOpcode] = byCode.get(code)
}

enum LuaConstantValue {
  case NilValue
  case BooleanValue(value: Boolean)
  case NumberValue(value: Double)
  case StringValue(value: LuaByteStringText)
}
