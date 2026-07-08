package io.joern.lua2cpg.bytecode

import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.nio.charset.StandardCharsets

object LuaBytecodeDecoder {
  private val LuaMagic: Array[Byte]     = Array(0x1b.toByte, 0x4c.toByte, 0x75.toByte, 0x61.toByte)
  private val Lua51Version: Int         = 0x51
  private val DefaultInputKind: String  = "lua-bytecode"
  private val SuccessDiagnosticKind     = "accepted"
  private val SeverityInfo              = "info"
  private val SeverityError             = "error"
  private val NumberModeFloating        = "floating"
  private val NumberModeIntegral        = "integral"
  private val LuaByteStringCharset      = StandardCharsets.ISO_8859_1

  def decode(path: String, bytes: Array[Byte]): LuaBytecodeDecodeResult = {
    val reader = new Reader(bytes)
    reader.decode(path)
  }

  private final class Reader(bytes: Array[Byte]) {
    private var index: Int             = 0
    private var version: Int           = 0
    private var format: Int            = 0
    private var endianFlag: Int        = 1
    private var byteOrder: ByteOrder   = ByteOrder.LITTLE_ENDIAN
    private var intSize: Int           = 4
    private var sizeTSize: Int         = 8
    private var instructionSize: Int   = 4
    private var luaNumberSize: Int     = 8
    private var integralFlag: Int      = 0
    private var currentProfileId: Option[String] = None

    def decode(path: String): LuaBytecodeDecodeResult = {
      if (bytes.length < LuaMagic.length || !bytes.take(LuaMagic.length).sameElements(LuaMagic)) {
        return rejected(path, "not-lua-bytecode", "input does not start with Lua bytecode magic")
      }

      index = LuaMagic.length
      try {
        decodeHeader()
        val root = decodePrototype("root", None, Vector.empty)
        accepted(path, root)
      } catch {
        case error: DecodeFailure => rejected(path, error.kind, error.message)
      }
    }

    private def decodeHeader(): Unit = {
      version = readByte()
      format = readByte()
      endianFlag = readByte()
      intSize = readByte()
      sizeTSize = readByte()
      instructionSize = readByte()
      luaNumberSize = readByte()
      integralFlag = readByte()

      if (version != Lua51Version) {
        reject("unsupported-bytecode-version", f"expected Lua bytecode version 0x51, got 0x$version%02x")
      }
      validateImplementedHeader()
      byteOrder = endianFlag match {
        case 0 => ByteOrder.BIG_ENDIAN
        case 1 => ByteOrder.LITTLE_ENDIAN
      }
      currentProfileId = Some(profileId)
    }

    private def validateImplementedHeader(): Unit = {
      if (format != 0) {
        reject("unsupported-bytecode-profile", s"unsupported Lua 5.1 bytecode format $format")
      }
      if (!Set(0, 1).contains(endianFlag)) {
        reject("unsupported-bytecode-profile", s"unsupported Lua 5.1 endianness flag $endianFlag")
      }
      if (intSize <= 0) {
        reject("unsupported-bytecode-profile", s"unsupported Lua 5.1 int size $intSize")
      }
      if (sizeTSize <= 0) {
        reject("unsupported-bytecode-profile", s"unsupported Lua 5.1 size_t size $sizeTSize")
      }
      if (instructionSize != 4) {
        reject("unsupported-bytecode-profile", s"unsupported Lua 5.1 instruction size $instructionSize")
      }
      if (luaNumberSize != 8) {
        reject("unsupported-bytecode-profile", s"unsupported Lua 5.1 lua_Number size $luaNumberSize")
      }
      if (integralFlag != 0) {
        reject("unsupported-bytecode-profile", s"unsupported Lua 5.1 number mode integral_flag=$integralFlag")
      }
    }

    private def decodePrototype(
      prototypeId: String,
      parentPrototypeId: Option[String],
      ordinalPath: Vector[Int]
    ): LuaPrototype = {
      val sourceName    = readString()
      val firstLine     = readUInt()
      val lastLine      = readUInt()
      val upvalueCount  = readByte()
      val numParams     = readByte()
      val isVararg      = (readByte() & 0x02) != 0
      val maxStack      = readByte()
      val instructions  = readVector(readUInt(), pc => decodeInstruction(pc))
      val constants     = readVector(readUInt(), constantIndex => decodeConstant(constantIndex))
      val nested = readVector(readUInt(), childOrdinal => {
        val childId = s"$prototypeId.$childOrdinal"
        decodePrototype(childId, Some(prototypeId), ordinalPath :+ childOrdinal)
      })
      val lineNumbers  = readVector(readUInt(), _ => readUInt())
      val locals       = readVector(readUInt(), _ => LuaLocal(readString(), readUInt(), readUInt()))
      val upvalueNames = readVector(readUInt(), _ => readString())

      LuaPrototype(
        prototypeId = prototypeId,
        parentPrototypeId = parentPrototypeId,
        ordinalPath = ordinalPath,
        sourceName = sourceName,
        firstLine = firstLine,
        lastLine = lastLine,
        upvalueCount = upvalueCount,
        numParams = numParams,
        isVararg = isVararg,
        maxStack = maxStack,
        instructions = instructions,
        constants = constants,
        nested = nested,
        lineNumbers = lineNumbers,
        locals = locals,
        upvalueNames = upvalueNames
      )
    }

    private def decodeInstruction(pc: Int): LuaInstruction = {
      val raw         = readUInt32()
      val opcodeCode  = bits(raw, 0, 6).toInt
      val opcode      = LuaOpcode.fromCode(opcodeCode).getOrElse {
        reject("malformed-constant", s"invalid opcode $opcodeCode at pc $pc")
      }
      val a = bits(raw, 6, 8).toInt
      opcode.mode match {
        case LuaInstructionMode.Abc =>
          LuaInstruction(
            pc = pc,
            opcode = opcode,
            mode = opcode.mode,
            a = a,
            b = bits(raw, 23, 9).toInt,
            c = Some(bits(raw, 14, 9).toInt)
          )
        case LuaInstructionMode.ABx =>
          LuaInstruction(pc = pc, opcode = opcode, mode = opcode.mode, a = a, b = bits(raw, 14, 18).toInt, c = None)
        case LuaInstructionMode.AsBx =>
          LuaInstruction(
            pc = pc,
            opcode = opcode,
            mode = opcode.mode,
            a = a,
            b = bits(raw, 14, 18).toInt - 131071,
            c = None
          )
      }
    }

    private def decodeConstant(constantIndex: Int): LuaConstant = {
      readByte() match {
        case 0 => LuaConstant(constantIndex, "nil", LuaConstantValue.NilValue)
        case 1 => LuaConstant(constantIndex, "boolean", LuaConstantValue.BooleanValue(readByte() != 0))
        case 3 => LuaConstant(constantIndex, "number", LuaConstantValue.NumberValue(readDouble()))
        case 4 => LuaConstant(constantIndex, "string", LuaConstantValue.StringValue(readString()))
        case other =>
          reject("malformed-constant", s"unsupported constant tag $other at index $constantIndex")
      }
    }

    private def readVector[A](count: Long, decodeElement: Int => A): Vector[A] = {
      if (count > Int.MaxValue) {
        reject("unsupported-bytecode-profile", s"unsupported Lua 5.1 element count $count")
      }
      Vector.tabulate(count.toInt)(decodeElement)
    }

    private def readByte(): Int = {
      ensureAvailable(1)
      val value = bytes(index) & 0xff
      index += 1
      value
    }

    private def readUInt32(): Long = {
      ensureAvailable(4)
      val value = ByteBuffer.wrap(bytes, index, 4).order(byteOrder).getInt.toLong & 0xffffffffL
      index += 4
      value
    }

    private def readUInt(): Long = readUnsignedInteger(intSize)

    private def readSizeT(): Long = readUnsignedInteger(sizeTSize)

    private def readUnsignedInteger(size: Int): Long = {
      ensureAvailable(size)
      val raw = bytes.slice(index, index + size)
      index += size
      val orderedBytes = if (byteOrder == ByteOrder.LITTLE_ENDIAN) raw.reverse else raw
      val value        = BigInt(1, orderedBytes)
      if (value > BigInt(Long.MaxValue)) {
        reject("unsupported-bytecode-profile", s"unsupported Lua 5.1 unsigned integer value $value")
      }
      value.longValue
    }

    private def readDouble(): Double = {
      ensureAvailable(luaNumberSize)
      val value = ByteBuffer.wrap(bytes, index, luaNumberSize).order(byteOrder).getDouble
      index += luaNumberSize
      value
    }

    private def readString(): LuaByteStringText = {
      val size = readSizeT()
      if (size == 0) {
        return ""
      }
      if (size > Int.MaxValue) {
        reject("malformed-constant", s"unsupported Lua bytecode string size $size")
      }
      ensureAvailable(size.toInt)
      val bytesStart = index
      index += size.toInt
      if (bytes(bytesStart + size.toInt - 1) != 0) {
        reject("malformed-constant", "unterminated Lua bytecode string")
      }
      new String(bytes, bytesStart, size.toInt - 1, LuaByteStringCharset)
    }

    private def ensureAvailable(size: Int): Unit = {
      if (size < 0 || index > bytes.length - size) {
        reject("truncated-bytecode", "unexpected end of bytecode stream")
      }
    }

    private def accepted(path: String, root: LuaPrototype): LuaBytecodeDecodeResult = {
      val profile = buildProfile()
      LuaBytecodeDecodeResult(
        artifact = LuaBytecodeArtifact(
          path = path,
          inputKind = DefaultInputKind,
          profileId = Some(profile.profileId),
          accepted = true,
          diagnostic = LuaDiagnostic(
            kind = SuccessDiagnosticKind,
            message = "Lua 5.1 bytecode accepted",
            severity = SeverityInfo,
            successFactsAllowed = true
          )
        ),
        profile = Some(profile),
        root = Some(root)
      )
    }

    private def rejected(path: String, kind: String, message: String): LuaBytecodeDecodeResult = {
      LuaBytecodeDecodeResult(
        artifact = LuaBytecodeArtifact(
          path = path,
          inputKind = DefaultInputKind,
          profileId = currentProfileId,
          accepted = false,
          diagnostic = LuaDiagnostic(
            kind = kind,
            message = message,
            severity = SeverityError,
            successFactsAllowed = false
          )
        ),
        profile = currentProfileId.map(_ => buildProfile()),
        root = None
      )
    }

    private def buildProfile(): LuaBytecodeProfile = {
      LuaBytecodeProfile(
        luaVersion = "5.1",
        bytecodeVersion = f"0x$version%02x",
        format = format,
        endianness = if (byteOrder == ByteOrder.BIG_ENDIAN) "big-endian" else "little-endian",
        intSize = intSize,
        sizeTSize = sizeTSize,
        instructionSize = instructionSize,
        luaNumberSize = luaNumberSize,
        numberMode = if (integralFlag == 0) NumberModeFloating else NumberModeIntegral,
        profileId = profileId
      )
    }

    private def profileId: String = {
      val endian     = if (byteOrder == ByteOrder.BIG_ENDIAN) "big" else "little"
      val numberMode = if (integralFlag == 0) "float" else "integral"
      s"lua51-$endian-int$intSize-size_t$sizeTSize-instruction$instructionSize-number$luaNumberSize-$numberMode"
    }

    private def bits(number: Long, position: Int, size: Int): Long = {
      (number >> position) & ((1L << size) - 1L)
    }

    private def reject(kind: String, message: String): Nothing = {
      throw DecodeFailure(kind, message)
    }
  }

  private final case class DecodeFailure(kind: String, message: String) extends RuntimeException(message)
}
