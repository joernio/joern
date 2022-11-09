package io.joern.pythonparser

import io.joern.pythonparser.CharStreamImpl.{defaultInputBufferSize, defaultMinimumReadSize}

import java.io.{IOException, InputStream, InputStreamReader}

object CharStreamImpl {
  private val defaultInputBufferSize = 4096
  private val defaultMinimumReadSize = 2048
}

class CharStreamImpl(inputStream: InputStream, inputBufferSize: Int, minimumReadSize: Int) extends CharStream {
  private val inputReader = new InputStreamReader(inputStream)

  private var inputBuffer       = new Array[Char](inputBufferSize)
  private var posToLine         = new Array[Int](inputBufferSize)
  private var posToColumn       = new Array[Int](inputBufferSize)
  private var readPos           = 1
  private var writePos          = 1
  private var tokenBeginPos     = 1
  private var inputBufferOffset = 0 // From start of inputStream
  private var tabSize           = 1

  // The first slot of inputBuffer, posToLine, posToColumn represents the last value
  // from the previous chunk red from inputStream. For the very first chunk we
  // initialise the values by hand so that requires no special cases in the implementation.
  inputBuffer(0) = 'a' // We could have picked any char that is not '\n' or '\r'.
  posToLine(0) = 1
  posToColumn(0) = 0

  def this(inputStream: InputStream) = {
    this(inputStream, defaultInputBufferSize, defaultMinimumReadSize)
  }

  def getBeginPos: Int = inputBufferOffset + tokenBeginPos - 1

  private def fillBuffer(): Unit = {
    if (readPos == writePos) {
      // No more data to read
      if (writePos == inputBuffer.length) {
        // No more space in inputBuffer

        val keepStartPos = tokenBeginPos - 1
        val charsToKeep  = writePos - keepStartPos
        if (inputBuffer.length - charsToKeep < minimumReadSize) {
          // Resize buffer and move content to the front.
          val newBufferLen = charsToKeep + minimumReadSize

          val newInputBuffer = new Array[Char](newBufferLen)
          Array.copy(inputBuffer, keepStartPos, newInputBuffer, 0, charsToKeep)
          inputBuffer = newInputBuffer

          val newPosToLine = new Array[Int](newBufferLen)
          Array.copy(posToLine, keepStartPos, newPosToLine, 0, charsToKeep)
          posToLine = newPosToLine

          val newPosToColumn = new Array[Int](newBufferLen)
          Array.copy(posToColumn, keepStartPos, newPosToColumn, 0, charsToKeep)
          posToColumn = newPosToColumn
        } else {
          // Enough space left to just move content to the front.
          Array.copy(inputBuffer, keepStartPos, inputBuffer, 0, charsToKeep)
          Array.copy(posToLine, keepStartPos, posToLine, 0, charsToKeep)
          Array.copy(posToColumn, keepStartPos, posToColumn, 0, charsToKeep)
        }
        writePos = charsToKeep
        readPos = readPos - keepStartPos
        inputBufferOffset += keepStartPos
        tokenBeginPos = 1
      }

      val charsRed = inputReader.read(inputBuffer, writePos, inputBuffer.length - writePos)
      if (charsRed != -1) {
        writePos += charsRed
      } else {
        throw new IOException()
      }
    }
  }

  private def lastRedPos: Int = {
    readPos - 1
  }

  private def updateLineAndColumn(pos: Int, char: Char, prevChar: Char): Unit = {

    val newLine =
      prevChar match {
        case '\n' =>
          true
        case '\r' =>
          if (char == '\n') {
            false
          } else {
            true
          }
        case _ =>
          false
      }

    if (newLine) {
      posToLine(pos) = posToLine(pos - 1) + 1
      posToColumn(pos) = 1
    } else {
      posToLine(pos) = posToLine(pos - 1)
      posToColumn(pos) = posToColumn(pos - 1) + 1
    }

    if (char == '\t') {
      posToColumn(pos) += -1 + (tabSize - (posToColumn(pos) % tabSize))
    }
  }

  /** Returns the next character from the selected input. The method of selecting the input is the responsibility of the
    * class implementing this interface. Can throw any java.io.IOException.
    */
  override def readChar(): Char = {
    fillBuffer()
    val char     = inputBuffer(readPos)
    val prevChar = inputBuffer(lastRedPos)
    updateLineAndColumn(readPos, char, prevChar)
    readPos += 1
    char
  }

  /** Returns the column position of the character last read.
    *
    * @deprecated
    * @see
    *   #getEndColumn
    */
  override def getColumn: Int = ???

  /** Returns the line number of the character last read.
    *
    * @deprecated
    * @see
    *   #getEndLine
    */
  override def getLine: Int = ???

  /** Returns the column number of the last character for current token (being matched after the last call to
    * BeginTOken).
    */
  override def getEndColumn: Int = {
    posToColumn(lastRedPos)
  }

  /** Returns the line number of the last character for current token (being matched after the last call to BeginTOken).
    */
  override def getEndLine: Int = {
    posToLine(lastRedPos)
  }

  /** Returns the column number of the first character for current token (being matched after the last call to
    * BeginTOken).
    */
  override def getBeginColumn: Int = {
    posToColumn(tokenBeginPos)
  }

  /** Returns the line number of the first character for current token (being matched after the last call to
    * BeginTOken).
    */
  override def getBeginLine: Int = {
    posToLine(tokenBeginPos)
  }

  /** Backs up the input stream by amount steps. Lexer calls this method if it had already read some characters, but
    * could not use them to match a (longer) token. So, they will be used again as the prefix of the next token and it
    * is the implementation's responsibility to do this right.
    */
  override def backup(amount: Int): Unit = {
    readPos -= amount
  }

  /** Returns the next character that marks the beginning of the next token. All characters must remain in the buffer
    * between two successive calls to this method to implement backup correctly.
    */
  override def BeginToken(): Char = {
    tokenBeginPos = readPos
    readChar()
  }

  /** Returns a string made up of characters from the marked token beginning to the current buffer position.
    * Implementations have the choice of returning anything that they want to. For example, for efficiency, one might
    * decide to just return null, which is a valid implementation.
    */
  override def GetImage(): String = {
    new String(inputBuffer, tokenBeginPos, readPos - tokenBeginPos)
  }

  /** Returns an array of characters that make up the suffix of length 'len' for the currently matched token. This is
    * used to build up the matched string for use in actions in the case of MORE. A simple and inefficient
    * implementation of this is as follows :
    *
    * { String t = GetImage(); return t.substring(t.length() - len, t.length()).toCharArray(); }
    */
  override def GetSuffix(len: Int): Array[Char] = {
    val suffix = new Array[Char](len)
    Array.copy(inputBuffer, readPos - len, suffix, 0, len)
    suffix
  }

  /** The lexer calls this function to indicate that it is done with the stream and hence implementations can free any
    * resources held by this class. Again, the body of this function can be just empty and it will not affect the
    * lexer's operation.
    */
  override def Done(): Unit = {
    inputReader.close()
  }

  override def setTabSize(i: Int): Unit = {
    tabSize = i
  }

  override def getTabSize: Int = {
    tabSize
  }

  override def getTrackLineColumn: Boolean = ???

  override def setTrackLineColumn(trackLineColumn: Boolean): Unit = ???
}
