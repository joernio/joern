/** originally from amm/compiler/src/main/scala-3/ammonite/compiler/AsmPositionUpdater.scala  */
package ammonite.foo

// originally based on https://github.com/VirtusLab/scala-cli/blob/67db91cfbaa74de806fd1d5ed00096affa0125c0/modules/build/src/main/scala/scala/build/postprocessing/AsmPositionUpdater.scala

import org.objectweb.asm
import java.io.InputStream

object AsmPositionUpdater {

  private class LineNumberTableMethodVisitor(
    lineShift: Int,
    delegate: asm.MethodVisitor
  ) extends asm.MethodVisitor(asm.Opcodes.ASM9, delegate) {
    override def visitLineNumber(line: Int, start: asm.Label): Unit =
      super.visitLineNumber(line + lineShift, start)
  }

  private class LineNumberTableClassVisitor(
    mappings: Map[String, (String, Int)],
    cw: asm.ClassWriter
  ) extends asm.ClassVisitor(asm.Opcodes.ASM9, cw) {
    private var lineShiftOpt = Option.empty[Int]
    def mappedStuff          = lineShiftOpt.nonEmpty
    override def visitSource(source: String, debug: String): Unit =
      mappings.get(source) match {
        case None =>
          super.visitSource(source, debug)
        case Some((newSource, lineShift)) =>
          lineShiftOpt = Some(lineShift)
          super.visitSource(newSource, debug)
      }
    override def visitMethod(
      access: Int,
      name: String,
      descriptor: String,
      signature: String,
      exceptions: Array[String]
    ): asm.MethodVisitor = {
      val main = super.visitMethod(access, name, descriptor, signature, exceptions)
      lineShiftOpt match {
        case None            => main
        case Some(lineShift) => new LineNumberTableMethodVisitor(lineShift, main)
      }
    }
  }

  def postProcess(
    mappings: Map[String, (String, Int)],
    clsInputStream: InputStream
  ): Option[Array[Byte]] = {
    val reader  = new asm.ClassReader(clsInputStream)
    val writer  = new asm.ClassWriter(reader, 0)
    val checker = new LineNumberTableClassVisitor(mappings, writer)
    reader.accept(checker, 0)
    if (checker.mappedStuff) Some(writer.toByteArray)
    else None
  }
}
