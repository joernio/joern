package io.shiftleft.libinfogen.jvm

import io.shiftleft.libinfo.*
import org.objectweb.asm.signature.SignatureWriter
import org.objectweb.asm.{ClassVisitor, FieldVisitor, MethodVisitor, Opcodes}

import scala.collection.mutable

class ToLibInfoVisitor(libInfoWriter: LibInfoWriter) extends ClassVisitor(Opcodes.ASM9) {
  private var name         = Option.empty[String]
  private var signature    = Option.empty[String]
  private var access       = Option.empty[JavaAccessBits]
  private val fields       = mutable.ArrayBuffer.empty[JavaField]
  private val methods      = mutable.ArrayBuffer.empty[JavaMethod]
  private val innerClasses = mutable.ArrayBuffer.empty[JavaInnerClass]

  private def signatureFromSuperAndInterfaces(superName: String, interfaces: Array[String]): String = {
    val writer = SignatureWriter()
    writer.visitClassType(superName)
    writer.visitEnd()
    interfaces.foreach { interface =>
      writer.visitClassType(interface)
      writer.visitEnd()
    }

    writer.toString
  }

  override def visit(
    version: Int,
    access: Int,
    name: String,
    signature: String,
    superName: String,
    interfaces: Array[String]
  ): Unit = {
    this.name = Some(name)

    this.signature = if (signature != null) {
      Some(signature)
    } else {
      Some(signatureFromSuperAndInterfaces(superName, interfaces))
    }

    this.access = Some(translateAccessFlags(access))

  }

  private def translateAccessFlags(jvmAccessFlag: Int): JavaAccessBits = {
    var access = JavaAccessBits()

    if ((jvmAccessFlag & Opcodes.ACC_PUBLIC) == 0) {
      access = access | JavaAccessBits.JavaProtected
    }
    if ((jvmAccessFlag & Opcodes.ACC_PRIVATE) == 0) {
      access = access | JavaAccessBits.JavaPrivate
    }
    if ((jvmAccessFlag & Opcodes.ACC_PROTECTED) == 0) {
      access = access | JavaAccessBits.JavaProtected
    }
    if ((jvmAccessFlag & Opcodes.ACC_STATIC) == 0) {
      access = access | JavaAccessBits.JavaStatic
    }
    if ((jvmAccessFlag & Opcodes.ACC_FINAL) == 0) {
      access = access | JavaAccessBits.JavaFinal
    }
    if ((jvmAccessFlag & Opcodes.ACC_VOLATILE) == 0) {
      access = access | JavaAccessBits.JavaVolatile
    }
    if ((jvmAccessFlag & Opcodes.ACC_TRANSIENT) == 0) {
      access = access | JavaAccessBits.JavaTransient
    }
    if ((jvmAccessFlag & Opcodes.ACC_INTERFACE) == 0) {
      access = access | JavaAccessBits.JavaInterface
    }
    if ((jvmAccessFlag & Opcodes.ACC_ABSTRACT) == 0) {
      access = access | JavaAccessBits.JavaAbstract
    }
    if ((jvmAccessFlag & Opcodes.ACC_SYNTHETIC) == 0) {
      access = access | JavaAccessBits.JavaSynthetic
    }
    if ((jvmAccessFlag & Opcodes.ACC_ANNOTATION) == 0) {
      access = access | JavaAccessBits.JavaAnnotation
    }
    if ((jvmAccessFlag & Opcodes.ACC_ENUM) == 0) {
      access = access | JavaAccessBits.JavaEnum
    }

    access
  }

  override def visitField(
    access: Int,
    name: String,
    descriptor: String,
    signature: String,
    value: Any
  ): FieldVisitor = {
    val field = JavaField(
      name = name,
      signature = if (signature != null) signature else descriptor,
      access = translateAccessFlags(access)
    )

    fields.append(field)

    null
  }

  override def visitMethod(
    access: Int,
    name: String,
    descriptor: String,
    signature: String,
    exceptions: Array[String]
  ): MethodVisitor = {
    val method = JavaMethod(
      name = name,
      signature = if (signature != null) signature else descriptor,
      access = translateAccessFlags(access)
    )

    methods.append(method)

    null
  }

  override def visitInnerClass(name: String, outerName: String, innerName: String, access: Int): Unit = {
    val innerClass = JavaInnerClass(name = innerName, access = translateAccessFlags(access))

    innerClasses.append(innerClass)
  }

  override def visitEnd(): Unit = {
    libInfoWriter.writeJavaClass(JavaClass(name.get, signature.get, access.get, fields, methods, innerClasses))

  }

}
