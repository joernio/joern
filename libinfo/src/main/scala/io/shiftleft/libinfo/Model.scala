package io.shiftleft.libinfo

trait LibInfoElement

type Symbol   = String
type BitField = Int

// Java access bits analog to JVM byte code.
object JavaAccessBits {
  val JavaPublic     = 0x1
  val JavaPrivate    = 0x2
  val JavaProtected  = 0x4
  val JavaStatic     = 0x8
  val JavaFinal      = 0x10
  val JavaVolatile   = 0x40
  val JavaTransient  = 0x80
  val JavaInterface  = 0x200
  val JavaAbstract   = 0x400
  val JavaSynthetic  = 0x1000
  val JavaAnnotation = 0x2000
  val JavaEnum       = 0x4000
}

case class JavaClass(
  name: Symbol,
  signature: Symbol,
  access: BitField,
  fields: collection.Seq[JavaField],
  methods: collection.Seq[JavaMethod],
  innerClasses: collection.Seq[JavaInnerClass]
) extends LibInfoElement

case class JavaField(name: Symbol, typeRef: Symbol, access: BitField)

case class JavaMethod(name: Symbol, signature: Symbol, access: BitField)

case class JavaInnerClass(name: Symbol, access: BitField)
