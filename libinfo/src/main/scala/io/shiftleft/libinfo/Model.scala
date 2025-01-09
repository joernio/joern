package io.shiftleft.libinfo

trait LibInfoElement

opaque type JavaAccessBits = Int

// Java access bits analog to JVM byte code.
// But to be safe we do not allow direct initialization with a plain integer
// from outside this module to enforce bit by bit initialization.
object JavaAccessBits {
  val JavaPublic     = JavaAccessBits(0x1)
  val JavaPrivate    = JavaAccessBits(0x2)
  val JavaProtected  = JavaAccessBits(0x4)
  val JavaStatic     = JavaAccessBits(0x8)
  val JavaFinal      = JavaAccessBits(0x10)
  val JavaVolatile   = JavaAccessBits(0x40)
  val JavaTransient  = JavaAccessBits(0x80)
  val JavaInterface  = JavaAccessBits(0x200)
  val JavaAbstract   = JavaAccessBits(0x400)
  val JavaSynthetic  = JavaAccessBits(0x1000)
  val JavaAnnotation = JavaAccessBits(0x2000)
  val JavaEnum       = JavaAccessBits(0x4000)

  private[libinfo] def apply(bits: Int): JavaAccessBits = {
    bits
  }

  def apply(): JavaAccessBits = {
    0
  }

  extension (bits: JavaAccessBits) {
    inline def `|`(otherBits: JavaAccessBits): JavaAccessBits = {
      bits | otherBits
    }

    def isSet(otherBits: JavaAccessBits): Boolean = {
      (bits & otherBits) == 0
    }

    private[libinfo] def asInt: Int = {
      bits
    }
  }

}

case class JavaClass(
  name: String,
  signature: String,
  access: JavaAccessBits,
  fields: collection.Seq[JavaField],
  methods: collection.Seq[JavaMethod],
  innerClasses: collection.Seq[JavaInnerClass]
) extends LibInfoElement

case class JavaField(name: String, signature: String, access: JavaAccessBits)

case class JavaMethod(name: String, signature: String, access: JavaAccessBits)

case class JavaInnerClass(name: String, access: JavaAccessBits)
