/** originally from amm/compiler/src/main/scala/ammonite/compiler/CompilerUtil.scala  */
package ammonite.foo

object CompilerUtil {

  val ignoredSyms = Set(
    "package class-use",
    "object package-info",
    "class package-info"
  )
  val ignoredNames = Set(
    // Probably synthetic
    "<init>",
    "<clinit>",
    "$main",
    // Don't care about this
    "toString",
    // Behaves weird in 2.10.x, better to just ignore.
    "_"
  )

}
