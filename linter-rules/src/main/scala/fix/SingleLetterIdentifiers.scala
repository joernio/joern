package fix

import scalafix.v1._

import scala.meta._

object SingleLetterIdentifiers {

  /** Allowlist for short names that are acceptable in specific contexts.
   *  E.g., a simple counter variable in index-based loops.
    */
  private val allowedIdentifiers: Set[String] = Set("_", "i", "j", "k")

  private def isForbidden(name: String): Boolean =
    name.length == 1 && !allowedIdentifiers.contains(name)

  private def lint(name: String, pos: Position): Patch = {
    Patch.lint(
      Diagnostic(
        id = "SingleLetterIdentifier",
        message = s"Avoid single-letter identifier '$name'. Use a more descriptive name.",
        position = pos
      )
    )
  }

}

class SingleLetterIdentifiers extends SyntacticRule("SingleLetterIdentifiers") {

  override def isLinter = true

  override def fix(implicit doc: SyntacticDocument): Patch = {
    val patches = doc.tree.collect {

      // method / constructor / lambda params
      case param: Term.Param
          if SingleLetterIdentifiers.isForbidden(param.name.value) =>
        SingleLetterIdentifiers.lint(param.name.value, param.name.pos)

      // val x = ...
      case v: Defn.Val =>
        Patch.fromIterable(
          v.pats.collect {
            case name: Pat.Var if SingleLetterIdentifiers.isForbidden(name.name.value) =>
              SingleLetterIdentifiers.lint(name.name.value, name.name.pos)
          }
        )

      // var x = ...
      case v: Defn.Var =>
        Patch.fromIterable(
          v.pats.collect {
            case name: Pat.Var if SingleLetterIdentifiers.isForbidden(name.name.value) =>
              SingleLetterIdentifiers.lint(name.name.value, name.name.pos)
          }
        )

      // for (x <- xs) yield ...
      case gen: Enumerator.Generator =>
        gen.pat match {
          case name: Pat.Var if SingleLetterIdentifiers.isForbidden(name.name.value) =>
            SingleLetterIdentifiers.lint(name.name.value, name.name.pos)
          case _ =>
            Patch.empty
        }

      // case x =>
      case c: Case =>
        c.pat match {
          case name: Pat.Var if SingleLetterIdentifiers.isForbidden(name.name.value) =>
            SingleLetterIdentifiers.lint(name.name.value, name.name.pos)
          case _ =>
            Patch.empty
        }
    }

    Patch.fromIterable(patches)
  }

}