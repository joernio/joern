package io.joern.solidity2cpg.domain

/** Represents objects in the Surya JSON. This allows them to be easily grouped together and used in match statements.
  */
sealed trait SuryaObject

object SuryaObject {
  case class SourceUnit(children: List[SuryaObject]) extends SuryaObject

  case class PragmaDirective(name: String, value: String) extends SuryaObject

  // TODO: Extend for unit and symbol aliases
  case class ImportDirective(path: String) extends SuryaObject

  // TODO: Create the rest of these
}
