package io.shiftleft.semanticcpg.typeinfo.dependencies

import io.shiftleft.semanticcpg.typeinfo.Version

sealed trait Constraint
case class Eq(str: Version) extends Constraint
case class Any() extends Constraint
case class Gt(constraint: Constraint) extends Constraint
case class Gte(constraint: Constraint) extends Constraint
case class Lt(constraint: Constraint) extends Constraint
case class Lte(constraint: Constraint) extends Constraint
case class And(left: Constraint, right: Constraint) extends Constraint
case class Or(left: Constraint, right: Constraint) extends Constraint
