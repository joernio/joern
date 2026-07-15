package io.joern.pysrc2cpg

import io.joern.pythonparser.ast

import scala.collection.mutable

type ClassOrFunctionDef = ast.ClassDef | ast.FunctionDef | ast.AsyncFunctionDef

/** Traverses a module and, per Python scope (module / function / class body), records the source-order index of every
  * ClassDef, FunctionDef or AsyncFunctionDef whose simple name is redefined within that scope. Classes and functions
  * are bucketed separately: a `def A` and a `class A` in the same scope do not shadow each other in the map. The LAST
  * occurrence per (kind, name) gets no entry (keeps the plain full name); earlier occurrences get 0, 1, 2, ...
  *
  * The suffix is also propagated to `contextStack` when entering the class body, so members (methods, nested classes)
  * of a mangled class receive fullNames prefixed with the mangled class name. Example:
  * {{{
  *   class A:
  *     def m(self): ...     # fullName: Test.py:<module>.A<redefined>0.m
  *   class A:
  *     def m(self): ...     # fullName: Test.py:<module>.A.m
  * }}}
  * Consumers querying by class simple name via `typeDecl.name("A").method` still find both, but queries pinned on full
  * name must account for the `<redefined>N` marker.
  *
  * At runtime Python binds the class name to the most recent `class` statement, so callers that reference the name
  * (e.g. `A()`) resolve to the last definition. Keeping that definition's fullName plain (unsuffixed) means: (1)
  * existing queries against the CPG that assume one class per name still land on the runtime-effective definition, and
  * (2) the earlier, shadowed definitions get disambiguated with `<redefined>0`, `<redefined>1`, ... in source order so
  * they remain addressable for taint tracking and static queries.
  */
class RedefinitionCalculator {
  private val defToRedefinedIndex = mutable.Map[ClassOrFunctionDef, Int]()

  private val scopeStack =
    mutable.Stack[mutable.LinkedHashMap[String, mutable.ArrayBuffer[ClassOrFunctionDef]]]()

  def calculate(module: ast.Module): Map[ClassOrFunctionDef, Int] = {
    pushScope()
    module.stmts.foreach(visitStmt)
    popScopeAndAssignIndices()
    defToRedefinedIndex.toMap
  }

  private def pushScope(): Unit =
    scopeStack.push(mutable.LinkedHashMap.empty)

  private def popScopeAndAssignIndices(): Unit = {
    val scope = scopeStack.pop()
    scope.values.foreach { mixedDefs =>
      val (classes, functions) = mixedDefs.partition(_.isInstanceOf[ast.ClassDef])
      List(classes, functions).foreach { defs =>
        if (defs.length > 1) {
          defs.zipWithIndex.init.foreach { case (cd, idx) =>
            defToRedefinedIndex(cd) = idx
          }
        }
      }
    }
  }

  private def recordDef(definition: ClassOrFunctionDef): Unit = {
    val name = definition match {
      case cd: ast.ClassDef         => cd.name
      case fd: ast.FunctionDef      => fd.name
      case fd: ast.AsyncFunctionDef => fd.name
    }
    scopeStack.top.getOrElseUpdate(name, mutable.ArrayBuffer.empty) += definition
  }

  private def visitStmts(stmts: Iterable[ast.istmt]): Unit =
    stmts.foreach(visitStmt)

  private def visitScopeBody(body: Iterable[ast.istmt]): Unit = {
    pushScope()
    visitStmts(body)
    popScopeAndAssignIndices()
  }

  private def visitLoop(body: Iterable[ast.istmt], orelse: Iterable[ast.istmt]): Unit = {
    visitStmts(body)
    visitStmts(orelse)
  }

  private def visitStmt(stmt: ast.istmt): Unit = stmt match {
    case cd: ast.ClassDef =>
      recordDef(cd)
      pushScope()
      visitStmts(cd.body)
      popScopeAndAssignIndices()
    case fd: ast.FunctionDef      => 
      recordDef(fd)
      visitScopeBody(fd.body)
    case fd: ast.AsyncFunctionDef =>
      recordDef(fd)
      visitScopeBody(fd.body)
    case ifStmt: ast.If =>
      visitStmts(ifStmt.body)
      visitStmts(ifStmt.orelse)
    case forStmt: ast.For      => visitLoop(forStmt.body, forStmt.orelse)
    case forStmt: ast.AsyncFor => visitLoop(forStmt.body, forStmt.orelse)
    case whileStmt: ast.While =>
      visitStmts(whileStmt.body)
      visitStmts(whileStmt.orelse)
    case withStmt: ast.With      => visitStmts(withStmt.body)
    case withStmt: ast.AsyncWith => visitStmts(withStmt.body)
    case tryStmt: ast.Try =>
      visitStmts(tryStmt.body)
      tryStmt.handlers.foreach(handler => visitStmts(handler.body))
      visitStmts(tryStmt.orelse)
      visitStmts(tryStmt.finalbody)
    case matchStmt: ast.Match =>
      matchStmt.cases.foreach(mc => visitStmts(mc.body))
    case _ =>
  }
}
