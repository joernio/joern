package io.joern.pysrc2cpg

import io.joern.pythonparser.ast

import scala.collection.mutable

/** Traverses a module and, per Python scope (module / function / class body), records the source-order index of every
  * ClassDef whose simple name is duplicated within that scope. The LAST occurrence per name gets no entry (keeps the
  * plain full name); earlier occurrences get 0, 1, 2, ...
  *
  * The suffix is also propagated to `contextStack` when entering the class body, so members (methods, nested classes)
  * of a mangled class receive fullNames prefixed with the mangled class name. Example:
  * {{{
  *   class A:
  *     def m(self): ...     # fullName: Test.py:<module>.A<duplicate>0.m
  *   class A:
  *     def m(self): ...     # fullName: Test.py:<module>.A.m
  * }}}
  * Consumers querying by class simple name via `typeDecl.name("A").method` still find both, but queries pinned on full
  * name must account for the `<duplicate>N` marker.
  *
  * At runtime Python binds the class name to the most recent `class` statement, so callers that reference the name
  * (e.g. `A()`) resolve to the last definition. Keeping that definition's fullName plain (unsuffixed) means: (1)
  * existing queries against the CPG that assume one class per name still land on the runtime-effective definition, and
  * (2) the earlier, shadowed definitions get disambiguated with `<duplicate>0`, `<duplicate>1`, ... in source order so
  * they remain addressable for taint tracking and static queries.
  */
class ClassDefDuplicateCalculator {
  private val classDefToDuplicateIndex = mutable.Map[ast.ClassDef, Int]()

  private val scopeStack =
    mutable.Stack[mutable.LinkedHashMap[String, mutable.ArrayBuffer[ast.ClassDef]]]()

  def calculate(module: ast.Module): Map[ast.ClassDef, Int] = {
    pushScope()
    module.stmts.foreach(visitStmt)
    popScopeAndAssignIndices()
    classDefToDuplicateIndex.toMap
  }

  private def pushScope(): Unit =
    scopeStack.push(mutable.LinkedHashMap.empty)

  private def popScopeAndAssignIndices(): Unit = {
    val scope = scopeStack.pop()
    scope.values.foreach { defs =>
      if (defs.length > 1) {
        defs.zipWithIndex.init.foreach { case (cd, idx) =>
          classDefToDuplicateIndex(cd) = idx
        }
      }
    }
  }

  private def recordClassDef(cd: ast.ClassDef): Unit = {
    scopeStack.top.getOrElseUpdate(cd.name, mutable.ArrayBuffer.empty) += cd
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
      recordClassDef(cd)
      pushScope()
      visitStmts(cd.body)
      popScopeAndAssignIndices()
    case fd: ast.FunctionDef      => visitScopeBody(fd.body)
    case fd: ast.AsyncFunctionDef => visitScopeBody(fd.body)
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
