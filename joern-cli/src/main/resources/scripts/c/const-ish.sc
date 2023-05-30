@main def main() = {
  cpg.method.internal.filter { method =>
    method.start.assignment.target
      .reachableBy(method.parameter.filter(_.code.contains("const")))
      .nonEmpty
  }.toSetImmutable
}
