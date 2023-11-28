def assertContains[A](name: String, actual: IterableOnce[A], expected: IterableOnce[A]) = {
  val actualSet = actual.iterator.to(Set)

  val missing = expected.iterator.filterNot(actualSet.contains).toSeq
  if (missing.nonEmpty)
    throw new AssertionError(s"""$name did not contain the following expected element(s):
         |$missing
         |Actual elements:
         |${actual.iterator.mkString(System.lineSeparator)}""".mkString) with scala.util.control.NoStackTrace
}
