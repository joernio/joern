@main def main(inputPath: String) = {
  importCode(inputPath)

  if (help.cpg.toString.isEmpty)
    throw new AssertionError("no help text available!") with scala.util.control.NoStackTrace
}
