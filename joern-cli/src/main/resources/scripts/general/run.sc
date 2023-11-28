@main def main(inputPath: String) = {
  importCode(inputPath)

  if (!run.toString.contains("base"))
    throw new AssertionError(s"""base layer not applied...?
         |output of `run`:
         |$run""".mkString) with scala.util.control.NoStackTrace
}
