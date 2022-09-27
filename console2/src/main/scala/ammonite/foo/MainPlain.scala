package ammonite.foo

import java.net.URL

object MainPlain {
  def main(args: Array[String]): Unit = {
    val printer = Printer(
      outStream = System.out,
      errStream = System.err,
      resultStream = System.out,
      warning = msg => println(s"Xwarn: $msg"),
      error = msg => println(s"Xerror: $msg"),
      info = msg => println(s"Xinfo: $msg")
    )
    val versionSortJar = "/home/mp/.cache/coursier/v1/https/repo1.maven.org/maven2/com/michaelpollmeier/versionsort/1.0.7/versionsort-1.0.7.jar"
    val versionSortJarUrl = new URL(s"file:$versionSortJar")

    val initialClassLoader = getClass.getClassLoader
    val initialClassPath = Classpath.classpath(initialClassLoader, None)

    val compiler0 = new Compiler(
      dynamicClassPath = dotty.tools.io.AbstractFile.getDirectory("."),
      initialClassPath = initialClassPath,
      classPath = initialClassPath,
      whiteList = Set.empty
    )
    val compiler1 = new Compiler(
      compiler0.dynamicClassPath,
      compiler0.initialClassPath,
      compiler0.classPath :+ versionSortJarUrl,
      compiler0.whiteList)
    val cmd0Src = """val foo = 42"""
    val compileResult0 = compiler0.compile(
      src = cmd0Src.getBytes("UTF-8"),
      printer,
      importsLen = 0,
      userCodeNestingLevel = 0,
      fileName = "cmd0.sc"
    )
    println(compileResult0.get) // foo defined successfully

    val cmd1Src =
      """val bar = foo
        |val baz = versionsort.VersionHelper.compare("1.0", "0.9")
        |""".stripMargin
    val compileResult1 = compiler1.compile(
      src = cmd1Src.getBytes("UTF-8"),
      printer,
      importsLen = 0,
      userCodeNestingLevel = 0,
      fileName = "cmd1.sc"
    )
    println(compileResult1.get) // bar|baz defined successfully :)
  }

}
