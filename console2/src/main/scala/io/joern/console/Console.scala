package io.joern.console

import dotty.tools.repl._

object Console:

  def foo: Int =
    42

  def main(args: Array[String]): Unit =
    // pass classpath on into the repl
    val cp = System.getProperty("java.class.path")
    // val cpPlus = s"$cp:/home/mp/.ivy2/cache/org.scala-lang.modules/scala-xml_3/bundles/scala-xml_3-2.0.1.jar"
    // val cpPlus = s"$cp:target/foo.jar"
    // println(s"passing -classpath $cpPlus")
    println("--------------------------------")
    val replArgs = args ++ Array("-classpath", cp)
    new ReplDriver(replArgs).tryRunning
