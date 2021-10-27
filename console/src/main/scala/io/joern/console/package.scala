package io.shiftleft

import better.files._

package object console {

  implicit class UnixUtils[A](content: Iterable[A]) {

    /**
      * Iterate over left hand side operand
      * and write to file. Think of it as the
      * Ocular version of the Unix `>` shell redirection.
      * */
    def |>(outfile: String): Unit =
      File(outfile).write(content.mkString("\n"))

    /**
      * Iterate over left hand side operand
      * and append to file. Think of it as the Ocular
      * version of the Unix `>>` shell redirection.
      * */
    def |>>(outfile: String): Unit =
      File(outfile).append("\n").append(content.mkString("\n"))
  }

  implicit class StringOps(value: String) {

    /**
      * Pipe string to file. Think of it as the Ocular version
      * of the Unix `>` shell redirection.
      * */
    def |>(outfile: String): Unit =
      File(outfile).write(value)

    /**
      * Append string to file. Think of it as the Ocular
      * version of the Unix `>>` shell redirection.
      * */
    def |>>(outfile: String): Unit =
      File(outfile).append("\n").append(value)
  }

}
