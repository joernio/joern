package io.joern

import flatgraph.help.Table.AvailableWidthProvider
import replpp.Operators.*
import replpp.Colors

package object console {

  implicit val defaultAvailableWidthProvider: AvailableWidthProvider =
    io.shiftleft.semanticcpg.defaultAvailableWidthProvider

  // TODO remove any time after the end of 2023 - this is completely deprecated
  implicit class UnixUtils[A](content: Iterable[A]) {
    given Colors = Colors.Default

    /** Iterate over left hand side operand and write to file. Think of it as the Ocular version of the Unix `>` shell
      * redirection.
      */
    @deprecated("please use `#>` instead", "2.0.45 (August 2023)")
    def |>(outfile: String): Unit =
      content #> outfile

    /** Iterate over left hand side operand and append to file. Think of it as the Ocular version of the Unix `>>` shell
      * redirection.
      */
    @deprecated("please use `#>>` instead", "2.0.45 (August 2023)")
    def |>>(outfile: String): Unit =
      content #>> outfile
  }

  implicit class StringOps(value: String) {
    given Colors = Colors.Default

    /** Pipe string to file. Think of it as the Ocular version of the Unix `>` shell redirection.
      */
    @deprecated("please use `#>` instead", "2.0.45 (August 2023)")
    def |>(outfile: String): Unit =
      value #> outfile

    /** Append string to file. Think of it as the Ocular version of the Unix `>>` shell redirection.
      */
    @deprecated("please use `#>>` instead", "2.0.45 (August 2023)")
    def |>>(outfile: String): Unit =
      value #>> outfile
  }

}
