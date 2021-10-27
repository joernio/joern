package io.shiftleft.console.embammonite

import ammonite.util.Colors
import org.slf4j.{Logger, LoggerFactory}

import java.io.{BufferedReader, InputStreamReader, PipedInputStream, PipedOutputStream, PrintWriter}
import java.util.UUID
import java.util.concurrent.{BlockingQueue, LinkedBlockingQueue, Semaphore}

/**
  * Result of executing a query, containing in particular
  * output received on standard out and on standard error.
  * */
class QueryResult(val out: String, val err: String, val uuid: UUID)

private[embammonite] case class Job(uuid: UUID, query: String, observer: QueryResult => Unit)

class EmbeddedAmmonite(predef: String = "") {

  import EmbeddedAmmonite.logger

  val jobQueue: BlockingQueue[Job] = new LinkedBlockingQueue[Job]()

  val (inStream, toStdin) = pipePair()
  val (fromStdout, outStream) = pipePair()
  val (fromStderr, errStream) = pipePair()

  val writer = new PrintWriter(toStdin)
  val reader = new BufferedReader(new InputStreamReader(fromStdout))
  val errReader = new BufferedReader(new InputStreamReader(fromStderr))

  val userThread = new Thread(new UserRunnable(jobQueue, writer, reader, errReader))

  val shellThread = new Thread(() => {
    val ammoniteShell =
      ammonite
        .Main(
          predefCode = EmbeddedAmmonite.predef + predef,
          welcomeBanner = None,
          remoteLogging = false,
          colors = Colors.BlackWhite,
          inputStream = inStream,
          outputStream = outStream,
          errorStream = errStream
        )
    ammoniteShell.run()
  })

  private def pipePair(): (PipedInputStream, PipedOutputStream) = {
    val out = new PipedOutputStream()
    val in = new PipedInputStream()
    in.connect(out)
    (in, out)
  }

  /**
    * Start the embedded ammonite shell
    * */
  def start(): Unit = {
    shellThread.start()
    userThread.start()
  }

  /**
    * Submit query `q` to shell and call `observer` when
    * the result is ready.
    * */
  def queryAsync(q: String)(observer: QueryResult => Unit): UUID = {
    val uuid = UUID.randomUUID()
    jobQueue.add(Job(uuid, q, observer))
    uuid
  }

  /**
    * Submit query `q` to the shell and return result.
    * */
  def query(q: String): QueryResult = {
    val mutex = new Semaphore(0)
    var result: QueryResult = null
    queryAsync(q) { r =>
      result = r
      mutex.release()
    }
    mutex.acquire()
    result
  }

  /**
    * Shutdown the embedded ammonite shell and
    * associated threads.
    * */
  def shutdown(): Unit = {
    shutdownShellThread()
    logger.info("Shell terminated gracefully")
    shutdownWriterThread()

    def shutdownWriterThread(): Unit = {
      jobQueue.add(Job(null, null, null))
      userThread.join()
    }
    def shutdownShellThread(): Unit = {
      writer.println("exit")
      writer.close()
      shellThread.join()
    }
  }

}

object EmbeddedAmmonite {

  /* The standard frontend attempts to query /dev/tty
      in multiple places, e.g., to query terminal dimensions.
      This does not work in intellij tests
      (see https://github.com/lihaoyi/Ammonite/issues/276)
      The below hack overrides the default frontend with
      a custom frontend that does not require /dev/tty.
      This also enables us to disable terminal echo
      by passing a `displayTransform` that returns
      an empty string on all input.
   */

  val predef: String =
    """class CustomFrontend extends ammonite.repl.AmmoniteFrontEnd(ammonite.compiler.Parsers) {
      |  override def width = 65536
      |  override def height = 65536
      |
      |  override def readLine(reader: java.io.Reader,
      |                        output: java.io.OutputStream,
      |                        prompt: String,
      |                        colors: ammonite.util.Colors,
      |                        compilerComplete: (Int, String) => (Int, Seq[String], Seq[String]),
      |                        history: IndexedSeq[String]) = {
      |
      |  val writer = new java.io.OutputStreamWriter(output)
      |
      |  val multilineFilter = ammonite.terminal.Filter.action(
      |    ammonite.terminal.SpecialKeys.NewLine,
      |    ti => ammonite.compiler.Parsers.split(ti.ts.buffer.mkString).isEmpty) {
      |      case ammonite.terminal.TermState(rest, b, c, _) => ammonite.terminal.filters.BasicFilters.injectNewLine(b, c, rest)
      |    }
      |
      |  val allFilters = ammonite.terminal.Filter.merge(extraFilters, multilineFilter, ammonite.terminal.filters.BasicFilters.all)
      |
      |  new ammonite.terminal.LineReader(width, prompt, reader, writer, allFilters, displayTransform = { (_: Vector[Char], i: Int) => (fansi.Str(""), i) } )
      |  .readChar(ammonite.terminal.TermState(ammonite.terminal.LazyList.continually(reader.read()), Vector.empty, 0, ""), 0)
      | }
      |}
      |
      |repl.frontEnd() = new CustomFrontend()
      |""".stripMargin

  private val logger: Logger = LoggerFactory.getLogger(classOf[EmbeddedAmmonite])
}
