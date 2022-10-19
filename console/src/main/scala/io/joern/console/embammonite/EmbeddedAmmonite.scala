package io.joern.console.embammonite

import dotty.tools.repl.State
import org.slf4j.{Logger, LoggerFactory}

import java.io.{BufferedReader, InputStream, InputStreamReader, PipedInputStream, PipedOutputStream, PrintStream, PrintWriter}
import java.util.UUID
import java.util.concurrent.{BlockingQueue, LinkedBlockingQueue, Semaphore}

/** Result of executing a query, containing in particular output received on standard out and on standard error.
  */
class QueryResult(val out: String, val err: String, val uuid: UUID) extends HasUUID

trait HasUUID {
  def uuid: UUID
}

private[embammonite] case class Job(uuid: UUID, query: String, observer: QueryResult => Unit)

class EmbeddedAmmonite(predef: String = "") {
  import EmbeddedAmmonite.logger

  val jobQueue: BlockingQueue[Job] = new LinkedBlockingQueue[Job]()

  val (inStream, toStdin)     = pipePair()
  val (fromStdout, outStream) = pipePair()
  val (fromStderr, errStream) = pipePair()

  val writer    = new PrintWriter(toStdin)
  val reader    = new BufferedReader(new InputStreamReader(fromStdout))
  val errReader = new BufferedReader(new InputStreamReader(fromStderr))

  val userThread = new Thread(new UserRunnable(jobQueue, writer, reader, errReader))

  val shellThread = new Thread(
    new Runnable {
      override def run(): Unit = {
        val inheritedClasspath = System.getProperty("java.class.path")
        val compilerArgs = Array(
          "-classpath", inheritedClasspath,
          "-explain", // verbose scalac error messages
          "-deprecation",
          "-color", "never"
        )

        val replDriver = new EmbeddedAmmonite.ReplDriver(compilerArgs, inStream, new PrintStream(outStream))
        val initialState: State = replDriver.initialState
        val state: State = initialState
        // TODO predef
//        val predefCode = predefPlus(additionalImportCode(config) ++ replConfig)
//        val state: State =
//          if (config.verbose) {
//            println(predefCode)
//            replDriver.run(predefCode)(using initialState)
//          } else {
//            replDriver.runQuietly(predefCode)(using initialState)
//          }

        replDriver.runUntilQuit(using state)()
      }
    })

  private def pipePair(): (PipedInputStream, PipedOutputStream) = {
    val out = new PipedOutputStream()
    val in  = new PipedInputStream()
    in.connect(out)
    (in, out)
  }

  /** Start the embedded ammonite shell
    */
  def start(): Unit = {
    shellThread.start()
    userThread.start()
  }

  /** Submit query `q` to shell and call `observer` when the result is ready.
    */
  def queryAsync(q: String)(observer: QueryResult => Unit): UUID = {
    val uuid = UUID.randomUUID()
    jobQueue.add(Job(uuid, q, observer))
    uuid
  }

  /** Submit query `q` to the shell and return result.
    */
  def query(q: String): QueryResult = {
    val mutex               = new Semaphore(0)
    var result: QueryResult = null
    queryAsync(q) { r =>
      result = r
      mutex.release()
    }
    mutex.acquire()
    result
  }

  /** Shutdown the embedded ammonite shell and associated threads.
    */
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
  private val logger: Logger = LoggerFactory.getLogger(classOf[EmbeddedAmmonite])

  import dotty.tools.dotc.core.Contexts.{Context, ContextBase, ContextState, FreshContext, ctx}
  import dotty.tools.repl.{AbstractFileClassLoader, CollectTopLevelImports, Newline, ParseResult, Parsed, Quit, State}
  import java.io.PrintStream
  import scala.annotation.tailrec

  class ReplDriver(args: Array[String],
                   in: InputStream,
                   out: PrintStream = scala.Console.out,
                   classLoader: Option[ClassLoader] = None) extends dotty.tools.repl.ReplDriver(args, out, classLoader) {
    val reader = new BufferedReader(new InputStreamReader(in))

    /** Run REPL with `state` until `:quit` command found
      * Main difference to the 'original': different greeting, trap Ctrl-c
      */
    override def runUntilQuit(using initialState: State = initialState)(): State = {
      /** Blockingly read a line, getting back a parse result */
      def readLine(state: State): ParseResult = {
        given Context = state.context

        try {
          val line = reader.readLine()
          ParseResult(line)(using state)
        } catch {
          case e =>
            e.printStackTrace()
            println(s"caught exception $e with msg=${e.getMessage} ^ - continuing anyway...")
            Newline
        }
      }

      @tailrec def loop(using state: State)(): State = {
        val res = readLine(state)
        if (res == Quit) state
        else loop(using interpret(res))()
      }

      try runBody {
        loop(using initialState)()
      }
    }
  }

}
