package io.joern.console.embammonite

import dotty.tools.repl.State
import org.slf4j.{Logger, LoggerFactory}

import java.io.{BufferedReader, InputStream, InputStreamReader, PipedInputStream, PipedOutputStream, PrintStream, PrintWriter}
import java.util.UUID
import java.util.concurrent.{BlockingQueue, LinkedBlockingQueue, Semaphore}

/** Result of executing a query, containing in particular output received on standard out. */
case class QueryResult(out: String, uuid: UUID) extends HasUUID

trait HasUUID {
  def uuid: UUID
}

private[embammonite] case class Job(uuid: UUID, query: String, observer: QueryResult => Unit)

class EmbeddedAmmonite(predefCode: String = "", verbose: Boolean = false) {
  private val logger: Logger = LoggerFactory.getLogger(getClass)

  val jobQueue: BlockingQueue[Job] = new LinkedBlockingQueue[Job]()

  val (inStream, toStdin)     = pipePair()
  val (fromStdout, outStream) = pipePair()

  val writer    = new PrintWriter(toStdin)
  val reader    = new BufferedReader(new InputStreamReader(fromStdout))

  val userThread = new Thread(new UserRunnable(jobQueue, writer, reader, verbose))

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

        val replDriver = new ReplDriver(compilerArgs, inStream, new PrintStream(outStream))
        val initialState: State = replDriver.initialState
        val state: State =
          if (verbose) {
            println(predefCode)
            replDriver.run(predefCode)(using initialState)
          } else {
            replDriver.runQuietly(predefCode)(using initialState)
          }

        replDriver.runUntilQuit(state)
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
    logger.info("Trying to shutdown shell and writer thread")
    shutdownShellThread()
    logger.info("Shell terminated gracefully")
    shutdownWriterThread()
    logger.info("Writer thread terminated gracefully")

    def shutdownWriterThread(): Unit = {
      jobQueue.add(Job(null, null, null))
      userThread.join()
    }
    def shutdownShellThread(): Unit = {
      writer.println(":exit")
      writer.close()
      shellThread.join()
    }
  }

}
