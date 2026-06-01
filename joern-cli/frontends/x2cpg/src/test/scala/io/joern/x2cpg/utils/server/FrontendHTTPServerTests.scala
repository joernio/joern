package io.joern.x2cpg.utils.server

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.net.http.HttpRequest.BodyPublishers
import java.net.http.HttpResponse.BodyHandlers
import java.util.concurrent.{CountDownLatch, Executors, TimeUnit}
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}

class FrontendHTTPServerTests extends AnyWordSpec with Matchers {

  /** Run `body` against a freshly-started server with the given handler, ensuring the server is stopped afterwards. */
  private def withServer[T](handler: Array[String] => Unit)(body: (FrontendHTTPServer, Int) => T): T = {
    val server = new FrontendHTTPServer(FrontendHTTPServer.singleThreadExecutor(), handler)
    val port   = server.startup()
    try body(server, port)
    finally server.stop()
  }

  /** Synchronous HTTP POST helper using the JDK client. */
  private def post(port: Int, body: String, method: String = "POST"): HttpResponse[String] = {
    val builder = HttpRequest
      .newBuilder()
      .uri(URI.create(s"http://localhost:$port/run"))
      .header("Content-Type", "application/x-www-form-urlencoded")

    val req = method match {
      case "POST" => builder.POST(BodyPublishers.ofString(body)).build()
      case "GET"  => builder.GET().build()
      case other  => builder.method(other, BodyPublishers.ofString(body)).build()
    }

    HttpClient.newHttpClient().send(req, BodyHandlers.ofString())
  }

  "FrontendHTTPServer.startup" should {

    "bind to a random available port and reject a second startup" in {
      val server = new FrontendHTTPServer(FrontendHTTPServer.singleThreadExecutor(), _ => ())
      val port   = server.startup()
      try {
        port should be > 0
        an[IllegalArgumentException] should be thrownBy server.startup()
      } finally server.stop()
    }
  }

  "FrontendHTTPServer.stop" should {

    "be a no-op when the server was never started" in {
      val server = new FrontendHTTPServer(FrontendHTTPServer.singleThreadExecutor(), _ => ())
      noException should be thrownBy server.stop()
    }

    "shut down the executor and stop accepting new connections" in {
      val executor = FrontendHTTPServer.singleThreadExecutor()
      val server   = new FrontendHTTPServer(executor, _ => ())
      server.startup()
      server.stop()
      executor.isShutdown shouldBe true
    }

    "be safe under repeated calls (at-most-once shutdown)" in {
      val server = new FrontendHTTPServer(FrontendHTTPServer.singleThreadExecutor(), _ => ())
      server.startup()
      server.stop()
      noException should be thrownBy server.stop()
      noException should be thrownBy server.stop()
    }

    "be safe when called concurrently from multiple threads" in {
      val server  = new FrontendHTTPServer(FrontendHTTPServer.cachedThreadPoolExecutor(), _ => ())
      val _       = server.startup()
      val starter = new CountDownLatch(1)
      val done    = new CountDownLatch(8)
      val pool    = Executors.newCachedThreadPool()
      try {
        (1 to 8).foreach { _ =>
          pool.submit(new Runnable {
            override def run(): Unit = {
              starter.await()
              server.stop()
              done.countDown()
            }
          })
        }
        starter.countDown()
        done.await(10, TimeUnit.SECONDS) shouldBe true
      } finally pool.shutdownNow()
    }

    "not deadlock when called while a handler is in flight" in {
      val handlerEntered = new CountDownLatch(1)
      val releaseHandler = new CountDownLatch(1)
      val executor       = FrontendHTTPServer.cachedThreadPoolExecutor()
      val server = new FrontendHTTPServer(
        executor,
        _ => {
          handlerEntered.countDown()
          releaseHandler.await()
        }
      )
      val port    = server.startup()
      val sender  = Executors.newSingleThreadExecutor()
      val stopper = Executors.newSingleThreadExecutor()
      try {
        sender.submit(new Runnable {
          override def run(): Unit = {
            try post(port, "input=/tmp/in&output=/tmp/out")
            catch { case _: Throwable => () }
          }
        })
        handlerEntered.await(10, TimeUnit.SECONDS) shouldBe true

        val stopFuture = stopper.submit(new Runnable {
          override def run(): Unit = server.stop()
        })
        // `executor.isShutdown` flips inside `stop()` after `shutdown()` and immediately before `awaitTermination`,
        // so by the time we observe it the stopper is parked exactly where the would-be deadlock would occur.
        // Releasing the handler now forces it through its `runningRequests -= 1` finally block, which must not be
        // blocked by the monitor stop() would have held in a buggy implementation.
        val deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(5)
        while (!executor.isShutdown && System.nanoTime() < deadline) {
          Thread.`yield`()
        }
        executor.isShutdown shouldBe true
        releaseHandler.countDown()
        stopFuture.get(15, TimeUnit.SECONDS)
      } finally {
        releaseHandler.countDown()
        sender.shutdownNow()
        stopper.shutdownNow()
      }
    }
  }

  "FrontendHTTPServer.stopServerAfterTimeout" should {

    "shut down once no requests have been served for the timeout window" in {
      val server = new FrontendHTTPServer(FrontendHTTPServer.cachedThreadPoolExecutor(), _ => ())
      server.startup()
      val timer = Executors.newSingleThreadExecutor()
      try {
        val started = System.nanoTime()
        val f = timer.submit(new Runnable {
          override def run(): Unit = server.stopServerAfterTimeout(1)
        })
        f.get(10, TimeUnit.SECONDS)
        val elapsedMs = TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - started)
        // Lower bound is intentionally below the 1s timeout to tolerate scheduler jitter on slow CI runners
        // (some JVMs may return from wait() a few ms early). The upper bound is enforced by `f.get(10s)`.
        elapsedMs should be >= 800L
      } finally timer.shutdownNow()
    }

    "wake from wait() promptly when stop() is called externally" in {
      val server = new FrontendHTTPServer(FrontendHTTPServer.cachedThreadPoolExecutor(), _ => ())
      server.startup()
      val timer = Executors.newSingleThreadExecutor()
      try {
        val f = timer.submit(new Runnable {
          override def run(): Unit = server.stopServerAfterTimeout(60)
        })
        Thread.sleep(200)
        server.stop()
        f.get(5, TimeUnit.SECONDS)
      } finally timer.shutdownNow()
    }
  }

  "Concurrent requests" should {

    "be tracked correctly so that stopServerAfterTimeout waits for them" in {
      val inFlight       = new AtomicInteger(0)
      val handlerEntered = new CountDownLatch(2)
      val releaseHandler = new CountDownLatch(1)
      val server = new FrontendHTTPServer(
        FrontendHTTPServer.cachedThreadPoolExecutor(),
        _ => {
          inFlight.incrementAndGet()
          handlerEntered.countDown()
          releaseHandler.await()
          inFlight.decrementAndGet()
          ()
        }
      )
      val port    = server.startup()
      val senders = Executors.newCachedThreadPool()
      try {
        (1 to 2).foreach { _ =>
          senders.submit(new Runnable {
            override def run(): Unit = {
              try post(port, "input=/tmp/in&output=/tmp/out")
              catch { case _: Throwable => () }
            }
          })
        }
        handlerEntered.await(10, TimeUnit.SECONDS) shouldBe true
        inFlight.get() shouldBe 2
        releaseHandler.countDown()
      } finally {
        senders.shutdownNow()
        server.stop()
      }
    }
  }

  "RunHandler" should {

    "return 405 for non-POST requests" in {
      withServer(_ => ()) { (_, port) =>
        val resp = post(port, "", method = "GET")
        resp.statusCode() shouldBe 405
        resp.body() shouldBe "Method Not Allowed"
      }
    }

    "invoke the handler with the parsed arguments and respond with 200 + the output path" in {
      val captured = new AtomicReference[Array[String]](Array.empty)
      withServer(args => captured.set(args)) { (_, port) =>
        val resp = post(port, "input=/tmp/in&output=/tmp/out&flag=")
        resp.statusCode() shouldBe 200
        resp.body() shouldBe "/tmp/out"
        val args = captured.get().toSet
        args should contain("/tmp/in")
        args should contain("--output")
        args should contain("/tmp/out")
        args should contain("--flag")
      }
    }

    "URL-decode keys and values in the form-encoded body" in {
      val captured = new AtomicReference[Array[String]](Array.empty)
      withServer(args => captured.set(args)) { (_, port) =>
        val resp = post(port, "input=%2Ftmp%2Fa%20b&output=%2Ftmp%2Fout")
        resp.statusCode() shouldBe 200
        resp.body() shouldBe "/tmp/out"
        captured.get().toSet should contain("/tmp/a b")
      }
    }

    "fall back to the default output path when none is provided" in {
      withServer(_ => ()) { (_, port) =>
        val resp = post(port, "input=/tmp/in")
        resp.statusCode() shouldBe 200
        resp.body() shouldBe io.joern.x2cpg.X2CpgConfig.defaultOutputPath
      }
    }

    "respond with 400 and the exception message when the handler throws" in {
      withServer(_ => throw new RuntimeException("boom")) { (_, port) =>
        val resp = post(port, "input=/tmp/in&output=/tmp/out")
        resp.statusCode() shouldBe 400
        resp.body() shouldBe "boom"
      }
    }

    "tolerate an empty request body" in {
      val captured = new AtomicReference[Array[String]](Array.empty)
      withServer(args => captured.set(args)) { (_, port) =>
        val resp = post(port, "")
        resp.statusCode() shouldBe 200
        captured.get() shouldBe empty
      }
    }
  }

  "FrontendHTTPClient" should {

    "round-trip a successful request to FrontendHTTPServer" in {
      val captured = new AtomicReference[Array[String]](Array.empty)
      withServer(args => captured.set(args)) { (_, port) =>
        val client = FrontendHTTPClient(port)
        val req    = client.buildRequest(("input", Some("/tmp/in")), ("output", Some("/tmp/out")))
        client.sendRequest(req).get shouldBe "/tmp/out"
        captured.get().toSet should contain("/tmp/in")
      }
    }

    "return a Failure when the handler throws" in {
      withServer(_ => throw new RuntimeException("nope")) { (_, port) =>
        val client = FrontendHTTPClient(port)
        val req    = client.buildRequest(("input", Some("/tmp/in")))
        val result = client.sendRequest(req)
        result.isFailure shouldBe true
        result.failed.get.getMessage should include("400")
        result.failed.get.getMessage should include("nope")
      }
    }
  }
}
