package io.shiftleft.resolver.client

import cats.Foldable
import cats.effect.kernel.DeferredSource
import cats.effect.std.{Dispatcher, Queue}
import cats.effect.{Async, Deferred, Resource}
import cats.syntax.all.*
import fs2.Stream
import org.http4s.client.websocket.*
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import scodec.bits.ByteVector

import java.net.URI
import java.net.http.{HttpClient, WebSocket}
import java.nio.ByteBuffer
import java.util.concurrent.{CompletableFuture, CompletionStage}

class WSClientImpl[F[_]: Async] extends WSClient[F] {
  private given Logger[F] = Slf4jLogger.getLogger[F]()

  private case class WebSocketQueuePair(webSocket: WebSocket,
                                        queue: Queue[F, Option[WSFrame]])

  private class ListenerImpl(queue: Queue[F, Option[WSFrame]],
                             dispatcher: Dispatcher[F]
                             ) extends WebSocket.Listener {
    private def dispatch[A](effect: F[A]): Unit = {
      dispatcher.unsafeRunAndForget(effect)
    }

    override def onPing(webSocket: WebSocket, message: ByteBuffer): CompletionStage[?] = {
      dispatch(queue.offer(Some(WSFrame.Ping(ByteVector(message)))))
      null
    }

    override def onClose(webSocket: WebSocket, statusCode: Int, reason: String): CompletionStage[?] = {
      dispatch(queue.offer(Some(WSFrame.Close(statusCode, reason))))
      dispatch(queue.offer(None))
      null
    }

    override def onText(webSocket: WebSocket, data: CharSequence, last: Boolean): CompletionStage[?] = {
      dispatch(queue.offer(Some(WSFrame.Text(data.toString, last))))
      null
    }

    override def onBinary(webSocket: WebSocket, data: ByteBuffer, last: Boolean): CompletionStage[?] = {
      dispatch(queue.offer(Some(WSFrame.Binary(ByteVector(data), last))))
      null
    }

  }

  private class WsConnectionSharedImpl(webSocket: WebSocket) {
    def send(wsf: WSFrame): F[Unit] = {
      wsf match {
        case ping: WSFrame.Ping =>
          futureToAsyncEffect(webSocket.sendPing(ping.data.toByteBuffer)).void
        case pong: WSFrame.Pong =>
          futureToAsyncEffect(webSocket.sendPong(pong.data.toByteBuffer)).void
        case close: WSFrame.Close =>
          futureToAsyncEffect(webSocket.sendClose(close.statusCode, close.reason)).void
        case text: WSFrame.Text =>
          futureToAsyncEffect(webSocket.sendText(text.data, text.last)).void
        case binary: WSFrame.Binary =>
          futureToAsyncEffect(webSocket.sendBinary(binary.data.toByteBuffer, binary.last)).void
      }
    }

    def sendMany[G[_] : Foldable, A <: WSFrame](wsfs: G[A]): F[Unit] = {
      wsfs.foldLeft(Async[F].unit){ case (effect, wsf) => effect.flatMap(_ => send(wsf))}
    }

    def subprotocol: Option[String] = {
      val protocol = webSocket.getSubprotocol
      if (protocol == "") {
        None
      } else {
        Option.apply(protocol)
      }
    }
  }

  private class WsConnectionHighLvlImpl(webSocket: WebSocket,
                                        queue: Queue[F, Option[WSFrame]],
                                        deferredCloseFrame: Deferred[F, WSFrame.Close]) extends WSConnectionHighLevel[F] {
    private val shared = new WsConnectionSharedImpl(webSocket)

    override def send(wsf: WSDataFrame): F[Unit] = {
      shared.send(wsf)
    }

    override def sendMany[G[_] : Foldable, A <: WSDataFrame](wsfs: G[A]): F[Unit] = {
      shared.sendMany(wsfs)
    }

    override def receive: F[Option[WSDataFrame]] = {
      queue.take.flatMap {
        case Some(frame) =>
          frame match {
            case _: WSFrame.Ping =>
              receive
            case _: WSFrame.Pong =>
              receive
            case close: WSFrame.Close =>
              deferredCloseFrame.complete(close) >> Async[F].pure(None)
            case frame: WSDataFrame =>
              Async[F].pure(Some(frame))
          }
        case None =>
          Async[F].pure(None)
      }
    }

    override def subprotocol: Option[String] = {
      shared.subprotocol
    }

    override def closeFrame: DeferredSource[F, WSFrame.Close] = {
      deferredCloseFrame
    }
  }

  private def futureToAsyncEffect[A](a: => CompletableFuture[A]): F[A] = {
    Async[F].fromCompletableFuture(Async[F].delay(a))
  }

  private def makeHttpClientResource(): Resource[F, HttpClient] = {
    Resource.make(Async[F].delay(HttpClient.newHttpClient()))(client => Async[F].delay {
      //Proper shutdown and close is only supported from java21 on which we
      //do not want to rely on yet.
      //client.shutdownNow()
      //client.close();
    })
  }

  private def makeWebSocketBuilder(httpClient: HttpClient, request: WSRequest): WebSocket.Builder = {
    val wsBuilder = httpClient.newWebSocketBuilder()
    request.headers.foreach { header =>
      wsBuilder.header(header.name.toString, header.value)
    }
    wsBuilder
  }

  private def makeWebSocketResource(wsBuilder: WebSocket.Builder,
                                    javaUri: URI,
                                    listener: ListenerImpl
                                   ): Resource[F, WebSocket] = {
    Resource.make(futureToAsyncEffect(wsBuilder.buildAsync(javaUri, listener)))(webSocket =>
      futureToAsyncEffect(webSocket.sendClose(4005, "")).void)
  }

  private def connectInternal(request: WSRequest): Resource[F, WebSocketQueuePair] = {
    for {
      dispatcher <- Dispatcher.sequential(await = true)
      httpClient <- makeHttpClientResource()
      queue <- Resource.eval(Queue.unbounded[F, Option[WSFrame]])
      wsBuilder = makeWebSocketBuilder(httpClient, request)
      javaUri = URI.create(request.uri.renderString)
      lister = new ListenerImpl(queue, dispatcher)
      webSocket <- makeWebSocketResource(wsBuilder, javaUri, lister)
    } yield WebSocketQueuePair(webSocket, queue)

  }

  // Not possible to implement with default java HttpClient implementation since into
  // always handles incoming ping and close frames.
  override def connect(request: WSRequest): Resource[F, WSConnection[F]] = ???

  override def connectHighLevel(request: WSRequest): Resource[F, WSConnectionHighLevel[F]] = {
    connectInternal(request).evalMap { pair =>
      for {
        deferredCloseFrame <- Deferred[F, WSFrame.Close]
        connection = WsConnectionHighLvlImpl(pair.webSocket, pair.queue, deferredCloseFrame)
      } yield connection
    }
  }
}
