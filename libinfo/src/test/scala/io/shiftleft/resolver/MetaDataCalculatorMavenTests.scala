package io.shiftleft.resolver

import cats.effect.{IO, Resource, Sync}
import cats.effect.unsafe.IORuntime
import io.shiftleft.resolver.api.{Coordinate, Id, MetaData, MetaDataStore}
import io.shiftleft.resolver.impl.{IdMaven, MetaDataCalculatorLocal, MetaDataFetcherMaven}
import org.http4s.client.JavaNetClientBuilder
import org.http4s.client.middleware.FollowRedirect
import org.http4s.ember.client.EmberClientBuilder
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MetaDataStoreNull[F[_] : Sync, I <: Id] extends MetaDataStore[F, I] {

  override def store(metaDatas: Vector[MetaData[I]]): F[Unit] = {
    Sync[F].unit
  }
}

class MetaDataCalculatorMavenTests extends AnyWordSpec with Matchers {
  "test1" in {
    val dep = Vector(Coordinate(IdMaven("org.springframework.boot", "spring-boot"), "3.4.2"))

    val y =
    for {
      client <- EmberClientBuilder.default[IO].build.map(FollowRedirect(10))
      fetcher = new MetaDataFetcherMaven(client, "https://repo1.maven.org/maven2")
      store = new MetaDataStoreNull[IO, IdMaven]
      calculator = new MetaDataCalculatorLocal(fetcher, store)
      r <- Resource.eval(calculator.calculateMetaData(dep))
    } yield r

    y.use_.unsafeRunSync()(IORuntime.global)

  }
}
