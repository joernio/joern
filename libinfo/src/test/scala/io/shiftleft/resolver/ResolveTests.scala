package io.shiftleft.resolver

import cats.Parallel
import cats.effect.*
import cats.syntax.all.*
import io.shiftleft.resolver.api.*
import io.shiftleft.resolver.impl.{DefaultResolver, FailMetaDataCalculator, IdGeneric, NaiveResolutionModel}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

class DummyMetaDataFetcher[F[_]: Sync](data: Map[Coordinate[IdGeneric], MetaData[IdGeneric]])
  extends MetaDataFetcher[F, IdGeneric] {

  override def fetch(deps: Vector[Coordinate[IdGeneric]]): F[(Vector[Coordinate[IdGeneric]], Vector[MetaData[IdGeneric]])] = {
    Sync[F].pure {
      val (hasMetaData, noMetaData) = deps.partition(data.contains)
      (noMetaData, hasMetaData.map(data.apply))
    }
  }
}

class ResolveTests extends AnyWordSpec with Matchers {
  "test1" in {
    val map = Map(
      Coordinate(IdGeneric("a"), "v1") -> MetaData(IdGeneric("a"), "v1", Vector(Coordinate(IdGeneric("b"), "v3"))),
      Coordinate(IdGeneric("b"), "v2") -> MetaData(IdGeneric("b"), "v2", Vector()),
      Coordinate(IdGeneric("b"), "v3") -> MetaData(IdGeneric("b"), "v3", Vector()),
    )
    val directDeps = Vector(
      Coordinate(IdGeneric("a"), "v1"),
      Coordinate(IdGeneric("b"), "v2"),
    )


    val metaDataFetcher = new DummyMetaDataFetcher[IO](map)
    val resolver = new DefaultResolver(metaDataFetcher, new FailMetaDataCalculator, new NaiveResolutionModel)

    import cats.effect.unsafe.IORuntime
    val resolvedDeps = resolver.resolve(directDeps).unsafeRunSync()(IORuntime.global)

    resolvedDeps should contain theSameElementsAs(
      Vector(
        Coordinate(IdGeneric("a"), "v1"),
        Coordinate(IdGeneric("b"), "v3")
      )
    )
  }
}
