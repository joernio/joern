package bench

import io.shiftleft.semanticcpg.typeinfo.*
import org.openjdk.jmh.annotations.*

import java.util.concurrent.TimeUnit
import scala.util.Try
  
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
class Loading {
  val n = 40
  val types = DataGen.genTypeDecls(n)
  val ionStrings = types.map(IonWriter.writeToString).filter(_.isSuccess).map(_.get)
  val jsonStrings = types.map(JsonWriter.writeToString).filter(_.isSuccess).map(_.get)
  val ionBinaries = types.map(IonWriter.writeToBinaryFormat).filter(_.isSuccess).map(_.get)
  val jsonBinaries = types.map(JsonWriter.writeToBinaryFormat).filter(_.isSuccess).map(_.get)
  assert(ionStrings.length == n)
  assert(jsonStrings.length == n)
  assert(ionBinaries.length == n)
  assert(jsonBinaries.length == n)
  
  @Benchmark 
  def ionLoadFromString = ionStrings.foreach(IonLoader.parse)
    
  @Benchmark
  def jsonLoadFromString = jsonStrings.foreach(JsonLoader.parse)

  @Benchmark
  def ionLoadFromBinaryFormat = ionBinaries.foreach(IonLoader.parse)

  @Benchmark
  def jsonLoadFromBinaryFormat = jsonBinaries.foreach(JsonLoader.parse)
}
