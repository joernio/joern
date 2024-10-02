package bench

import io.shiftleft.semanticcpg.typeinfo.*
import io.shiftleft.semanticcpg.typeinfo.util.DataGen
import org.openjdk.jmh.annotations.*

import java.util.concurrent.TimeUnit
import scala.util.{Random, Try}
  
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
class Loading {
//  Random.setSeed(1401204)
//  val n = 40
//  val types = DataGen.genTypeDecls(n)
//  val ionStrings = types.map(IonWriter.writeToString).filter(_.isSuccess).map(_.get)
//  val ionBinaries = types.map(IonWriter.writeToBinaryFormat).filter(_.isSuccess).map(_.get)
//  assert(ionStrings.length == n)
//  assert(ionBinaries.length == n)
//  
//  @Benchmark 
//  def ionLoadFromString = ionStrings.foreach(IonTextTypeInfoLoader.loadFromString)
//  
  // Commented out now because the interface has changed and this function is not implemented
//  @Benchmark
//  def ionLoadFromBinaryFormat = ionBinaries.foreach(IonTextTypeInfoLoader.loadFromBytes)
}
