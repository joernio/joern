package bench

import io.shiftleft.semanticcpg.typeinfo.*
import io.shiftleft.semanticcpg.typeinfo.util.DataGen
import org.openjdk.jmh.annotations.*

import java.util.concurrent.TimeUnit
import scala.util.Random

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
class Writing {
//  Random.setSeed(1401204)
//  val n = 40
//  val types = DataGen.genTypeDecls(n)
//
//  @Benchmark
//  def ionWriteToString = types.foreach(IonWriter.writeToString)
//  
//  @Benchmark
//  def ionWriteToBinaryFormat = types.foreach(IonWriter.writeToBinaryFormat)
}