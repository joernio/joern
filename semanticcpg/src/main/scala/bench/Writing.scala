package bench

import io.shiftleft.semanticcpg.typeinfo.*
import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
class Writing {
  val n = 40
  val types = DataGen.genTypeDecls(n)

  @Benchmark
  def ionWriteToString = types.foreach(IonWriter.writeToString)

  @Benchmark
  def jsonWriteToString = types.foreach(JsonWriter.writeToString)
  
  @Benchmark
  def ionWriteToBinaryFormat = types.foreach(IonWriter.writeToBinaryFormat)

  @Benchmark
  def jsonWriteToBinaryFormat = types.foreach(JsonWriter.writeToBinaryFormat)
}