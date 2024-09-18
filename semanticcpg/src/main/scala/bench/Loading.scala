package bench

import io.shiftleft.semanticcpg.typeinfo.*
import org.openjdk.jmh.annotations.*

import java.util.concurrent.TimeUnit
import scala.util.Random

object DataGen {
  Random.setSeed(1401204)
  private def shouldGenVersion(): Boolean = Random.nextInt(10) <= 6
  private def genLength(bnd: Int): Int = Random.nextInt(bnd)
  private def genName(): String = Random.nextString(8 + genLength(18))
  private def genFqName(): String = Random.nextString(40 + genLength(120))
  private def genSignature(): String = Random.nextString(10 + genLength(30))
  private def genTypeParam(): String = Random.nextString(1 + genLength(4))
  private def genList[T](n: Int, gen: () => T): List[T] =
    if n == 0 then List() else gen() :: genList(n - 1, gen)
  private def genVersion(): Option[String] = Some(s"${Random.nextInt(1000)}.${Random.nextInt(1000)}.${Random.nextInt(1000)}")
  private def genDependency(): Dependency =
    Dependency(fullName = genFqName(), version = if shouldGenVersion() then genVersion() else None)
  private def genMember(): Member =
    Member(name = genName(), typeFullName = genFqName())
  private def genMethod(): Method =
    Method(name = genName(), fullName = genFqName(), signature = genSignature())
  def genTypeDecl(): TypeDecl =
    TypeDecl(
      fullName = genFqName(),
      name = genName(),
      typeParams = genList(Random.nextInt(20), genTypeParam),
      methods = genList(Random.nextInt(1000), genMethod),
      members = genList(Random.nextInt(1000), genMember),
      dependencies = genList(Random.nextInt(500), genDependency),
      inherits = genList(Random.nextInt(100), genFqName)
    )
  def genTypeDecls(n: Int): Array[TypeDecl] = Array.fill(n){genTypeDecl()}
}
  
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
class Loading {
  val n = 100
  val types = DataGen.genTypeDecls(n)
  val ionStrings = types.map(IonWriter.writeToString)
  val jsonStrings = types.map(JsonWriter.writeToString)
  
//  @Setup
//  def prepare: Unit = 
//    currentString = IonWriter.writeToString(DataGen.genTypeDecl())
//  
//  @TearDown
//  def check: Unit = assert(result.isRight)
//  
//  @Benchmark 
//  def run: Unit = 
//    result = IonTypeLoader.parse(currentString)
}
