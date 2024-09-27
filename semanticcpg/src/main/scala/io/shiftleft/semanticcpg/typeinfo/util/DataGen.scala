package io.shiftleft.semanticcpg.typeinfo.util

import io.shiftleft.semanticcpg.typeinfo.{Member, Method, TypeDecl}

import scala.util.Random

object DataGen {
  def genTypeDecl(): TypeDecl =
    TypeDecl(
      fullName = genFqName(),
      name = genName(),
      typeParams = genList(Random.nextInt(2), genTypeParam),
      methods = genList(Random.nextInt(100), genMethod),
      members = genList(Random.nextInt(100), genMember),
      inherits = genList(Random.nextInt(6), genFqName)
    )
  def genTypeDecls(n: Int): Array[TypeDecl] = Array.fill(n) { genTypeDecl() }

  private def shouldGenVersion(): Boolean = Random.nextInt(10) <= 6
  private def genLength(bnd: Int): Int    = Random.nextInt(bnd)
  private def genName(): String           = Random.nextString(8 + genLength(18))
  private def genFqName(): String         = Random.nextString(40 + genLength(120))
  private def genSignature(): String      = Random.nextString(10 + genLength(30))
  private def genTypeParam(): String      = Random.nextString(1 + genLength(4))
  private def genList[T](n: Int, gen: () => T): List[T] =
    if n == 0 then List() else gen() :: genList(n - 1, gen)
  private def genVersion(): Option[String] = Some(
    s"${Random.nextInt(1000)}.${Random.nextInt(1000)}.${Random.nextInt(1000)}"
  )
  private def genMember(): Member =
    Member(name = genName(), typeFullName = genFqName())
  private def genMethod(): Method =
    Method(name = genName(), fullName = genFqName(), signature = genSignature())
}
