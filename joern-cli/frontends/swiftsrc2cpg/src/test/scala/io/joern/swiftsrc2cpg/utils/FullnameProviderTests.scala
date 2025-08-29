package io.joern.swiftsrc2cpg.utils

import io.joern.swiftsrc2cpg.utils.SwiftTypesProvider.{ResolvedTypeInfo, SwiftFileLocalTypeMapping}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable
import scala.jdk.CollectionConverters.*

class FullnameProviderTests extends AnyFlatSpec with Matchers {

  private class MockFullNameProvider(typeMap: SwiftFileLocalTypeMapping) extends FullnameProvider(typeMap) {
    override def typeFullname(range: (Int, Int), nodeKind: String): Option[String] =
      super.typeFullname(range, nodeKind)

    override def declFullname(range: (Int, Int), nodeKind: String): Option[String] =
      super.declFullname(range, nodeKind)
  }

  val typeInfo1          = ResolvedTypeInfo(Some("type.fullname.A"), Some("decl.fullname.A"), "AKind")
  val typeInfo2          = ResolvedTypeInfo(Some("type.fullname.B"), Some("decl.fullname.B"), "BKind")
  val typeInfoNoType     = ResolvedTypeInfo(None, Some("decl.fullname.C"), "CKind")
  val typeInfoNoFullName = ResolvedTypeInfo(Some("type.fullname.D"), None, "DKind")

  "typeFullname" should "return the type when available in the map" in {
    val mockTypeMap = new SwiftFileLocalTypeMapping()
    mockTypeMap.put((10, 20), mutable.HashSet(typeInfo1))

    val provider = new MockFullNameProvider(mockTypeMap)
    provider.typeFullname((10, 20), "nodeKind") shouldBe Some("type.fullname.A")
  }

  it should "return None when type is not available" in {
    val mockTypeMap = new SwiftFileLocalTypeMapping()
    mockTypeMap.put((10, 20), mutable.HashSet(typeInfoNoType))

    val provider = new MockFullNameProvider(mockTypeMap)
    provider.typeFullname((10, 20), "nodeKind") shouldBe None
  }

  it should "try with a single-point range when original range is not found" in {
    val mockTypeMap = new SwiftFileLocalTypeMapping()
    mockTypeMap.put((10, 10), mutable.HashSet(typeInfo2))

    val provider = new MockFullNameProvider(mockTypeMap)
    provider.typeFullname((10, 20), "nodeKind") shouldBe Some("type.fullname.B")
  }

  it should "handle empty sets in the type map" in {
    val mockTypeMap = new SwiftFileLocalTypeMapping()
    mockTypeMap.put((10, 20), mutable.HashSet())
    mockTypeMap.put((10, 10), mutable.HashSet())

    val provider = new MockFullNameProvider(mockTypeMap)
    provider.typeFullname((10, 20), "nodeKind") shouldBe None
  }

  "declFullname" should "return the fullName when available in the map" in {
    val mockTypeMap = new SwiftFileLocalTypeMapping()
    mockTypeMap.put((30, 40), mutable.HashSet(typeInfo1))

    val provider = new MockFullNameProvider(mockTypeMap)
    provider.declFullname((30, 40), "nodeKind") shouldBe Some("decl.fullname.A")
  }

  it should "return None when fullName is not available" in {
    val mockTypeMap = new SwiftFileLocalTypeMapping()
    mockTypeMap.put((30, 40), mutable.HashSet(typeInfoNoFullName))

    val provider = new MockFullNameProvider(mockTypeMap)
    provider.declFullname((30, 40), "nodeKind") shouldBe None
  }

  it should "try with a single-point range when original range is not found" in {
    val mockTypeMap = new SwiftFileLocalTypeMapping()
    mockTypeMap.put((30, 30), mutable.HashSet(typeInfo2))

    val provider = new MockFullNameProvider(mockTypeMap)
    provider.declFullname((30, 40), "nodeKind") shouldBe Some("decl.fullname.B")
    provider.typeFullname((30, 40), "nodeKind") shouldBe Some("type.fullname.B")
  }

  it should "handle multiple type infos for the same range" in {
    val mockTypeMap = new SwiftFileLocalTypeMapping()
    val multiSet    = mutable.HashSet(typeInfo1, typeInfo2)
    mockTypeMap.put((50, 60), multiSet)

    val provider = new MockFullNameProvider(mockTypeMap)
    // Since filterForNodeKind just returns headOption at the moment, this would pick the first one
    provider.declFullname((50, 60), "nodeKind") shouldBe defined
    provider.typeFullname((50, 60), "nodeKind") shouldBe defined
  }

  it should "return None when range is not available at all" in {
    val mockTypeMap = new SwiftFileLocalTypeMapping()

    val provider = new MockFullNameProvider(mockTypeMap)
    provider.declFullname((30, 40), "nodeKind") shouldBe None
    provider.typeFullname((30, 40), "nodeKind") shouldBe None
  }
}
