package io.joern.swiftsrc2cpg.utils

import io.joern.swiftsrc2cpg.utils.SwiftTypesProvider.{ResolvedTypeInfo, SwiftFileLocalTypeMapping}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FullnameProviderTests extends AnyFlatSpec with Matchers {

  private class MockFullNameProvider(typeMap: SwiftFileLocalTypeMapping) extends FullnameProvider(typeMap) {
    override def typeFullname(range: (Int, Int), nodeKind: String): Option[String] =
      super.typeFullname(range, nodeKind)

    override def declFullname(range: (Int, Int), nodeKind: String): Option[String] =
      super.declFullname(range, nodeKind)
  }

  val typeInfo1          = ResolvedTypeInfo(Some("type.fullname.A"), Some("decl.fullname.A"), Seq.empty, "AKind")
  val typeInfo2          = ResolvedTypeInfo(Some("type.fullname.B"), Some("decl.fullname.B"), Seq.empty, "BKind")
  val typeInfoNoType     = ResolvedTypeInfo(None, Some("decl.fullname.C"), Seq.empty, "CKind")
  val typeInfoNoFullName = ResolvedTypeInfo(Some("type.fullname.D"), None, Seq.empty, "DKind")

  "typeFullname" should "return the type when available in the map" in {
    val mockTypeMap = Map((10, 20) -> Set(typeInfo1))

    val provider = new MockFullNameProvider(mockTypeMap)
    provider.typeFullname((10, 20), "nodeKind") shouldBe Some("type.fullname.A")
  }

  it should "return None when type is not available" in {
    val mockTypeMap = Map((10, 20) -> Set(typeInfoNoType))

    val provider = new MockFullNameProvider(mockTypeMap)
    provider.typeFullname((10, 20), "nodeKind") shouldBe None
  }

  it should "try with a single-point range when original range is not found" in {
    val mockTypeMap = Map((10, 10) -> Set(typeInfo2))

    val provider = new MockFullNameProvider(mockTypeMap)
    provider.typeFullname((10, 20), "nodeKind") shouldBe Some("type.fullname.B")
  }

  it should "handle empty sets in the type map" in {
    val mockTypeMap = Map((10, 20) -> Set.empty[ResolvedTypeInfo], (10, 10) -> Set.empty[ResolvedTypeInfo])

    val provider = new MockFullNameProvider(mockTypeMap)
    provider.typeFullname((10, 20), "nodeKind") shouldBe None
  }

  "declFullname" should "return the fullName when available in the map" in {
    val mockTypeMap = Map((30, 40) -> Set(typeInfo1))

    val provider = new MockFullNameProvider(mockTypeMap)
    provider.declFullname((30, 40), "nodeKind") shouldBe Some("decl.fullname.A")
  }

  it should "return None when fullName is not available" in {
    val mockTypeMap = Map((30, 40) -> Set(typeInfoNoFullName))

    val provider = new MockFullNameProvider(mockTypeMap)
    provider.declFullname((30, 40), "nodeKind") shouldBe None
  }

  it should "try with a +-1 range when original range is not found" in {
    val mockTypeMap = Map((30, 30) -> Set(typeInfo2))

    val provider = new MockFullNameProvider(mockTypeMap)
    provider.declFullname((31, 29), "nodeKind") shouldBe Some("decl.fullname.B")
    provider.typeFullname((31, 29), "nodeKind") shouldBe Some("type.fullname.B")
  }

  it should "try with a single-point range when original range is not found" in {
    val mockTypeMap = Map((30, 30) -> Set(typeInfo2))

    val provider = new MockFullNameProvider(mockTypeMap)
    provider.declFullname((30, 40), "nodeKind") shouldBe Some("decl.fullname.B")
    provider.typeFullname((30, 40), "nodeKind") shouldBe Some("type.fullname.B")
  }

  it should "handle multiple type infos for the same range" in {
    val mockTypeMap = Map((50, 60) -> Set(typeInfo1, typeInfo2))

    val provider = new MockFullNameProvider(mockTypeMap)
    // Since filterForNodeKind just returns headOption at the moment, this would pick the first one
    provider.declFullname((50, 60), "nodeKind") shouldBe defined
    provider.typeFullname((50, 60), "nodeKind") shouldBe defined
  }

  it should "return None when range is not available at all" in {
    val mockTypeMap = Map.empty[(Int, Int), Set[ResolvedTypeInfo]]

    val provider = new MockFullNameProvider(mockTypeMap)
    provider.declFullname((30, 40), "nodeKind") shouldBe None
    provider.typeFullname((30, 40), "nodeKind") shouldBe None
  }
}
