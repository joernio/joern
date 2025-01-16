package io.joern.javasrc2cpg.util

import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.joern.javasrc2cpg.scope.Scope
import io.shiftleft.codepropertygraph.generated.nodes.NewTypeDecl
import io.shiftleft.codepropertygraph.generated.nodes.NewMember
import io.shiftleft.codepropertygraph.generated.nodes.NewMethod
import io.joern.javasrc2cpg.astcreation.ExpectedType
import io.joern.javasrc2cpg.scope.Scope.SimpleVariable
import io.joern.javasrc2cpg.scope.Scope.ScopeMember
import io.joern.javasrc2cpg.scope.Scope.CapturedVariable
import io.shiftleft.codepropertygraph.generated.nodes.NewMethodParameterIn
import io.joern.javasrc2cpg.scope.Scope.ScopeParameter
import io.joern.javasrc2cpg.scope.Scope.NotInScope
import io.joern.x2cpg.ValidationMode

class ScopeTests extends AnyFlatSpec with Matchers with BeforeAndAfterAll {
  private implicit val withSchemaValidation: ValidationMode = ValidationMode.Enabled
  private implicit val disableTypeFallback: Boolean         = false
  private val genericSignature                              = "GENERIC_SIGNATURE"

  behavior of "javasrc2cpg scope"

  it should "find a simple variable for a member" in {
    val typeDecl = NewTypeDecl().name("FooDecl")
    val member   = NewMember().name("fooMember")
    val method   = NewMethod().name("fooMethod")

    val scope = new Scope()
    scope.pushTypeDeclScope(typeDecl, isStatic = false, Option(genericSignature))
    scope.enclosingTypeDecl.get.addMember(member, isStatic = false)
    scope.pushMethodScope(method, ExpectedType.empty, isStatic = false)

    scope.lookupVariable("fooMember") shouldBe SimpleVariable(ScopeMember(member, false))
  }

  it should "find a capture chain for a captured member in an outer class" in {
    val outerTypeDecl = NewTypeDecl().name("FooDecl")
    val outerMember   = NewMember().name("fooMember")
    val method        = NewMethod().name("fooMethod")
    val innerTypeDecl = NewTypeDecl().name("InnerDecl")

    val scope = new Scope()
    scope.pushTypeDeclScope(outerTypeDecl, isStatic = false)
    scope.enclosingTypeDecl.get.addMember(outerMember, isStatic = false)
    scope.pushMethodScope(method, ExpectedType.empty, isStatic = false)
    scope.pushTypeDeclScope(innerTypeDecl, isStatic = false)

    scope.lookupVariable("fooMember") shouldBe CapturedVariable(List(outerTypeDecl), ScopeMember(outerMember, false))
  }

  it should "find a capture chain for a captured variable" in {
    val outerTypeDecl  = NewTypeDecl().name("FooDecl")
    val method         = NewMethod().name("fooMethod")
    val outerParameter = NewMethodParameterIn().name("fooParameter")
    val innerTypeDecl  = NewTypeDecl().name("InnerDecl")

    val scope = new Scope()
    scope.pushTypeDeclScope(outerTypeDecl, isStatic = false)
    scope.pushMethodScope(method, ExpectedType.empty, isStatic = false)
    scope.enclosingMethod.get.addParameter(outerParameter, genericSignature)
    scope.pushTypeDeclScope(innerTypeDecl, isStatic = false)

    scope.lookupVariable("fooParameter") shouldBe CapturedVariable(
      Nil,
      ScopeParameter(outerParameter, genericSignature)
    )
  }

  it should "find a capture chain for a captured variable in an outer-outer scope" in {
    val outerOuterTypeDecl  = NewTypeDecl().name("OuterOuterTypeDecl")
    val outerOuterMethod    = NewMethod().name("OuterOuterMethod")
    val outerOuterParameter = NewMethodParameterIn().name("parameter")
    val outerTypeDecl       = NewTypeDecl().name("OuterTypeDecl")
    val outerMethod         = NewMethod().name("outerMethod")
    val innerTypeDecl       = NewTypeDecl().name("InnerDecl")

    val scope = new Scope()
    scope.pushTypeDeclScope(outerOuterTypeDecl, isStatic = false)
    scope.pushMethodScope(outerOuterMethod, ExpectedType.empty, isStatic = false)
    scope.enclosingMethod.get.addParameter(outerOuterParameter, genericSignature)
    scope.pushTypeDeclScope(outerTypeDecl, isStatic = false)
    scope.pushMethodScope(outerMethod, ExpectedType.empty, isStatic = false)
    scope.pushTypeDeclScope(innerTypeDecl, isStatic = false)

    scope.lookupVariable("parameter") shouldBe CapturedVariable(
      List(outerTypeDecl),
      ScopeParameter(outerOuterParameter, genericSignature)
    )
  }

  it should "not find a capture chain for a captured variable in an outer-outer scope interrupted by a static scope" in {
    val outerOuterTypeDecl  = NewTypeDecl().name("OuterOuterTypeDecl")
    val outerOuterMethod    = NewMethod().name("OuterOuterMethod")
    val outerOuterParameter = NewMethodParameterIn().name("parameter")
    val outerTypeDecl       = NewTypeDecl().name("OuterTypeDecl")
    val outerMethod         = NewMethod().name("outerMethod")
    val innerTypeDecl       = NewTypeDecl().name("InnerDecl")

    val scope = new Scope()
    scope.pushTypeDeclScope(outerOuterTypeDecl, isStatic = false)
    scope.pushMethodScope(outerOuterMethod, ExpectedType.empty, isStatic = false)
    scope.enclosingMethod.get.addParameter(outerOuterParameter, genericSignature)
    scope.pushTypeDeclScope(outerTypeDecl, isStatic = false)
    scope.pushMethodScope(outerMethod, ExpectedType.empty, isStatic = false)
    scope.pushTypeDeclScope(innerTypeDecl, isStatic = true)

    scope.lookupVariable("parameter") shouldBe NotInScope
  }

  it should "not find a capture chain for a member outside a static method scope" in {
    val outerTypeDecl = NewTypeDecl().name("FooDecl")
    val outerMember   = NewMember().name("fooMember")
    val method        = NewMethod().name("fooMethod")
    val innerTypeDecl = NewTypeDecl().name("InnerDecl")

    val scope = new Scope()
    scope.pushTypeDeclScope(outerTypeDecl, isStatic = false)
    scope.enclosingTypeDecl.get.addMember(outerMember, isStatic = false)
    scope.pushMethodScope(method, ExpectedType.empty, isStatic = true)
    scope.pushTypeDeclScope(innerTypeDecl, isStatic = false)

    scope.lookupVariable("fooMember") shouldBe NotInScope
  }

  it should "not find a capture chain for a member outside a static field scope" in {
    val outerTypeDecl = NewTypeDecl().name("FooDecl")
    val outerMember   = NewMember().name("fooMember")
    val innerTypeDecl = NewTypeDecl().name("InnerDecl")

    val scope = new Scope()
    scope.pushTypeDeclScope(outerTypeDecl, isStatic = false)
    scope.enclosingTypeDecl.get.addMember(outerMember, isStatic = false)
    scope.pushFieldDeclScope(isStatic = true, "fooMember")
    scope.pushTypeDeclScope(innerTypeDecl, isStatic = false)

    scope.lookupVariable("fooMember") shouldBe NotInScope
  }

  it should "find a capture chain for a member outside an instance field scope" in {
    val outerTypeDecl = NewTypeDecl().name("FooDecl")
    val outerMember   = NewMember().name("fooMember")
    val innerTypeDecl = NewTypeDecl().name("InnerDecl")

    val scope = new Scope()
    scope.pushTypeDeclScope(outerTypeDecl, isStatic = false)
    scope.enclosingTypeDecl.get.addMember(outerMember, isStatic = false)
    scope.pushFieldDeclScope(isStatic = false, "fooMember")
    scope.pushTypeDeclScope(innerTypeDecl, isStatic = false)

    scope.lookupVariable("fooMember") shouldBe CapturedVariable(
      List(outerTypeDecl),
      ScopeMember(outerMember, isStatic = false)
    )
  }

  it should "return correct captured variables for single nesting level" in {
    val outerTypeDecl = NewTypeDecl().name("FooDecl")
    val outerMember   = NewMember().name("fooMember")
    val method        = NewMethod().name("fooMethod")

    val scope = new Scope()
    scope.pushTypeDeclScope(outerTypeDecl, isStatic = false)
    scope.enclosingTypeDecl.get.addMember(outerMember, isStatic = false)
    scope.pushMethodScope(method, ExpectedType.empty, isStatic = false)

    scope.getCapturesForNewScope(false) shouldEqual Map(
      "fooMember" -> CapturedVariable(List(outerTypeDecl), ScopeMember(outerMember, isStatic = false))
    )
  }
}
