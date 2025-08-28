package io.joern.swiftsrc2cpg.utils

import io.joern.swiftsrc2cpg.astcreation.AstCreatorHelper
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class StripGenericsTests extends AnyWordSpec with Matchers {

  "AstCreatorHelper.stripGenerics" should {
    "return input unchanged if it doesn't contain generics" in {
      AstCreatorHelper.stripGenerics("") shouldBe ""
      AstCreatorHelper.stripGenerics("SimpleType") shouldBe "SimpleType"
      AstCreatorHelper.stripGenerics("Swift.String") shouldBe "Swift.String"
    }

    "remove simple generic parameters" in {
      AstCreatorHelper.stripGenerics("Array<String>") shouldBe "Array"
      AstCreatorHelper.stripGenerics("Swift.Dictionary<String, Int>") shouldBe "Swift.Dictionary"
      AstCreatorHelper.stripGenerics("Optional<Int>") shouldBe "Optional"
    }

    "preserve special tags" in {
      AstCreatorHelper.stripGenerics("<anonymous>") shouldBe "<anonymous>"
      AstCreatorHelper.stripGenerics("Foo.Bar<T>.<lambda>1") shouldBe "Foo.Bar.<lambda>1"
      AstCreatorHelper.stripGenerics("Namespace.<type>") shouldBe "Namespace.<type>"
      AstCreatorHelper.stripGenerics("Module.<extension>") shouldBe "Module.<extension>"
      AstCreatorHelper.stripGenerics("Type.<global>") shouldBe "Type.<global>"
      AstCreatorHelper.stripGenerics("<wildcard>") shouldBe "<wildcard>"
    }

    "handle nested generic parameters" in {
      AstCreatorHelper.stripGenerics("Result<Success<T>, Error<E>>") shouldBe "Result"
      AstCreatorHelper.stripGenerics("Dictionary<String, Array<Int>>") shouldBe "Dictionary"
      AstCreatorHelper.stripGenerics("Promise<Result<Data, Error>>") shouldBe "Promise"
    }

    "preserve infix and prefix operators" in {
      AstCreatorHelper.stripGenerics("<op> infix") shouldBe "<op> infix"
      AstCreatorHelper.stripGenerics("<<< prefix") shouldBe "<<< prefix"
      AstCreatorHelper.stripGenerics("<+> infix") shouldBe "<+> infix"
      AstCreatorHelper.stripGenerics(
        "Foo<T>.Promise<Result<Data, Error>>.<+> infix(param:Dictionary<String, Array<Int>>) -> Result<Success<T>, Error<E>>"
      ) shouldBe "Foo.Promise.<+> infix(param:Dictionary) -> Result"
    }

    "handle Swift-specific function type signatures" in {
      AstCreatorHelper.stripGenerics("Function<(Int, String) -> Bool>") shouldBe "Function"
      AstCreatorHelper.stripGenerics("Callback<(Result<User, Error>) -> Void>") shouldBe "Callback"
    }

    "handle complex cases with mixed special tags and generics" in {
      AstCreatorHelper.stripGenerics("Module.Class<T>.<lambda>") shouldBe "Module.Class.<lambda>"
      AstCreatorHelper.stripGenerics("Outer<T>.Inner<U>.<extension>") shouldBe "Outer.Inner.<extension>"
      AstCreatorHelper.stripGenerics("Generic<T>.Protocol<Element>.<type>") shouldBe "Generic.Protocol.<type>"
      AstCreatorHelper.stripGenerics(
        "Collection<Element>.Iterator<T>.<anonymous>"
      ) shouldBe "Collection.Iterator.<anonymous>"
    }
  }
}
