package io.joern.javasrc2cpg.jartypereader

import io.joern.javasrc2cpg.jartypereader.model.{
  BoundWildcard,
  ClassSignature,
  ClassTypeSignature,
  NameWithTypeArgs,
  ResolvedTypeDecl,
  SimpleTypeArgument,
  TypeParameter,
  TypeVariableSignature,
  UnboundWildcard
}
import io.joern.javasrc2cpg.jartypereader.JarTypeReader.ObjectTypeSignature
import io.joern.javasrc2cpg.jartypereader.model.Bound.{BoundAbove, BoundBelow}
import io.shiftleft.utils.ProjectRoot
import org.scalatest.Inside.inside
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.Paths

class JarTypeReaderTests extends AnyFreeSpec with Matchers {
  private val packagePrefix = "io.joern.javasrc2cpg.jartypereader.testcode"

  "java.lang.Object fully qualified name should be correct" in {
    ObjectTypeSignature.qualifiedName shouldBe "java.lang.Object"
  }

  "non generic types should be read correctly" in {
    val packageName = "simplenongeneric"
    val types       = getTypes(packageName)

    inside(types) { case List(testDecl) =>
      testDecl.name shouldBe "Test"
      testDecl.packageSpecifier shouldBe Some(s"$packagePrefix.$packageName")
      testDecl.signature shouldBe ClassSignature(Nil, Some(ObjectTypeSignature), Nil)
      testDecl.qualifiedName shouldBe s"$packagePrefix.$packageName.Test"

      inside(testDecl.getDeclaredMethods) { case List(addMethod) =>
        addMethod.name shouldBe "add"
        addMethod.parentTypeDecl shouldBe testDecl
        addMethod.qualifiedName shouldBe s"$packagePrefix.$packageName.Test.add"

        addMethod.signature.returnSignature.name shouldBe "int"
        addMethod.signature.returnSignature.qualifiedName shouldBe "int"
        addMethod.signature.throwsSignatures shouldBe Nil
        addMethod.signature.typeParameters shouldBe Nil
        addMethod.signature.paramTypes.size shouldBe 2
        addMethod.signature.paramTypes.map(_.name).toSet shouldBe Set("int")
        addMethod.signature.paramTypes.map(_.qualifiedName).toSet shouldBe Set("int")
      }
    }
  }

  "generic types with no overloaded methods should be read correctly" in {
    val packageName = "simplegeneric"
    val types       = getTypes(packageName)

    inside(types) { case List(testDecl) =>
      testDecl.name shouldBe "Test"
      testDecl.packageSpecifier shouldBe Some(s"$packagePrefix.$packageName")
      testDecl.isAbstract shouldBe false
      testDecl.isInterface shouldBe false

      val signature = testDecl.signature
      signature.typeParameters shouldEqual List(TypeParameter("T", Some(ObjectTypeSignature), Nil))
      signature.superclassSignature.map(_.qualifiedName) shouldBe Some("java.lang.Object")
      signature.superinterfaceSignatures shouldBe Nil
    }
  }

  "non generics with parent classes should have the correct signature" in {
    val packageName = "nongenericwithsuperclass"
    val types       = getTypes(packageName)

    inside(types.sortBy(_.name)) { case List(fooDecl, testDecl) =>
      fooDecl.qualifiedName shouldBe s"$packagePrefix.$packageName.Foo"
      fooDecl.signature shouldEqual ClassSignature(Nil, Some(ObjectTypeSignature), Nil)

      testDecl.qualifiedName shouldBe s"$packagePrefix.$packageName.Test"
      val signature = testDecl.signature
      signature.superclassSignature.map(_.qualifiedName) shouldBe Some(s"$packagePrefix.$packageName.Foo")
      signature.superclassSignature.map(_.typedName.typeArguments) shouldBe Some(Nil)
      signature.superinterfaceSignatures.map(_.qualifiedName) shouldBe List("java.lang.Comparable")
      signature.superinterfaceSignatures.flatMap(_.typedName.typeArguments) shouldBe Nil
    }
  }

  "non generic interfaces should have correct signatures" in {
    val packageName = "nongenericinterface"
    val types       = getTypes(packageName)

    inside(types.sortBy(_.name)) { case List(fooDecl, testDecl) =>
      fooDecl.qualifiedName shouldBe s"$packagePrefix.$packageName.Foo"
      fooDecl.signature shouldBe ClassSignature(Nil, None, Nil)
      fooDecl.isInterface shouldBe true
      fooDecl.isAbstract shouldBe false

      testDecl.qualifiedName shouldBe s"$packagePrefix.$packageName.Test"
      val signature = testDecl.signature
      signature.superclassSignature.map(_.qualifiedName) shouldBe Some(s"java.lang.Object")
      signature.superinterfaceSignatures.map(_.qualifiedName) shouldBe List(s"$packagePrefix.$packageName.Foo")
      signature.superinterfaceSignatures.flatMap(_.typedName.typeArguments) shouldBe Nil
      testDecl.isInterface shouldBe false
      testDecl.isAbstract shouldBe false
    }
  }

  "generic methods should have correct signatures" in {
    val packageName = "genericmethods"
    val types       = getTypes(packageName)

    types.size shouldBe 1
    val testDecl = types.head

    val sParam = TypeParameter("S", Some(ObjectTypeSignature), Nil)
    testDecl.signature shouldEqual ClassSignature(List(sParam), Some(ObjectTypeSignature), Nil)

    testDecl.getDeclaredMethods.size shouldBe 1
    val fooMethod = testDecl.getDeclaredMethods.head

    fooMethod.name shouldBe "foo"
    fooMethod.qualifiedName shouldBe s"$packagePrefix.$packageName.Test.foo"
    fooMethod.isAbstract shouldBe false
    val tParam = TypeParameter("T", Some(ObjectTypeSignature), Nil)
    fooMethod.signature.typeParameters shouldEqual List(tParam)
    fooMethod.signature.paramTypes shouldEqual List(TypeVariableSignature("S"), TypeVariableSignature("T"))
    fooMethod.signature.returnSignature shouldEqual ClassTypeSignature(
      Some("java.lang"),
      NameWithTypeArgs("String", Nil),
      Nil
    )
  }

  "generic classes should have correct signatures and type arguments" in {
    val packageName = "genericclasses"
    val types       = getTypes(packageName)

    inside(types.sortBy(_.name)) { case List(fooDecl, testDecl) =>
      fooDecl.qualifiedName shouldBe s"$packagePrefix.$packageName.Foo"
      fooDecl.signature.superinterfaceSignatures shouldBe Nil
      fooDecl.signature.superclassSignature shouldBe Some(ObjectTypeSignature)
      val sParam = TypeParameter("S", Some(ObjectTypeSignature), Nil)
      val tParam = TypeParameter("T", Some(ObjectTypeSignature), Nil)
      fooDecl.signature.typeParameters shouldBe List(tParam, sParam)

      testDecl.qualifiedName shouldBe s"$packagePrefix.$packageName.Test"
      testDecl.signature.superinterfaceSignatures shouldBe Nil
      testDecl.signature.typeParameters shouldBe List(tParam)

      inside(testDecl.signature.superclassSignature) { case Some(fooSup) =>
        fooSup.qualifiedName shouldBe s"$packagePrefix.$packageName.Foo"
        fooSup.typedName.name shouldBe "Foo"
        fooSup.typedName.typeArguments shouldBe List(
          SimpleTypeArgument(TypeVariableSignature("T")),
          SimpleTypeArgument(ClassTypeSignature(Some("java.lang"), NameWithTypeArgs("String", Nil), Nil))
        )
      }
    }
  }

  "overloaded methods should be treated separately" in {
    val packageName = "overloadedmethods"
    val types       = getTypes(packageName)
    val methods     = types.flatMap(_.getDeclaredMethods)

    inside(methods.sortBy(_.signature.typeParameters.head.name)) { case List(sFoo, tFoo) =>
      sFoo.qualifiedName shouldBe s"$packagePrefix.$packageName.Test.foo"
      tFoo.qualifiedName shouldBe s"$packagePrefix.$packageName.Test.foo"

      sFoo.signature.paramTypes shouldBe List(TypeVariableSignature("S"))
      inside(sFoo.signature.typeParameters) { case List(sParam) =>
        sParam.name shouldBe "S"
        sParam.classBound shouldBe Some(ObjectTypeSignature)
        sParam.interfaceBounds shouldBe Nil
      }

      tFoo.signature.paramTypes shouldBe List(TypeVariableSignature("T"))
      inside(tFoo.signature.typeParameters) { case List(tParam) =>
        tParam.name shouldBe "T"
        tParam.classBound shouldBe None
        tParam.interfaceBounds shouldBe List(
          ClassTypeSignature(Some("java.lang"), NameWithTypeArgs("Comparable", Nil), Nil)
        )
      }
    }
  }

  "fields with wildcard types should have the correct signatures" in {
    val packageName = "wildcards"
    val types       = getTypes(packageName)
    val fields      = types.flatMap(_.fields).sortBy(_.name)

    fields.foreach { field =>
      val qualifiedName = field.signature.qualifiedName
      if (qualifiedName != "java.util.List") {
        fail(s"Expected type `java.util.List` for ${field.name} but got $qualifiedName")
      }
    }

    val signatureMap = fields
      .map(field => field.name -> field.signature)
      .collect { case (name, signature: ClassTypeSignature) =>
        name -> signature
      }
      .toMap

    inside(signatureMap.get("boundAbove")) { case Some(signature) =>
      inside(signature.typedName.typeArguments) { case List(boundWildcard: BoundWildcard) =>
        boundWildcard.bound shouldBe BoundAbove
        boundWildcard.typeSignature.qualifiedName shouldBe "java.lang.String"
      }
    }

    inside(signatureMap.get("boundBelow")) { case Some(signature) =>
      inside(signature.typedName.typeArguments) { case List(boundWildcard: BoundWildcard) =>
        boundWildcard.bound shouldBe BoundBelow
        boundWildcard.typeSignature.qualifiedName shouldBe "java.lang.String"
      }
    }

    inside(signatureMap.get("unboundWildcard")) { case Some(signature) =>
      signature.typedName.typeArguments shouldBe List(UnboundWildcard)
    }

    inside(signatureMap.get("noType")) { case Some(signature) =>
      signature.typedName.typeArguments shouldBe Nil
    }
  }

  private def getTypes(name: String): List[ResolvedTypeDecl] = {
    val path      = Paths.get("joern-cli", "frontends", "javasrc2cpg", "target", "testjars", s"$name.jar")
    val inputPath = ProjectRoot.relativise(path.toString)
    JarTypeReader.getTypes(inputPath)
  }
}
