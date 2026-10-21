package io.joern.c2cpg.astcreation

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class BasicTypeNamesTests extends AnyWordSpec with Matchers {

  "BasicTypeNames.canonicalName" should {
    "use one name for every spelling of the same type" in {
      val spellings = List(
        List("int", "signed int", "int signed")                   -> List("int", "int", "int"),
        List("long", "long int", "int long")                      -> List("long", "long", "long"),
        List("signed long", "long signed int", "long int signed") -> List("long", "long", "long"),
        List("short", "short int")                                -> List("short", "short"),
        List("unsigned short", "short unsigned", "unsigned short int", "short unsigned int") ->
          List("unsigned short", "unsigned short", "unsigned short", "unsigned short"),
        List("long long", "long long int")                                         -> List("long long", "long long"),
        List("unsigned long long", "long long unsigned", "unsigned long long int") ->
          List("unsigned long long", "unsigned long long", "unsigned long long"),
        List("unsigned", "unsigned int")             -> List("unsigned int", "unsigned int"),
        List("signed")                               -> List("int"),
        List("char", "signed char", "unsigned char") -> List("char", "signed char", "unsigned char"),
        List("double", "long double")                -> List("double", "long double")
      )
      for ((spelled, names) <- spellings; (spelling, name) <- spelled.zip(names)) {
        withClue(s"`$spelling`: ") { BasicTypeNames.canonicalName(spelling) shouldBe name }
      }
    }
  }

  "BasicTypeNames.normalize" should {
    "rewrite every run of keywords in a type" in {
      BasicTypeNames.normalize("const long int *") shouldBe "const long *"
      BasicTypeNames.normalize("volatile short unsigned int") shouldBe "volatile unsigned short"
      BasicTypeNames.normalize("unsigned long int (long long int, char)") shouldBe
        "unsigned long (long long, char)"
      BasicTypeNames.normalize("unsigned int wchar_t") shouldBe "unsigned wchar_t"
      BasicTypeNames.normalize("_Complex double") shouldBe "double _Complex"
    }

    "leave names alone that only contain a keyword" in {
      BasicTypeNames.normalize("long_value") shouldBe "long_value"
      BasicTypeNames.normalize("std::integer") shouldBe "std::integer"
      BasicTypeNames.normalize("float") shouldBe "float"
    }

    "be idempotent" in {
      for (
        tpe <- List(
          "unsigned long long",
          "signed char",
          "long double",
          "unsigned int",
          "short",
          "unsigned __int128",
          "double _Complex",
          "unsigned wchar_t",
          "bool",
          "_Bool",
          "float",
          "void"
        )
      ) {
        BasicTypeNames.normalize(tpe) shouldBe tpe
      }
    }
  }

  "BasicTypeNames.keepsWhitespaceBetween" should {
    "keep the whitespace of valid C type names" in {
      BasicTypeNames.keepsWhitespaceBetween("unsigned", "int") shouldBe true
      BasicTypeNames.keepsWhitespaceBetween("signed", "char") shouldBe true
      BasicTypeNames.keepsWhitespaceBetween("volatile", "long") shouldBe true
      BasicTypeNames.keepsWhitespaceBetween("long", "long") shouldBe true
      BasicTypeNames.keepsWhitespaceBetween("long", "double") shouldBe true
      BasicTypeNames.keepsWhitespaceBetween("double", "_Complex") shouldBe true
      BasicTypeNames.keepsWhitespaceBetween("unsigned", "__int128") shouldBe true
    }

    "not keep any other whitespace" in {
      BasicTypeNames.keepsWhitespaceBetween("long", "*") shouldBe false
      BasicTypeNames.keepsWhitespaceBetween("char", "*") shouldBe false
      BasicTypeNames.keepsWhitespaceBetween("Foo", "Bar") shouldBe false
    }
  }
}
