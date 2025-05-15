package io.joern.scanners.php
import io.joern.suites.PHPQueryTestSuite
import io.joern.console.Query
import io.joern.console.scan.QueryWrapper

class MagicHashTests extends PHPQueryTestSuite(MagicHash) {

  "The `magic-hash` query" when {
    "detects Magic Hash in loose comparison " in {
      val cpg = code("""<?php
          |$input = $_GET["password"];
          |$hash = hash('md5', $input);
          |if ($hash == "0e1234") {
          |};
          |""".stripMargin)
      val query   = queryBundle.magicHash()
      val results = this.findMatchingCalls(cpg, query)
      results shouldBe List("$_GET[\"password\"]")
    }
  }

  "The `magic-hash` query" when {
    "detects (case insensitive) Magic Hash in loose comparison " in {
      val cpg = code("""<?php
          |$input = $_GET["password"];
          |$hash = hAsh('mD5', $input);
          |if ($hash == "0e1234") {
          |};
          |""".stripMargin)
      val query   = queryBundle.magicHash()
      val results = this.findMatchingCalls(cpg, query)
      results shouldBe List("$_GET[\"password\"]")
    }
  }

  "The `magic-hash` query" when {
    "detects Magic Hash (through dedicated hash funcs) in loose comparison " in {
      val cpg = code("""<?php
          |$input = $_GET["password"];
          |$hash = md5($input);
          |if ($hash == "0e1234") {
          |};
          |""".stripMargin)
      val query   = queryBundle.magicHash()
      val results = this.findMatchingCalls(cpg, query)
      results shouldBe List("$_GET[\"password\"]")
    }
  }
}
