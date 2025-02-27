package io.joern.scanners.php
import io.joern.suites.PHPQueryTestSuite
import io.joern.console.Query
import io.joern.console.scan.QueryWrapper

/** Test suite for PHP security vulnerabilities using PHPJoern queries. Validates detections of SQL Injection, Command
  * Injection, Code Injection, Unrestricted File Upload, and Cross-Site Scripting vulnerabilities.
  */
class PHPJoernTests extends PHPQueryTestSuite(PhpJoern) {

  "The `sqli` query" when {
    "detects SQL Injection using user input" in {
      val cpg = code("""<?php
          |$input = $_GET["username"];
          |$query = "SELECT * FROM users WHERE name = '$input'";
          |$result = mysql_query($query);
          |""".stripMargin)
      val query   = queryBundle.sqli()
      val results = this.findMatchingCalls(cpg, query)
      results shouldBe List("$_GET[\"username\"]")
    }

    "ignores safe code execution without user input" in {
      val cpg = code("""<?php
          |$input = "username";
          |$query = "SELECT * FROM users WHERE name = '$input'";
          |$result = mysql_query($query);
          |""".stripMargin)
      val query   = queryBundle.sqli()
      val results = this.findMatchingCalls(cpg, query)
      results shouldBe List()
    }
  }

  "The `cmdi` query" when {
    "detects Command Injection via system calls" in {
      val cpg = code("""<?php
          |$cmd = $_GET["cmd"];
          |system($cmd);
          |""".stripMargin)
      val query   = queryBundle.cmdi()
      val results = this.findMatchingCalls(cpg, query)
      results shouldBe List("$_GET[\"cmd\"]")
    }

    "ignores safe code execution without user input" in {
      val cpg = code("""<?php
          |$cmd = "ls \tmp";
          |system($cmd);
          |""".stripMargin)
      val query   = queryBundle.cmdi()
      val results = this.findMatchingCalls(cpg, query)
      results shouldBe List()
    }
  }

  "The `codei` query" when {
    "detects Code Injection using eval" in {
      val cpg = code("""<?php
          |$code = $_GET["code"];
          |eval($code);
          |""".stripMargin)
      val query   = queryBundle.codei()
      val results = this.findMatchingCalls(cpg, query)
      results shouldBe List("$_GET[\"code\"]")
    }

    "ignores safe code execution without user input" in {
      val cpg = code("""<?php
          |$safeCode = 'echo "safe";';
          |eval($safeCode);
          |""".stripMargin)
      val query   = queryBundle.codei()
      val results = this.findMatchingCalls(cpg, query)
      results shouldBe List()
    }
  }

  "The `uuf` query" when {
    "detects Unrestricted File Upload vulnerabilities" in {
      val cpg = code("""<?php
          |$filename = $_GET["file_name"];
          |move_uploaded_file("tmp_file", "/uploads/" . $filename);
          |""".stripMargin)
      val query   = queryBundle.uuf()
      val results = this.findMatchingCalls(cpg, query)
      results shouldBe List("$_GET[\"file_name\"]")
    }

    "ignores safe code execution without user input" in {
      val cpg = code("""<?php
          |$filename = "user_file";
          |move_uploaded_file("tmp_file", "/uploads/" . $filename);
          |""".stripMargin)
      val query   = queryBundle.uuf()
      val results = this.findMatchingCalls(cpg, query)
      results shouldBe List()
    }
  }

  "The `xss` query" when {
    "detects Cross-site Scripting via echo" in {
      val cpg = code("""<?php
          |$user_input = $_GET["input"];
          |echo $user_input;
          |""".stripMargin)
      val query   = queryBundle.xss()
      val results = this.findMatchingCalls(cpg, query)
      results shouldBe List("$_GET[\"input\"]")
    }

    "ignores safe code execution without user input" in {
      val cpg = code("""<?php
          |$user_input = "input";
          |echo $user_input;
          |""".stripMargin)
      val query   = queryBundle.xss()
      val results = this.findMatchingCalls(cpg, query)
      results shouldBe List()
    }
  }
}
