package io.joern.php2cpg.passes

import io.joern.php2cpg.Config
import io.joern.php2cpg.testfixtures.PhpCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class PhpDownloadDependenciesTest extends PhpCode2CpgFixture() {

  "a PHP project with downloading of dependencies enabled" should {

    val cpg = code(
      """
        |{
        |    "require": {
        |        "aws/aws-sdk-php": "3.304.4",
        |        "php": ">=7.4.3"
        |    }
        |}
        |""".stripMargin,
      "composer.json"
    )
      .moreCode("""<?php
          |require 'vendor/autoload.php';
          |
          |$s3 = new S3Client([
          |    'version' => 'latest',
          |    'region'  => 'us-east-1',
          |    'credentials' => [
          |        'key'    => 'YOUR_AWS_ACCESS_KEY_ID',
          |        'secret' => 'YOUR_AWS_SECRET_ACCESS_KEY',
          |    ]
          |]);
          |
          |""".stripMargin)
      .withConfig(Config().withDownloadDependencies(true))

    "download the AWS library and populate the CPG with external high-level nodes" in {
      cpg.method.fullName(".*S3Client.*").map(x => (x.name, x.fullName, x.isExternal)).foreach(println)
//      val List(s3Client) = cpg.typeDecl("S3Client").isExternal(true).l: @unchecked
//      s3Client.isExternal shouldBe true
    }
    
    "download the AWS library and infer the full name of `S3Client` correctly" in {
      cpg.identifier("s3").typeFullName.toSet shouldBe Set("AWS\\S3\\S3Client")
    }

  }

}
