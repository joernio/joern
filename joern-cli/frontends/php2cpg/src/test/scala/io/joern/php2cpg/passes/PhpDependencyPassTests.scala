package io.joern.php2cpg.passes

import io.joern.php2cpg.parser.Domain.PhpOperators
import io.joern.php2cpg.testfixtures.PhpCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class PhpDependencyPassTests extends PhpCode2CpgFixture() {

  "a composer file" should {

    val cpg = code(
      """
        |{
        |    "require": {
        |        "composer/semver": "3.4.0",
        |        "aws/aws-sdk-php": "3.*",
        |        "php": ">=7.4.3"
        |    },
        |    "autoload": {
        |        "psr-4": {
        |            "MediaWiki\\Composer\\": "includes/composer",
        |            "Monolog\\": ["src/", "lib/"]
        |        }
        |    }
        |}
        |""".stripMargin,
      "composer.json"
    )

    "have its dependencies under `require` parsed and given nodes" in {
      inside(cpg.dependency.filterNot(_.version.startsWith(PhpOperators.autoload)).l) {
        case semver :: aws :: php :: Nil =>
          semver.name shouldBe "composer/semver"
          semver.version shouldBe "3.4.0"
          aws.name shouldBe "aws/aws-sdk-php"
          aws.version shouldBe "3.*"
          php.name shouldBe "php"
          php.version shouldBe ">=7.4.3"
        case xs => fail(s"Expected exactly 3 dependencies, instead got [${xs.name.mkString(",")}]")
      }
    }

    "have `autoload` fields parsed and given versions prefixed with `<autoload>`" in {
      inside(cpg.dependency.filter(_.version.startsWith(PhpOperators.autoload)).l) {
        case composer :: monologSrc :: monologLib :: Nil =>
          composer.name shouldBe "MediaWiki\\Composer\\"
          composer.version shouldBe s"${PhpOperators.autoload}includes/composer"
          monologSrc.name shouldBe "Monolog\\"
          monologSrc.version shouldBe s"${PhpOperators.autoload}src/"
          monologLib.name shouldBe "Monolog\\"
          monologLib.version shouldBe s"${PhpOperators.autoload}lib/"
        case xs => fail(s"Expected exactly 3 dependencies, instead got [${xs.name.mkString(",")}]")
      }
    }

  }

}
