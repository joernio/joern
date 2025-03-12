package io.joern.x2cpg.utils.dependency

import io.shiftleft.semanticcpg.utils.ExternalCommand
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MavenCoordinatesTests extends AnyWordSpec with Matchers {

  "test maven coordinates from gradle output" in {
    val gradleOutputLines =
      Seq(
        "|    +--- org.springframework.boot:spring-boot-starter-logging:3.0.5",
        "|    |    |    +--- ch.qos.logback:logback-classic:1.4.6",
        "|    |    |    |    +--- ch.qos.logback:logback-core:1.4.6",
        "|    |    |    |    \\--- org.slf4j:slf4j-api:2.0.4 -> 2.0.7",
        "|    |    |    +--- org.apache.logging.log4j:log4j-to-slf4j:2.19.0",
        "|    |    |    |    +--- org.slf4j:slf4j-api:1.7.36 -> 2.0.7",
        "|    |    |    |    \\--- org.apache.logging.log4j:log4j-api:2.19.0 -> 2.15.0",
        "|    |    |    \\--- org.slf4j:jul-to-slf4j:2.0.7"
      )

    val result = MavenCoordinates.fromGradleOutput(gradleOutputLines)
    result shouldBe Seq(
      "ch.qos.logback:logback-classic:1.4.6",
      "ch.qos.logback:logback-core:1.4.6",
      "org.apache.logging.log4j:log4j-api:2.15.0",
      "org.apache.logging.log4j:log4j-to-slf4j:2.19.0",
      "org.slf4j:jul-to-slf4j:2.0.7",
      "org.slf4j:slf4j-api:2.0.7",
      "org.springframework.boot:spring-boot-starter-logging:3.0.5"
    )
  }
}
