package io.joern.x2cpg.utils.dependency

import io.shiftleft.semanticcpg.utils.ExternalCommand
import org.slf4j.LoggerFactory

import java.nio.file.Path
import scala.util.{Failure, Success}

object MavenCoordinates {
  private[dependency] def fromGradleOutput(lines: Seq[String]): Seq[String] = {
    /*
    on the following regex, for the following input:
    ```
    |    |    +--- org.springframework.boot:spring-boot-starter-logging:3.0.5
    |    |    |    +--- ch.qos.logback:logback-classic:1.4.6
    |    |    |    |    +--- ch.qos.logback:logback-core:1.4.6
    |    |    |    |    \--- org.slf4j:slf4j-api:2.0.4 -> 2.0.7
    |    |    |    +--- org.apache.logging.log4j:log4j-to-slf4j:2.19.0
    |    |    |    |    +--- org.slf4j:slf4j-api:1.7.36 -> 2.0.7
    |    |    |    |    \--- org.apache.logging.log4j:log4j-api:2.19.0 -> 2.15.0
    |    |    |    \--- org.slf4j:jul-to-slf4j:2.0.7
    ```
    the resulting matches and their groups are:
    ```
    org.springframework.boot:spring-boot-starter-logging:3.0.5
    ^g1 ------------------------------------------------^^g2 ^
    ch.qos.logback:logback-classic:1.4.6
    ^g1 --------------------------^^g2 ^
    ch.qos.logback:logback-core:1.4.6
    ^g1 -----------------------^^g2 ^
    org.slf4j:slf4j-api:2.0.4 -> 2.0.7
    ^g1 ---------------^^g2 ^^g3^^g4 ^
    org.apache.logging.log4j:log4j-to-slf4j:2.19.0
    ^g1 -----------------------------------^^g2 -^
    org.slf4j:slf4j-api:1.7.36 -> 2.0.7
    ^g1 ---------------^^g2 -^^g3^^g4 ^
    org.apache.logging.log4j:log4j-api:2.19.0 -> 2.15.0
    ^g1 ------------------------------^^g2 -^^g3^^g4 -^
    org.slf4j:jul-to-slf4j:2.0.7
    ^g1 ------------------^^g2 ^
    ```
     */
    val pattern = """^[| ]*[+\\]\s*[-]*\s*([^:]+:[^:]+:)([^\s]+)(\s+->\s+)?([^\s]+)?""".r
    lines
      .flatMap { l =>
        pattern.findFirstMatchIn(l) match {
          case Some(m) =>
            if (Option(m.group(4)).isEmpty)
              Some(m.group(1) + m.group(2))
            else
              Some(m.group(1) + m.group(4))
          case _ => None
        }
      }
      .distinct
      .sorted
  }
}
