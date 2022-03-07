import java.io.{File => JFile}
import sbt.librarymanagement.UpdateReport

object SbtHelper {

  object JarClassifier extends Enumeration {
    type JarClassifier = Value
    val Classes, Sources, Javadoc = Value
  }

  def findJar(artifactName: String, report: UpdateReport, jarClassifier: JarClassifier.Value): JFile = {
    val jarOption =
      report.configurations
        .filter(_.configuration.name == "compile")
        .flatMap { config =>
          config.modules.filter(_.module.name == artifactName).flatMap { module =>
            val wantedClassifier = jarClassifier match {
              case JarClassifier.Classes => None
              case JarClassifier.Sources => Some("sources")
              case JarClassifier.Javadoc => Some("javadoc")
            }
            module.artifacts.collect {
              case (artifact, file) if artifact.classifier == wantedClassifier => file
            }
          }
        }
        .headOption

    assert(jarOption.isDefined, s"jar for $artifactName not found")
    jarOption.get
  }
}
