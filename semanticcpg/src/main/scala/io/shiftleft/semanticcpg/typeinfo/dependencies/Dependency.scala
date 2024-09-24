package io.shiftleft.semanticcpg.typeinfo.dependencies

import io.shiftleft.semanticcpg.typeinfo.{JavaPackageIdentifier, PackageIdentifier, Version}

import scala.jdk.CollectionConverters._
import org.apache.commons.csv.{CSVFormat, CSVParser, CSVRecord}
import java.io.{File, FileReader}
import scala.util.{Failure, Success, Try}

final case class Dependency(name: PackageIdentifier, version: Constraint)
final case class ResolvedDependency(name: PackageIdentifier, version: Version)

object Dependency {
  def fromFile(f: File): Try[List[Dependency]] = {
    val reader = FileReader(f)
    val parser: CSVParser = CSVFormat.RFC4180.parse(reader)
    Try(parser
      .getRecords()
      .asScala
      .map(csvRecordToDependency)
      .toList)
  }

  private def csvRecordToDependency(rec: CSVRecord): Dependency = {
    val packageName = rec.get(0)
    val constraint = rec.get(1)
    Dependency(JavaPackageIdentifier(packageName), Constraint.parse(constraint))
  }
}