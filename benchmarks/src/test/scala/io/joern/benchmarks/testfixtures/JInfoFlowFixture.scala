package io.joern.benchmarks.testfixtures
import java.io.File

class JInfoFlowFixture(category: String, fileExt: String, benchmarkNo: String)
    extends BenchmarkFixture(pkg = "jinfoflow", category = category, fileExt = fileExt, benchmarkNo = benchmarkNo)

class JInfoFlowBasicFixture(fileExt: String, benchmarkNo: Int)
    extends JInfoFlowFixture(category = "Basic", fileExt = fileExt, benchmarkNo = benchmarkNo.toString)

class JInfoFlowCtxFixture(fileExt: String, benchmarkNo: Int)
    extends JInfoFlowFixture(category = "Ctx", fileExt = fileExt, benchmarkNo = benchmarkNo.toString)

class JInfoFlowEventsFixture(fileExt: String, benchmarkNo: Int)
    extends JInfoFlowFixture(category = "Events", fileExt = fileExt, benchmarkNo = benchmarkNo.toString) {

  override protected def getListOfFiles(dir: String): List[File] = {
    val targetFiles     = super.getListOfFiles(dir)
    val eventFramework  = new java.io.File(s"$dir${java.io.File.separator}eventframework")
    val events          = new java.io.File(s"$dir${java.io.File.separator}events")
    val supportingFiles = eventFramework.listFiles() ++ events.listFiles()
    targetFiles ++ supportingFiles.filter(_.isFile)
  }

}
