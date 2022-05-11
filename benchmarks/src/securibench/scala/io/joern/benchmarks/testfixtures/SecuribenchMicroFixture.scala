package io.joern.benchmarks.testfixtures

class SecuribenchMicroFixture(category: String, fileExt: String, benchmarkNo: String)
    extends BenchmarkFixture(
      benchmarkName = "securibench",
      pkg = "securibench/micro",
      category = category,
      fileExt = fileExt,
      benchmarkNo = benchmarkNo
    )

class SecuribenchMicroAliasingFixture(fileExt: String, benchmarkNo: Int)
    extends SecuribenchMicroFixture(category = "Aliasing", fileExt = fileExt, benchmarkNo = benchmarkNo.toString)

class SecuribenchMicroBasicFixture(fileExt: String, benchmarkNo: Int)
  extends SecuribenchMicroFixture(category = "Basic", fileExt = fileExt, benchmarkNo = benchmarkNo.toString)
