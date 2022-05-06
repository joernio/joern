package io.joern.dataflowengineoss.testfixtures

class SecuribenchMicroFixture extends BenchmarkFixture {

  override val benchmarkName = "securibench"
  override val pkg           = "securibench/micro"
  override val fileExt       = ".java"

}

class SecuribenchMicroAliasingFixture extends SecuribenchMicroFixture {

  override val category = "Aliasing"

}
