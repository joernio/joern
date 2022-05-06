package io.joern.dataflowengineoss.securibench.micro

import io.joern.dataflowengineoss.testfixtures.SecuribenchMicroAliasingFixture

class AliasingBenchmark1 extends SecuribenchMicroAliasingFixture {

  override val benchmarkNo: String = "1"

  s"Aliasing$benchmarkNo" should "leak field data" in {
    cpg.graph.nodeCount() should be > 0
  }

}

class AliasingBenchmark2 extends SecuribenchMicroAliasingFixture {

  override val benchmarkNo: String = "2"

  s"Aliasing$benchmarkNo" should "leak field data" in {}

}
