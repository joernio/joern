package io.joern.benchmarks.testfixtures

class SecuribenchMicroFixture(category: String, fileExt: String, benchmarkNo: String)
    extends BenchmarkFixture(
      pkg = "securibench/micro",
      category = category,
      fileExt = fileExt,
      benchmarkNo = benchmarkNo
    )

class SecuribenchMicroAliasingFixture(fileExt: String, benchmarkNo: Int)
    extends SecuribenchMicroFixture(category = "Aliasing", fileExt = fileExt, benchmarkNo = benchmarkNo.toString)

class SecuribenchMicroArraysFixture(fileExt: String, benchmarkNo: Int)
    extends SecuribenchMicroFixture(category = "Arrays", fileExt = fileExt, benchmarkNo = benchmarkNo.toString)

class SecuribenchMicroBasicFixture(fileExt: String, benchmarkNo: Int)
    extends SecuribenchMicroFixture(category = "Basic", fileExt = fileExt, benchmarkNo = benchmarkNo.toString)

class SecuribenchMicroCollectionsFixture(fileExt: String, benchmarkNo: String)
    extends SecuribenchMicroFixture(category = "Collections", fileExt = fileExt, benchmarkNo = benchmarkNo)

class SecuribenchMicroDatastructuresFixture(fileExt: String, benchmarkNo: Int)
    extends SecuribenchMicroFixture(category = "Datastructures", fileExt = fileExt, benchmarkNo = benchmarkNo.toString)

class SecuribenchMicroFactoriesFixture(fileExt: String, benchmarkNo: Int)
    extends SecuribenchMicroFixture(category = "Factories", fileExt = fileExt, benchmarkNo = benchmarkNo.toString)

class SecuribenchMicroInterFixture(fileExt: String, benchmarkNo: Int)
    extends SecuribenchMicroFixture(category = "Inter", fileExt = fileExt, benchmarkNo = benchmarkNo.toString)

class SecuribenchMicroPredFixture(fileExt: String, benchmarkNo: Int)
    extends SecuribenchMicroFixture(category = "Pred", fileExt = fileExt, benchmarkNo = benchmarkNo.toString)

class SecuribenchMicroReflectionFixture(fileExt: String, benchmarkNo: Int)
    extends SecuribenchMicroFixture(category = "Refl", fileExt = fileExt, benchmarkNo = benchmarkNo.toString)

class SecuribenchMicroSanitizersFixture(fileExt: String, benchmarkNo: Int)
    extends SecuribenchMicroFixture(category = "Sanitizers", fileExt = fileExt, benchmarkNo = benchmarkNo.toString)

class SecuribenchMicroSessionFixture(fileExt: String, benchmarkNo: Int)
    extends SecuribenchMicroFixture(category = "Session", fileExt = fileExt, benchmarkNo = benchmarkNo.toString)

class SecuribenchMicroStrongUpdatesFixture(fileExt: String, benchmarkNo: Int)
    extends SecuribenchMicroFixture(category = "StrongUpdates", fileExt = fileExt, benchmarkNo = benchmarkNo.toString)
