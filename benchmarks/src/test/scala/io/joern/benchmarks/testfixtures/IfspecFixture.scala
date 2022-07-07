package io.joern.benchmarks.testfixtures

class IfspecFixture(category: String, fileExt: String, benchmarkNo: String)
    extends BenchmarkFixture(pkg = "ifspec", category = category, fileExt = fileExt, benchmarkNo = benchmarkNo)

class IfspecAliasingFixture(fileExt: String, benchmarkNo: Int)
    extends IfspecFixture(category = "Aliasing", fileExt = fileExt, benchmarkNo = benchmarkNo.toString)

class IfspecArraysFixture(fileExt: String, benchmarkNo: Int)
    extends IfspecFixture(category = "Arrays", fileExt = fileExt, benchmarkNo = benchmarkNo.toString)

class IfspecCastingFixture(fileExt: String, benchmarkNo: Int)
    extends IfspecFixture(category = "Casting", fileExt = fileExt, benchmarkNo = benchmarkNo.toString)

class IfspecClassInitializerFixture(fileExt: String, benchmarkNo: Int)
    extends IfspecFixture(category = "ClassInitializer", fileExt = fileExt, benchmarkNo = benchmarkNo.toString)

class IfspecExceptionsFixture(fileExt: String, benchmarkNo: Int)
    extends IfspecFixture(category = "Exceptions", fileExt = fileExt, benchmarkNo = benchmarkNo.toString)

class IfspecHighConditionalFixture(fileExt: String, benchmarkNo: Int)
    extends IfspecFixture(category = "HighConditional", fileExt = fileExt, benchmarkNo = benchmarkNo.toString)

class IfspecLibraryFixture(fileExt: String, benchmarkNo: Int)
    extends IfspecFixture(category = "Library", fileExt = fileExt, benchmarkNo = benchmarkNo.toString)

class IfspecSimpleFixture(fileExt: String, benchmarkNo: Int)
    extends IfspecFixture(category = "Simple", fileExt = fileExt, benchmarkNo = benchmarkNo.toString)
