# Joern Query Database ("Joern-Scan")

This is the central query database for the open-source code analysis
platform [Joern](https://github.com/joernio/joern). It has
two purposes:

* It provides the batteries required to turn Joern into a ready-to-run code scanning tool.
* Its queries serve as examples useful for those looking to write their own queries.
* [built on JDK11](https://github.com/ShiftLeftSecurity/overflowdb/blob/master/.github/workflows/release.yml) but runs on JRE >= 1.8

The query database is distributed as a standalone library that
includes Joern as a dependency. This means that it is not necessary to
install Joern to make use of the queries in the database.

At the same time, the database is a Joern extension, that is, when
dynamically loaded at startup, its functionality becomes available on
the interactive Joern shell and in Joern scripts.

You can fork this project to build your own custom queries and
scanners or kindly send a PR to to this repo to have them considered
for inclusion in the default distribution.

## Installing and running

The installation script downloads joern and installs it in a sub-directory.
The query database is installed as an extension.

```
./install.sh
```

You can run all queries as follows:

```
./joern-scan path/to/code
```

For example,

```
mkdir foo
echo "int foo(int a, int b, int c, int d, int e, int f) {}" > foo/foo.c
./joern-scan foo
```

runs all queries on the sample code in the directory `foo`, determining that the function `foo`
has too many parameters.

## Adding your own queries

Please follow the rules below for a tear-free query writing experience:

* Queries in the package `io.joern.scanners` are picked up automatically at runtime,
  so please put your queries there.
* Each query must begin with the annotation `@q` and must be placed in a query bundle.
  A query bundle is simply an `object` that derives from `QueryBundle`
* Queries can have parameters,but you must provide a default value for each parameter
* Please add unit tests for queries. These also serve as a spec for what your query does.
* Please format the code before sending a PR using `sbt scalafmt Test/scalafmt`

Take a look at the query bundle `Metrics` at `src/main/scala/io/joern/scanners/c/Metrics.scala`
as an example:

```
object Metrics extends QueryBundle {

  @q
  def tooManyParameters(n: Int = 4): Query =
    Query.make(
      name = "too-many-params",
      author = Crew.fabs,
      title = s"Number of parameters larger than $n",
      description = s"This query identifies functions with more than $n formal parameters",
      score = 1.0,
      withStrRep({ cpg =>
        cpg.method.internal.filter(_.parameter.size > n)
      }),
      tags = List(QueryTags.metrics)
    )

  @q
  def tooHighComplexity(n: Int = 4): Query =
    Query.make(
      name = "too-high-complexity",
      author = Crew.fabs,
      title = s"Cyclomatic complexity higher than $n",
      description = s"This query identifies functions with a cyclomatic complexity higher than $n",
      score = 1.0,
      withStrRep({ cpg =>
        cpg.method.internal.filter(_.controlStructure.size > n)
      }),
      tags = List(QueryTags.metrics)
    )
  ...
}
```

Corresponding tests for queries are located in
`src/test/scala/io/joern/scanners`. For example, tests for the metrics
queries are located in
`src/test/scala/io/joern/scanners/c/MetricsTests.scala`:

```
class MetricsTests extends Suite {

  override val code = """
    int too_many_params(int a, int b, int c, int d, int e) {
    }
	...
	"""

  "find functions with too many parameters" in {
    Metrics.tooManyParameters(4)(cpg).map(_.evidence) match {
      case List(List(method: nodes.Method)) =>
        method.name shouldBe "too_many_params"
      case _ => fail
    }
  }
  ...
}
```

These tests can be run individually from the IntelliJ IDE during query
development.

## Building/Testing the database

We use the Scala Build Tool (sbt). Please make sure you have sbt
installed. The version does not matter as sbt will fetch the required
version based on the build file (`build.sbt`).

Once `sbt` is installed, you can build and test the database as
follows:

```
sbt test
```

You can test newly developed queries

If you want to test newly created queries with `joern-scan` as follows:

```
sbt joerncli/stage
./querydb-install.sh && ./joern-scan <src>
```

## Exporting the database to JSON

After running `install.sh`, you can launch
```
./joern-scan --dump
```
to create a file named `querydb.json` that contains the list of all available queries
along with its meta information.
