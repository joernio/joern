# Benchmarks

Contains various static analysis benchmarks implemented for Joern.

## Getting Started

The benchmarks are implemented in ScalaTest. Implemented benchmarks are:

* [Securibench Micro](http://too4words.github.io/securibench-micro/)

Below are instructions on how to run Joern against each benchmark. If
you would like the benchmark results to be written to a file instead
of printed to STDOUT, set the path to the environment variable 
`JOERN_BENCHMARK_RESULT_FILE`.

### Securibench Micro

The benchmark can be run using either the Java source code frontend
or JVM bytecode Jimple frontend:

* Java source: `sbt "benchmarks / testOnly *.securibench.micro.java.*"`
* JVM bytecode: `sbt "benchmarks / testOnly *.securibench.micro.jvm.*"`

#### Java Source Results
| Category          | #    | FP   | TP   | TN   | FN   |
| ----------------- | ---- | ---- | ---- | ---- | ---- |
| Aliasing          | 14 | 1 | 4 | 1 | 8 |
| Arrays            | 15 | 4 | 7 | 2 | 2 |
| Basic             | 67 | 5 | 59 | 1 | 2 |
| Collections       | 22 | 8 | 14 | 0 | 0 |
| Data Structures   | 8 | 1 | 2 | 2 | 3 |
| Factories         | 6 | 3 | 3 | 0 | 0 |
| Inter-procedural  | 25 | 7 | 8 | 3 | 7 |
| Predicates        | 9 | 4 | 5 | 0 | 0 |
| Reflection        | 4 | 1 | 1 | 0 | 2 |
| Sanitizers        | 9 | 3 | 2 | 2 | 2 |
| Session           | 4 | 1 | 3 | 0 | 0 |
| Strong Updates    | 5 | 0 | 0 | 4 | 1 |
| *Total*           | *188* | *38* | *108* | *15* | *27* |

Total accuracy: 65,426%
#### JVM Bytecode Results
| Category          | #    | FP   | TP   | TN   | FN   |
| ----------------- | ---- | ---- | ---- | ---- | ---- |
| Aliasing          | 14 | 1 | 10 | 1 | 2 |
| Arrays            | 15 | 5 | 7 | 1 | 2 |
| Basic             | 67 | 5 | 60 | 1 | 1 |
| Collections       | 22 | 8 | 14 | 0 | 0 |
| Data Structures   | 8 | 1 | 2 | 2 | 3 |
| Factories         | 6 | 3 | 3 | 0 | 0 |
| Inter-procedural  | 25 | 5 | 7 | 5 | 8 |
| Predicates        | 9 | 3 | 5 | 1 | 0 |
| Reflection        | 4 | 1 | 2 | 0 | 1 |
| Sanitizers        | 9 | 3 | 2 | 2 | 2 |
| Session           | 4 | 1 | 3 | 0 | 0 |
| Strong Updates    | 5 | 0 | 0 | 4 | 1 |
| *Total*           | *188* | *36* | *115* | *17* | *20* |

Total accuracy: 70,213%
