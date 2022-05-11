# Benchmarks

Contains various static analysis benchmarks implemented for Joern.

## Getting Started

The benchmarks are implemented in ScalaTest. Implemented benchmarks are:

* [Securibench Micro](http://too4words.github.io/securibench-micro/) - `securibench:test`

Below are instructions on how to run Joern against each benchmark.

### Securibench Micro

The benchmark can be run using either the Java source code frontend 
or JVM bytecode Jimple frontend:

* Java source: `sbt "benchmarks / testOnly *.securibench.micro.java.*"`
* JVM bytecode: `sbt "benchmarks / testOnly *.securibench.micro.jvm.*"`

#### Java Source Results

| Category           | #    | FP   | TP   | TN   | FN   |
| ------------------ | ---- | ---- | ---- | ---- | ---- |
| Aliasing           | 14   | 1    | 4    | 1    | 8    |
| Arrays             | 15   | 4    | 7    | 2    | 2    |
| Basic              | 67   | 5    | 59   | 1    | 2    |
| Collections        | 22   | 8    | 14   | 0    | 0    |
| *Total*            | *118* | *18* | *84* | *4* | *12* |

Total accuracy: 74,576%

#### JVM Bytecode Results

| Category           | #    | FP   | TP   | TN  | FN  |
| ------------------ | ---- | ---- | ---- | --- | --- |
| Aliasing           | 14   | 1    | 10   | 1   | 2   |
| Arrays             | 15   | 5    | 7    | 1   | 2   |
| Basic              | 67   | 5    | 60   | 1   | 1   |
| Collections        | 22   | 8    | 14   | 0   | 0   |
| *Total*            | *118* | *19* | *91* | *3* | *5* |

Total accuracy: 79,661%
