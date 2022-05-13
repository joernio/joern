# Benchmarks

Contains various static analysis benchmarks implemented for Joern.

## Getting Started

The benchmarks are implemented in ScalaTest. Implemented benchmarks are:

* [Securibench Micro](http://too4words.github.io/securibench-micro/)

Below are instructions on how to run Joern against each benchmark.

### Securibench Micro

The benchmark can be run using either the Java source code frontend 
or JVM bytecode Jimple frontend:

* Java source: `sbt "benchmarks / testOnly *.securibench.micro.java.*"`
* JVM bytecode: `sbt "benchmarks / testOnly *.securibench.micro.jvm.*"`

#### Java Source Results

| Category           | #    | FP   | TP   | TN   | FN   |
| ------------------ | ---- | ---- | ---- | ---- | ---- |
| Aliasing           | 14 | 1 | 4 | 1 | 8 |
| Arrays             | 15 | 4 | 7 | 2 | 2 |
| Basic              | 67 | 5 | 59 | 1 | 2 |
| Collections        | 22 | 8 | 14 | 0 | 0 |
| Data Structures    | 8 | 1 | 2 | 2 | 3 |
| Factories          | 6 | 3 | 3 | 0 | 0 |
| Inter              | 17 | 6 | 7 | 1 | 3 |
| *Total*            | *149* | *28* | *96* | *7* | *18* |
Total accuracy: 69,128%

#### JVM Bytecode Results

| Category           | #    | FP   | TP   | TN   | FN   |
| ------------------ | ---- | ---- | ---- | ---- | ---- |
| Aliasing           | 14 | 1 | 10 | 1 | 2 |
| Arrays             | 15 | 5 | 7 | 1 | 2 |
| Basic              | 67 | 5 | 60 | 1 | 1 |
| Collections        | 22 | 8 | 14 | 0 | 0 |
| Data Structures    | 8 | 2 | 3 | 1 | 2 |
| Factories          | 6 | 3 | 3 | 0 | 0 |
| Inter              | 17 | 6 | 8 | 1 | 2 |
| *Total*            | *149* | *30* | *105* | *5* | *9* |
Total accuracy: 73,826%
