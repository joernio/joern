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
| Inter              | 25 | 8 | 9 | 2 | 6 |
| Pred               | 9 | 4 | 5 | 0 | 0 |
| *Total*            | *166* | *34* | *103* | *8* | *21* |
Total accuracy: 66,867%

#### JVM Bytecode Results

| Category           | #    | FP   | TP   | TN   | FN   |
| ------------------ | ---- | ---- | ---- | ---- | ---- |
| Aliasing           | 14 | 1 | 10 | 1 | 2 |
| Arrays             | 15 | 5 | 7 | 1 | 2 |
| Basic              | 67 | 5 | 60 | 1 | 1 |
| Collections        | 22 | 8 | 14 | 0 | 0 |
| Data Structures    | 8 | 2 | 3 | 1 | 2 |
| Factories          | 6 | 3 | 3 | 0 | 0 |
| Inter              | 25 | 6 | 8 | 4 | 7 |
| Pred               | 9 | 3 | 5 | 1 | 0 |
| *Total*            | *166* | *33* | *110* | *9* | *14* |
Total accuracy: 71,687%
