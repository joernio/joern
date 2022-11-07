
### Securibench Micro

Securibench Micro is a series of small test cases designed to exercise different parts of a static security analyzer.
Securibench Micro may be used to compare the effectiveness of runtime techniques such as penetration testing tools.

* Java source: `sbt "benchmarks / testOnly *.securibench.micro.java.*"`
* JVM bytecode: `sbt "benchmarks / testOnly *.securibench.micro.jvm.*"`

#### Java Source Results
| Category           | #    | FP   | TP   | TN   | FN   |
| ------------------ | ---- | ---- | ---- | ---- | ---- |
| Aliasing           | 14 | 1 | 4 | 1 | 8 |
| Arrays             | 15 | 1 | 7 | 5 | 2 |
| Basic              | 67 | 0 | 59 | 6 | 2 |
| Collections        | 22 | 6 | 14 | 2 | 0 |
| Data Structures    | 8 | 1 | 2 | 2 | 3 |
| Factories          | 6 | 0 | 3 | 3 | 0 |
| Inter-procedural   | 25 | 0 | 12 | 10 | 3 |
| Predicates         | 9 | 4 | 5 | 0 | 0 |
| Reflection         | 4 | 1 | 2 | 0 | 1 |
| Sanitizers         | 9 | 3 | 2 | 2 | 2 |
| Session            | 4 | 1 | 3 | 0 | 0 |
| Strong Updates     | 5 | 0 | 0 | 4 | 1 |
| *Total*            | *188* | *18* | *113* | *35* | *22* |

Total accuracy: 78,723%
#### JVM Bytecode Results
| Category           | #    | FP   | TP   | TN   | FN   |
| ------------------ | ---- | ---- | ---- | ---- | ---- |
| Aliasing           | 14 | 1 | 10 | 1 | 2 |
| Arrays             | 15 | 2 | 7 | 4 | 2 |
| Basic              | 67 | 0 | 59 | 6 | 2 |
| Collections        | 22 | 7 | 14 | 1 | 0 |
| Data Structures    | 8 | 2 | 3 | 1 | 2 |
| Factories          | 6 | 0 | 3 | 3 | 0 |
| Inter-procedural   | 25 | 0 | 8 | 10 | 7 |
| Predicates         | 9 | 3 | 5 | 1 | 0 |
| Reflection         | 4 | 1 | 2 | 0 | 1 |
| Sanitizers         | 9 | 3 | 2 | 2 | 2 |
| Session            | 4 | 1 | 3 | 0 | 0 |
| Strong Updates     | 5 | 0 | 0 | 4 | 1 |
| *Total*            | *188* | *20* | *116* | *33* | *19* |

Total accuracy: 79,255%

### IFSpec

IFSpec contains a diverse set of information flow benchmarks for Java programs.

* Java source: `sbt "benchmarks / testOnly *.ifspec.java.*"`
* JVM bytecode: `sbt "benchmarks / testOnly *.ifspec.jvm.*"`

#### Java Source Results
| Category           | #    | FP   | TP   | TN   | FN   |
| ------------------ | ---- | ---- | ---- | ---- | ---- |
| Aliasing           | 11 | 0 | 0 | 6 | 5 |
| Arrays             | 12 | 2 | 1 | 5 | 4 |
| Casting            | 2 | 1 | 0 | 0 | 1 |
| Class Initializer  | 7 | 2 | 2 | 2 | 1 |
| High Conditional   | 12 | 5 | 4 | 2 | 1 |
| Implicit Flows     | 32 | 10 | 11 | 5 | 6 |
| Exceptions         | 9 | 2 | 4 | 1 | 2 |
| Explicit Flows     | 41 | 13 | 10 | 12 | 6 |
| Library            | 7 | 2 | 4 | 0 | 1 |
| Simple             | 18 | 9 | 6 | 3 | 0 |
| *Total*            | *73* | *23* | *21* | *17* | *12* |

Total accuracy: 52,055%
#### JVM Bytecode Results
| Category           | #    | FP   | TP   | TN   | FN   |
| ------------------ | ---- | ---- | ---- | ---- | ---- |
| Aliasing           | 11 | 0 | 0 | 6 | 5 |
| Arrays             | 12 | 2 | 1 | 5 | 4 |
| Casting            | 2 | 1 | 1 | 0 | 0 |
| Class Initializer  | 7 | 2 | 2 | 2 | 1 |
| High Conditional   | 12 | 5 | 4 | 2 | 1 |
| Implicit Flows     | 32 | 10 | 12 | 5 | 5 |
| Exceptions         | 9 | 2 | 5 | 1 | 1 |
| Explicit Flows     | 41 | 12 | 10 | 13 | 6 |
| Library            | 7 | 2 | 4 | 0 | 1 |
| Simple             | 18 | 8 | 6 | 4 | 0 |
| *Total*            | *73* | *22* | *22* | *18* | *11* |

Total accuracy: 54,795%

### JInfoFlow

JInfoFlow-bench is a taint analysis benchmark suite containing 12 plain Java benchmarks exercising reflection,
event-driven architecture, and popular software engineering patterns.

* Java source: `sbt "benchmarks / testOnly *.jinfoflow.java.*"`
* JVM bytecode: `sbt "benchmarks / testOnly *.jinfoflow.jvm.*"`

#### Java Source Results
| Category           | #    | FP   | TP   | TN   | FN   |
| ------------------ | ---- | ---- | ---- | ---- | ---- |
| Basic              | 4 | 0 | 1 | 2 | 1 |
| Context            | 8 | 2 | 1 | 1 | 4 |
| Event              | 8 | 0 | 0 | 3 | 5 |
| *Total*            | *20* | *2* | *2* | *6* | *10* |

Total accuracy: 40,000%
#### JVM Bytecode Results
| Category           | #    | FP   | TP   | TN   | FN   |
| ------------------ | ---- | ---- | ---- | ---- | ---- |
| Basic              | 4 | 0 | 1 | 2 | 1 |
| Context            | 8 | 2 | 2 | 1 | 3 |
| Event              | 8 | 0 | 0 | 3 | 5 |
| *Total*            | *20* | *2* | *3* | *6* | *9* |

Total accuracy: 45,000%
