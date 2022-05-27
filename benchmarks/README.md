
### Securibench Micro

The benchmark can be run using either the Java source code frontend
or JVM bytecode Jimple frontend:

* Java source: `sbt "benchmarks / testOnly *.securibench.micro.java.*"`
* JVM bytecode: `sbt "benchmarks / testOnly *.securibench.micro.jvm.*"`

#### Java Source Results
| Category          | #    | FP   | TP   | TN   | FN   |
| ----------------- | ---- | ---- | ---- | ---- | ---- |
| Aliasing          | 14 | 1 | 4 | 1 | 8 |
| Arrays            | 15 | 1 | 7 | 5 | 2 |
| Basic             | 67 | 0 | 58 | 6 | 3 |
| Collections       | 22 | 6 | 14 | 2 | 0 |
| Data Structures   | 8 | 0 | 2 | 3 | 3 |
| Factories         | 6 | 0 | 3 | 3 | 0 |
| Inter-procedural  | 25 | 0 | 8 | 10 | 7 |
| Predicates        | 9 | 4 | 5 | 0 | 0 |
| Reflection        | 4 | 1 | 1 | 0 | 2 |
| Sanitizers        | 9 | 3 | 2 | 2 | 2 |
| Session           | 4 | 1 | 3 | 0 | 0 |
| Strong Updates    | 5 | 0 | 0 | 4 | 1 |
| *Total*           | *188* | *17* | *107* | *36* | *28* |

Total accuracy: 76,064%
#### JVM Bytecode Results
| Category          | #    | FP   | TP   | TN   | FN   |
| ----------------- | ---- | ---- | ---- | ---- | ---- |
| Aliasing          | 14 | 1 | 10 | 1 | 2 |
| Arrays            | 15 | 2 | 7 | 4 | 2 |
| Basic             | 67 | 0 | 59 | 6 | 2 |
| Collections       | 22 | 7 | 14 | 1 | 0 |
| Data Structures   | 8 | 0 | 2 | 3 | 3 |
| Factories         | 6 | 0 | 3 | 3 | 0 |
| Inter-procedural  | 25 | 0 | 7 | 10 | 8 |
| Predicates        | 9 | 3 | 5 | 1 | 0 |
| Reflection        | 4 | 1 | 2 | 0 | 1 |
| Sanitizers        | 9 | 3 | 2 | 2 | 2 |
| Session           | 4 | 1 | 3 | 0 | 0 |
| Strong Updates    | 5 | 0 | 0 | 4 | 1 |
| *Total*           | *188* | *18* | *114* | *35* | *21* |

Total accuracy: 79,255%
