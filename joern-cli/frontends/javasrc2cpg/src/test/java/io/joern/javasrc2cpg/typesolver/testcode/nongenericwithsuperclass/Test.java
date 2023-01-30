package io.joern.javasrc2cpg.typesolver.testcode.nongenericwithsuperclass;

class Foo {}

public class Test extends Foo implements Comparable {

    @Override
    public int compareTo(Object o) {
        return 0;
    }
}
