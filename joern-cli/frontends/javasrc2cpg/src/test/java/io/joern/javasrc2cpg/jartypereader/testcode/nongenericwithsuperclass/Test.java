package io.joern.javasrc2cpg.jartypereader.testcode.nongenericwithsuperclass;

class Foo {}

public class Test extends Foo implements Comparable {

    @Override
    public int compareTo(Object o) {
        return 0;
    }
}
