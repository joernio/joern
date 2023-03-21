package io.joern.javasrc2cpg.jartypereader.testcode.overloadedmethods;

public class Test {
    public <S> void foo(S s) {
        System.out.println("IN S");
    }
    public <T extends Comparable> void foo(T t) {
        System.out.println("IN T");
    }
}
