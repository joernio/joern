package io.joern.javasrc2cpg.typesolver.testcode.simplegeneric;

public class Test <T> {

    public <S> S foo(S s, T t) {
        return s;
    }
}
