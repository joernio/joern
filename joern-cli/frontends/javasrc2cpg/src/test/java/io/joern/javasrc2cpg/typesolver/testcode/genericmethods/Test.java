package io.joern.javasrc2cpg.typesolver.testcode.genericmethods;

public class Test<S> {

    public <T> String foo(S s, T t) {
        return s.toString() + t.toString();
    }
}
