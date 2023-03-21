package io.joern.javasrc2cpg.jartypereader.testcode.genericmethods;

public class Test<S> {

    public <T> String foo(S s, T t) {
        return s.toString() + t.toString();
    }
}
