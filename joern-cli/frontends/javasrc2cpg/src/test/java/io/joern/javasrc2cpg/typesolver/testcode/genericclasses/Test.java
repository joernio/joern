package io.joern.javasrc2cpg.typesolver.testcode.genericclasses;

class Foo<T, S> {}

public class Test<T> extends Foo<T, String> {}
