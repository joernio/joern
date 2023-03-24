package io.joern.javasrc2cpg.jartypereader.testcode.wildcards;

import java.util.List;
public class Test {
    List noType;
    List<?> unboundWildcard;
    List<? extends String> boundAbove;
    List<? super String> boundBelow;
}

