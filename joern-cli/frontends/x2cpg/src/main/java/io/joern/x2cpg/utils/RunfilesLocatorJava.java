package io.joern.x2cpg.utils;

import java.io.IOException;
import java.util.Optional;

// This class is only used in SBT build where runfiles location is not a
// build concept and thus handled on a case by case basis.
// The SBT implementation just returns empty to indicate that the runfiles
// cannot be retrieved.
class JoernRunfilesLocatorJava {
    static Optional<String> resolve(String path) throws IOException {
        return Optional.empty();
    }

}
