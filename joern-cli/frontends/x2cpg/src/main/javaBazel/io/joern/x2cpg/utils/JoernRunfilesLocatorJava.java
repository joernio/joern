package io.joern.x2cpg.utils;

import com.google.devtools.build.runfiles.Runfiles;
import com.google.devtools.build.runfiles.AutoBazelRepository;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Optional;

// This class is only part of the Bazel build.
@AutoBazelRepository
class JoernRunfilesLocatorJava {
  static Optional<String> resolve(String path) throws IOException {
    // The AutoBazelRepository_RunfilesLocator class is auto generated based on the @AutoBazelRepository
    // annotation.
    Runfiles runfiles = Runfiles.preload().withSourceRepository(AutoBazelRepository_JoernRunfilesLocatorJava.NAME);
    String resolvedPath = runfiles.rlocation(path);
    if (Files.exists(Path.of(resolvedPath))) {
      return Optional.of(resolvedPath);
    } else {
      return Optional.empty();
    }
  }

}
