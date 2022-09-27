/*
Copyright (C) 2012-2014 EPFL
Copyright (C) 2012-2014 Typesafe, Inc.

All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright notice,
      this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright notice,
      this list of conditions and the following disclaimer in the documentation
      and/or other materials provided with the distribution.
    * Neither the name of the EPFL nor the names of its contributors
      may be used to endorse or promote products derived from this software
      without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package io.github.retronym.java9rtexport;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.nio.file.*;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

public class Export {
    private final static Object lock = new Object();
    private static File tempFile = null;

    public static String rtJarName = "rt-" + System.getProperty("java.version") + ".jar";

    public static File rt() {
        try {
            synchronized (lock) {
                if (tempFile == null) {
                    Path tempPath = Files.createTempFile("rt", ".jar");
                    tempFile = tempPath.toFile();
                    tempFile.deleteOnExit();
                    tempFile.delete();
                    FileSystem fileSystem = FileSystems.getFileSystem(URI.create("jrt:/"));
                    Path path = fileSystem.getPath("/modules");
                    URI uri = URI.create("jar:" + tempPath.toUri());
                    Map<String, String> env = new HashMap<>();
                    env.put("create", "true");
                    try (FileSystem zipfs = FileSystems.newFileSystem(uri, env)) {
                        Iterator<Path> iterator = Files.list(path).iterator();
                        while (iterator.hasNext()) {
                            Path next = iterator.next();
                            Copy.copyDirectory(next, zipfs.getPath("/"));
                        }
                    }
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
            System.exit(-1);
        }
        return tempFile;
    }

    public static boolean rtTo(File dest, boolean verbose) {
        try {
            if (!dest.exists()) {
                if (verbose) {
                    System.out.println("Copying Java " +
                            System.getProperty("java.version") +
                            " runtime jar to " +
                            dest.getParentFile() +
                            " ...");
                    System.out.flush();
                }
                dest.getParentFile().mkdirs();
                java.nio.file.Files.copy(rt().toPath(), dest.toPath());
                return true;
            }
        } catch (IOException e) {
            e.printStackTrace();
            System.exit(-1);
        }
        return false;
    }

    public static File rtAt(File dir, boolean verbose) {
        File f = new File(dir, rtJarName);
        rtTo(f, verbose);
        return f;
    }

    public static File rtAt(File dir) {
        return rtAt(dir, false);
    }
}
