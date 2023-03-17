@main def main() = {
  assert(importCode.c.isAvailable, "C frontend should be available, but isn't")
  assert(importCode.cpp.isAvailable, "CPP frontend should be available, but isn't")
  assert(importCode.ghidra.isAvailable, "GHIDRA frontend should be available, but isn't")
  assert(importCode.kotlin.isAvailable, "KOTLIN frontend should be available, but isn't")
  assert(importCode.java.isAvailable, "JAVA frontend should be available, but isn't")
  assert(importCode.jvm.isAvailable, "JVM frontend should be available, but isn't")
  assert(importCode.javascript.isAvailable, "JAVASCRIPT frontend should be available, but isn't")
  assert(importCode.php.isAvailable, "PHP frontend should be available, but isn't")
  assert(importCode.python.isAvailable, "PYTHON frontend should be available, but isn't")

  println("frontends smoketest successful: all required frontends are available")

}
