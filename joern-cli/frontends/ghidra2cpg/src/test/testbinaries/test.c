#include <stdio.h>
#include <string.h>

// refNodeTests 
void refNodeTests() {
   int x = 10;
   int y = 10;
}
// Dataflowtests
int dataflow() {
  asm ("add $1, %eax\n\t"
    "mov %eax, %edx\n\t"
    "mov %edx, %ecx\n\t"
    "mov %ecx, %eax");
}

// CallNodeTests
int level3() { return 1; }
int level2() { return level3(); }
int level1() { return level2(); }

// LiteralNodeTests
void literalNodeTest() {
  int x;
  char y[] = "TEST";
  x = 100;
}
void localNodeTests() {
  int x = 10; 
}
int main (int argc, char** argv) {
  int x = 10; 
  level1();
  literalNodeTest();
  // parameterNodeTests
  printf("hello world");
}
