#include <stdio.h>
#include <string.h>

// gcc -fno-stack-protector -z execstack -no-pie -o buf1 buf1.c
int main(int argc, char *argv[]) {
  if (argc == 1) {
    printf("Program executed with no arguments.\n");
    return 0;
  }
  char c[6];
  strcpy(c, argv[1]);
  printf("First argument is: %s\n", c);
  return 0;
}
