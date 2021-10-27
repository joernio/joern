#include <stdio.h>
#include <string.h>
#include <stdlib.h>

// gcc -fno-stack-protector -z execstack -no-pie -o buf2 buf2.c
int main(int argc, char *argv[]) {
  const char* inEnv = getenv("BUF2IN");
  if (inEnv == NULL) {
    printf("BUF2IN environment variable not set.");
    return -1;
  }

  char c[6];
  strcpy(c, inEnv);
  printf("First argument is: %s\n", c);
  return 0;
}

