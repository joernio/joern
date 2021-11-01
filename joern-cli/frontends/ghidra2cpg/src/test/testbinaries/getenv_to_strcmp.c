#include <stdio.h>
#include <string.h>
#include <stdlib.h>

// $ gcc -o getenv_to_strcmp getenv_to_strcmp.c
//

int main(int argc, char *argv[]) {
  const char* in = getenv("IN");
  if (auth == NULL) {
    printf("IN environment variable not set.\n");
    return -1;
  }
  if (strcmp(in, "GREET") == 0) {
    printf("Greetings!\n");
  }
  return 0;
}
