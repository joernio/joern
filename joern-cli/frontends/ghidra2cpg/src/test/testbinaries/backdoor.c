#include <stdio.h>
#include <string.h>
#include <stdlib.h>

// `backdoor.c`
//
//
// This program demonstrates how an authentication backdoor might be implemented.
// Possibly inspired by implementations D-Link shipped to their customers in 2021.
//
// To compile:
// $ gcc -o backdoor backdoor.c
//
// To trigger the legitimate authentication, create a file `/tmp/secret.txt` and
// place the string `SECRET` for example on its first line; then call the program like so:
// ```
// $ AUTH="SECRET" ACTION="GREET" CMD="CMD" ./backdoor
// Auth check succeeded.
// ```
//
// To trigger the backdoor with the _GREET_ action:
// ```
// $ AUTH="GSGFP" ACTION="GREET" CMD="CMD" ./backdoor
// Auth check succeeded.
// Greetings!
// ```
//
// To trigger the backdoor with the _EXECUTE_ action:
// ```
// $ AUTH="EYG@QQC" ACTION="EXECUTE" CMD="echo 'PWNED' > /tmp/pwned.txt" ./backdoor
// Auth check succeeded.
// Command `echo 'PWNED' > /tmp/pwned.txt` executed, exit code 0.
// $ cat /tmp/pwned.txt
// PWNED
// ```

int dlink_style_auth_check(const char *param_1, const char *param_2) {
  const unsigned MAX_LENGTH = 64;
  char shady[MAX_LENGTH];
  strcpy(shady, param_2);
  int i;
  for (i = 0; i < strlen(shady); i++) {
    shady[i] = shady[i] ^ (i % 256);
  }
  // uncomment to print the backdoor string
  // printf("shady: `%s`", shady);
  if (strcmp(shady, param_1) == 0) {
    return 1;
  }
  return -1;
}

int check_auth(const char *param_1, const char *param_2) {
  FILE *fptr;
  const unsigned MAX_LENGTH = 64;
  char buffer[MAX_LENGTH];

  // the legitimate authentication
  fptr = fopen("/tmp/secret.txt", "r");
  if (fptr != NULL) {
    // read first line into buffer
    fgets(buffer, MAX_LENGTH, fptr);
    char *trimmed = strtok(buffer, "\n");
    fclose(fptr);
    // auth succeeded
    if (strcmp(trimmed, param_1) == 0) {
      return 1;
    }
    // backdoor
    return dlink_style_auth_check(param_1, param_2);
  }
  // backdoor
  return dlink_style_auth_check(param_1, param_2);
}

int main(int argc, char *argv[]) {
  const char* auth = getenv("AUTH");
  const char* action = getenv("ACTION");
  const char* cmd = getenv("CMD");
  if (auth == NULL) {
    printf("AUTH environment variable not set.\n");
    return -1;
  }
  if (action == NULL) {
    printf("ACTION environment variable not set.\n");
    return -1;
  }
  if (cmd == NULL) {
    printf("CMD environment variable not set.\n");
    return -1;
  }

  if (check_auth(auth, action) < 0) {
    printf("Auth check failed, exiting.\n");
    return -1;
  }
  printf("Auth check succeeded.\n");

  if (strcmp(action, "GREET") == 0) {
    printf("Greetings!\n");
  } else if (strcmp(action, "EXECUTE") == 0) {
    int cmd_exit = system(cmd);
    printf("Command `%s` executed, exit code %d.\n", cmd, cmd_exit);
  }
  return 0;
}

