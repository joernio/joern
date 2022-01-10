#include <stdio.h>

// The code is vulnerable on purpose
int main (int argc, char**argv) {
   char buf[30];

   sprintf(buf, "Provided value = %s", argv[1]);
   puts(buf);
   
   return(0);
}
