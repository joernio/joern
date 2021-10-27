#include <string.h>

void vulnerable_strcpy(char *dest, char *src, size_t size) {
    strcpy(dest, src);
    strncpy(dest, src, size);
}
