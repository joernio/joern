int index_into_dst_array (char *dst, char *src, int offset) {
  for(i = 0; i < strlen(src); i++) {
    dst[i + + j*8 + offset] = src[i];
  }
}

int vulnerable(size_t len, char *src) {
  char *dst = malloc(len + 8);
  memcpy(dst, src, len + 7);
}
