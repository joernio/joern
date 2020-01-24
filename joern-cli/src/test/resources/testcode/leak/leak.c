int main() {
  int* leak = malloc(sizeof(int));
  int* number = malloc(sizeof(int));

  free(number);

  return 0;
}
