int main() {
  int* nested;
  if ((nested = malloc(sizeof(int) * 42)) == NULL) return NULL;

  int* multiplication = malloc(sizeof(int) * 3);

  int* addition = malloc(sizeof(int) + 55);

  int* valid = malloc(sizeof(int));

  return 0;
}
