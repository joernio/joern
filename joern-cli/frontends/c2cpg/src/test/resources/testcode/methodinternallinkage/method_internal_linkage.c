void method1() {
  int x;
  x = 1;
}

void method2(int x) {
  x = 1;
}

void method3(int x) {
  int y;
  {
    int x;
    int y;

    x = 1;
    y = 1;
  }

  x = 1;
  y = 1;
}