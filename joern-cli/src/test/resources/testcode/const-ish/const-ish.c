void modify_non_const_struct(struct Foo* foo) {
  foo = NULL;
  return;
}

void modify_const_struct_c_cast(const struct Foo* foo) {
  struct Foo* foo2 = (Foo*) foo;
  foo2 = NULL;
  return;
}

void modify_const_struct_cpp_cast(const struct Foo* foo) {
  struct Foo* foo2 = const_cast<Foo*>(foo);
  foo2 = NULL;
  return;
}

void modify_non_const_struct_member(struct Foo* foo) {
  foo->y = 5;
  return;
}

void modify_const_struct_member(const struct Foo* foo) {
  foo->y = 5;
  return;
}

void modify_const_struct_member_c_cast(const struct Foo* foo) {
  struct Foo* foo2 = (Foo*) foo;
  foo2->y = 5;
  return;
}

void modify_const_struct_member_cpp_cast(const struct Foo* foo) {
  struct Foo* foo2 = const_cast<Foo*>(foo);
  foo2->y = 5;
  return;
}

class Oop {
  Foo* fooA;
  const Foo* fooB;

  void non_const_member() {
    fooA = NULL;
    return;
  }

  void const_member() {
    fooB = NULL;
    return;
  }

  void const_member_c_cast() {
    Foo* f = (Foo*) this->fooB;
    f = NULL;
    return;
  }

  void const_member_cpp_cast() {
    Foo* f = const_cast<Foo*>(this->fooB);
    f = NULL;
    return;
  }

  void non_const_member_update() {
    fooA->y = 7;
    return;
  }

  void const_member_update() {
    fooB->y = 7;
    return;
  }

  void const_c_cast_member_update() {
    Foo* f = (Foo*) this->fooB;
    f->y = 7;
    return;
  }

  void const_cpp_cast_member_update() {
    Foo* f = const_cast<Foo*>(this->fooB);
    f->y = 7;
    return;
  }
};
