int main() {
    int *p;
    int *q;
    int *r;
    int i = 1;

    int simple_subtraction = p - q;
    int nested_subtraction = p - q - r;
    int literal_subtraction = p - i;

    int addrOf_subtraction = p - &i;
    int nested_addrOf_subtraction =  3 - &i - 4;
    int literal_addrOf_subtraction = 3 - &i;

    int valid = i - 5;

    int x[5];
    int array_subtraction = x - p;
    int array_literal_subtraction = x - 3;
    int array_addrOf_subtraction = x - &i;

    struct Foo {
        int i;
        int* p;
    } foo_t;

    struct foo_t f*;
    int valid_struct = foo_t->i - 1;
    int unsafe_struct = foo_t->p - 1;

    return 0;
}