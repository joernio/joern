const int const_number() { return 5 + 5; }
int side_effect_number() { return 0; }

int eligible() {
    int result = const_number() + 5;
    return result;
}

int not_eligible() {
    int result = side_effect_number() + 5;
    return result;
}

int eligible_params(int n) {
    return n + 5;
}

void not_eligible_params(int* x) {
    (*x)--;
    *x -= 1;
    return;
}

int main() { return eligible() + not_eligible(); }
