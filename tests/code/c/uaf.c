#include <stddef.h>
#include <stdlib.h>

void *bad() {
	void *x = NULL;
	free(x);
	return x;
}

void *false_positive() {
    void *x = NULL;
	free(x);
	x = NULL;
	return x;
}

int main(int argc, char * argv[]){
    bad();
}
