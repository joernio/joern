#include <sys/time.h>

int main() {
    int time = gettimeofday();

    int x;
    int y = 5;
    get_user(x, &y);

    exit(0);
}
