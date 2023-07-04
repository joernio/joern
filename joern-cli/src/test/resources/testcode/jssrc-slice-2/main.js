class Foo {

    constructor(a, b) {
        this.a = a;
        this.b = b;
    }

    getA() {
        return this.a;
    }
}

// x and y should be the same slice
function bar(y) {
    y.getA();
}
const x = new Foo(1, 2)

bar(x)
