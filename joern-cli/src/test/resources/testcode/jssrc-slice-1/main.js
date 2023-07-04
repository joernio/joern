const express = require('express')
const app = express()
const port = 3000

app.get('/', (req, res) => {
    res.send('Hello World!')
})

app.listen(port, () => {
    console.log(`Example app listening on port ${port}`)
})

console.log(app)

function notHiddenByClosure() {
    console.debug(app)
}

class Car {
    constructor(name, year) {
        this.name = name;
        this.year = year;
    }

    rev() {
        return "vroom";
    }

}

function carTest() {
    const c = new Car("Noodle", 2012);
    c.rev();
}
