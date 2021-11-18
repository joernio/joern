const mysql = require('mysql');

var connection = mysql.createConnection();

connection.connect();

function getUserInput() {
    return "";
}

function lookForProperty(property) {
    var query = property;
    queryDatabase(query);
}

function queryDatabase(sql) {
    var result = "";
    connection.query("SELECT * from " + sql, function (err, rows, fields) {
        if (err != "") console.log("error");
        result = rows[0].solution;
    });
    return result;
}

var input = getUserInput();
lookForProperty(input);

connection.end();
