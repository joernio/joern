"use strict";

const fs = require('fs');
const parser = require('@solidity-parser/parser');
const treeify = require('treeify');

export function parse(file) {
    var options = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : {};
    const content = fs.readFileSync(file).toString('utf-8');
    const ast = (() => {
        try {
            return parser.parse(content);
        } catch (err) {
            console.log(`Error found while parsing the following file: ${file}`);
            throw err;
        }
    })();

   if (options.jsonOutput) {

       return JSON.stringify(ast);

   } else {

       return treeify.asTree(ast, true);
   } 
}
