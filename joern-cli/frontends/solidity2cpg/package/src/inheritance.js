"use strict";

const fs = require('fs');
const parser = require('@solidity-parser/parser');
const graphviz = require('graphviz');
const { linearize } = require('c3-linearization');
const importer = require('../lib/utils/importer');

export function inheritance(files, options = {}) {
  if (files.length === 0) {
    throw new Error(`\nNo files were specified for analysis in the arguments. Bailing...\n`);
  }

  const digraph = graphviz.digraph('G');
  digraph.set('ratio', 'auto');
  digraph.set('page', '40');

  // make the files array unique by typecasting them to a Set and back
  // this is not needed in case the importer flag is on, because the 
  // importer module already filters the array internally
  if(!options.contentsInFilePath && options.importer) {
    files = importer.importProfiler(files);
  } else {
    files = [...new Set(files)];
  }

  for (let file of files) {

    let content;
    if(!options.contentsInFilePath) {
      try {
        content = fs.readFileSync(file).toString('utf-8');
      } catch (e) {
        if (e.code === 'EISDIR') {
          console.error(`Skipping directory ${file}`);
          continue;
        } else {
          throw e;
        }
      }
    } else {
      content = file;
    }

    const ast = (() => {
      try {
        return parser.parse(content);
      } catch (err) {
        if(!options.contentsInFilePath) {
          console.error(`\nError found while parsing the following file: ${file}\n`);
        } else {
          console.error(`\nError found while parsing one of the provided files\n`);
        }
        throw err;
      }
    })();

    let contractName = null;
    let dependencies = {};

    parser.visit(ast, {
      ContractDefinition(node) {
        contractName = node.name;

        if (!digraph.getNode(contractName)) {

          digraph.addNode(contractName);
        }


        dependencies[contractName] = node.baseContracts.map(spec =>
          spec.baseName.namePath
        );

        for (let dep of dependencies[contractName]) {
          if (!digraph.getNode(dep)) {

            digraph.addNode(dep);
          }

          digraph.addEdge(contractName, dep);
        }
      }
    });

    dependencies = linearize(dependencies, {reverse: true});
  }

  return digraph.to_dot();
}