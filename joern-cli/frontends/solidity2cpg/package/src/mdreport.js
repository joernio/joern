"use strict";

const fs = require('fs');
const parser = require('@solidity-parser/parser');
const sha1File = require('sha1-file');
const importer = require('../lib/utils/importer');

export function mdreport(infiles, options = {}) {
  let content = '';

  if (infiles.length === 0) {
    throw new Error(`\nNo files were specified for analysis in the arguments. Bailing...\n`);
  }

  let filesTable = `
|  File Name  |  SHA-1 Hash  |
|-------------|--------------|
`;

  let contractsTable = `
|  Contract  |         Type        |       Bases      |                  |                 |
|:----------:|:-------------------:|:----------------:|:----------------:|:---------------:|
|     └      |  **Function Name**  |  **Visibility**  |  **Mutability**  |  **Modifiers**  |
`;

  // make the files array unique by typecasting them to a Set and back
  // this is not needed in case the importer flag is on, because the 
  // importer module already filters the array internally
  if(!options.contentsInFilePath && options.importer) {
    infiles = importer.importProfiler(infiles);
  } else {
    infiles = [...new Set(infiles)];
  }

  for (let file of infiles) {
    filesTable += `| ${file} | ${sha1File(file)} |
`;

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

    var isPublic = false;
    var doesModifierExist = false;
    var isConstructor = false;

    parser.visit(ast, {
      ContractDefinition(node) {

        const name = node.name;
        let bases = node.baseContracts.map(spec => {
          return spec.baseName.namePath;
        }).join(', ');

        let specs = '';
        if (node.kind === 'library') {
          specs += 'Library';
        } else if (node.kind === 'interface') {
          specs += 'Interface';
        } else {
          specs += 'Implementation';
        }

        contractsTable += `||||||
| **${name}** | ${specs} | ${bases} |||
`;
      },

      FunctionDefinition(node) {
        let name;
        isPublic = false;
        doesModifierExist = false;
        isConstructor = false;

        if (node.isConstructor) {
          name = '<Constructor>';
        } else if (node.isFallback) {
          name = '<Fallback>';
        } else if (node.isReceiveEther) {
          name = '<Receive Ether>';
        } else {
          name = node.name;
        }


        let spec = '';
        if (node.visibility === 'public' || node.visibility === 'default') {
          spec += 'Public ❗️';
          isPublic = true;
        } else if (node.visibility === 'external') {
          spec += 'External ❗️';
          isPublic = true;
        } else if (node.visibility === 'private') {
          spec += 'Private 🔐';
        } else if (node.visibility === 'internal') {
          spec += 'Internal 🔒';
        }

        let payable = '';
        if (node.stateMutability === 'payable') {
          payable = '💵';
        }

        let mutating = '';
        if (!node.stateMutability) {
          mutating = '🛑';
        }

        contractsTable += `| └ | ${name} | ${spec} | ${mutating} ${payable} |`;
      },

      'FunctionDefinition:exit': function(node) {
        if (!isConstructor && isPublic && !doesModifierExist) {
          contractsTable += 'NO❗️';
        }
        contractsTable += ` |
`;
      },

      ModifierInvocation(node) {
        doesModifierExist = true;
        contractsTable += ` ${node.name}`;
        
      }
    });
  }

  const reportContents = `${'#'.repeat(options.deepness)} Sūrya's Description Report

${'#'.repeat(options.deepness + 1)} Files Description Table

${filesTable}

${'#'.repeat(options.deepness + 1)} Contracts Description Table

${contractsTable}

${'#'.repeat(options.deepness + 1)} Legend

|  Symbol  |  Meaning  |
|:--------:|-----------|
|    🛑    | Function can modify state |
|    💵    | Function is payable |
`;
  
  return reportContents;
}
