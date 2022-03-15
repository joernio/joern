"use strict";

const fs = require('fs');
const parser = require('@solidity-parser/parser');
const { resolveImportPath } = require('./utils/importer');

export function flatten(files) {
  if (files.length === 0) {
    console.log('No files were specified for analysis in the arguments. Bailing...');
    return;
  }

  // create a set of paths already inserted to pass while flattening each file, to avoid duplication
  let visitedPaths = new Set();
  let flat = '';
  for (let file of files){
    if(visitedPaths.has(file)){
      continue;
    }
    let result = replaceImportsWithSource(file, visitedPaths);
    flat += result.flattenedContent;
    flat += '\n\n';
    visitedPaths.add(result.visitedPaths);
  }
  console.log(flat);
}

/**  
 * Given a solidity file, returns the content with imports replaced by source code
 * 
 * @param      {string}  file  The file
 * @param      {Array}   visitedPaths     Paths already resolved that should be skipped if seen again
 * @return     {object}  { resolvedContent: A string with imports replaced by source code, visitedPaths }
 */
function replaceImportsWithSource(file, visitedPaths = new Set()) {
  let content;
  try {
    content = fs.readFileSync(file).toString('utf-8');
  } catch (e) {
    if (e.code === 'EISDIR') {
      console.error(`Skipping directory ${file}`);
    } else {
      throw e;
    }
  }

  // prepend the code with a space and comment helpful for the flattened output
  content = `// The following code is from flattening this file: ${file}\n${content}`;

  const ast = (() => {
    try {
      return parser.parse(content, {loc: true});
    } catch (err) {
      console.error(`\nError found while parsing the following file: ${file}\n`);
      throw err;
    }
  })();

  let importsAndLocations = [];
  parser.visit(ast, {
    ImportDirective(node) {
      let importPath = resolveImportPath(file, node.path);
      importsAndLocations.push({ importPath, location: node.loc });
    }
  });
  
  let contentLines = content.split('\n');
  for (let el of importsAndLocations){
    // arrays are 0-indexed, file lines are 1-indexed so the statement is at `start.line - 1`
    let importStatementText = contentLines[el.location.start.line - 1];
    
    if (!visitedPaths.has(el.importPath)){
      // first time handling this import path, comment it out, and replace with flattened source code
      contentLines[el.location.start.line - 1] =  
      `// The following code is from flattening this import statement in: ${file}\n// ${importStatementText}\n${replaceImportsWithSource(el.importPath, visitedPaths).flattenedContent}`;
      
    } else {
      // we've already visited this path, just comment out the import statement
      contentLines[el.location.start.line - 1] = 
        `// Skipping this already resolved import statement found in ${file} \n// ${importStatementText}`;
      
      visitedPaths.add(el.importPath);
    }
  }
  
  visitedPaths.add(file);

  return {
    flattenedContent: contentLines.join('\n'),
    visitedPaths
  };
}
