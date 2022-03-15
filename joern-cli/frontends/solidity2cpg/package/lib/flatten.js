"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.flatten = flatten;
var fs = require('fs');
var parser = require('@solidity-parser/parser');

var _require = require('./utils/importer'),
    resolveImportPath = _require.resolveImportPath;

function flatten(files) {
  if (files.length === 0) {
    console.log('No files were specified for analysis in the arguments. Bailing...');
    return;
  }

  // create a set of paths already inserted to pass while flattening each file, to avoid duplication
  var visitedPaths = new Set();
  var flat = '';
  var _iteratorNormalCompletion = true;
  var _didIteratorError = false;
  var _iteratorError = undefined;

  try {
    for (var _iterator = files[Symbol.iterator](), _step; !(_iteratorNormalCompletion = (_step = _iterator.next()).done); _iteratorNormalCompletion = true) {
      var file = _step.value;

      if (visitedPaths.has(file)) {
        continue;
      }
      var result = replaceImportsWithSource(file, visitedPaths);
      flat += result.flattenedContent;
      flat += '\n\n';
      visitedPaths.add(result.visitedPaths);
    }
  } catch (err) {
    _didIteratorError = true;
    _iteratorError = err;
  } finally {
    try {
      if (!_iteratorNormalCompletion && _iterator.return) {
        _iterator.return();
      }
    } finally {
      if (_didIteratorError) {
        throw _iteratorError;
      }
    }
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
function replaceImportsWithSource(file) {
  var visitedPaths = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : new Set();

  var content = void 0;
  try {
    content = fs.readFileSync(file).toString('utf-8');
  } catch (e) {
    if (e.code === 'EISDIR') {
      console.error('Skipping directory ' + file);
    } else {
      throw e;
    }
  }

  // prepend the code with a space and comment helpful for the flattened output
  content = '// The following code is from flattening this file: ' + file + '\n' + content;

  var ast = function () {
    try {
      return parser.parse(content, { loc: true });
    } catch (err) {
      console.error('\nError found while parsing the following file: ' + file + '\n');
      throw err;
    }
  }();

  var importsAndLocations = [];
  parser.visit(ast, {
    ImportDirective: function ImportDirective(node) {
      var importPath = resolveImportPath(file, node.path);
      importsAndLocations.push({ importPath: importPath, location: node.loc });
    }
  });

  var contentLines = content.split('\n');
  var _iteratorNormalCompletion2 = true;
  var _didIteratorError2 = false;
  var _iteratorError2 = undefined;

  try {
    for (var _iterator2 = importsAndLocations[Symbol.iterator](), _step2; !(_iteratorNormalCompletion2 = (_step2 = _iterator2.next()).done); _iteratorNormalCompletion2 = true) {
      var el = _step2.value;

      // arrays are 0-indexed, file lines are 1-indexed so the statement is at `start.line - 1`
      var importStatementText = contentLines[el.location.start.line - 1];

      if (!visitedPaths.has(el.importPath)) {
        // first time handling this import path, comment it out, and replace with flattened source code
        contentLines[el.location.start.line - 1] = '// The following code is from flattening this import statement in: ' + file + '\n// ' + importStatementText + '\n' + replaceImportsWithSource(el.importPath, visitedPaths).flattenedContent;
      } else {
        // we've already visited this path, just comment out the import statement
        contentLines[el.location.start.line - 1] = '// Skipping this already resolved import statement found in ' + file + ' \n// ' + importStatementText;

        visitedPaths.add(el.importPath);
      }
    }
  } catch (err) {
    _didIteratorError2 = true;
    _iteratorError2 = err;
  } finally {
    try {
      if (!_iteratorNormalCompletion2 && _iterator2.return) {
        _iterator2.return();
      }
    } finally {
      if (_didIteratorError2) {
        throw _iteratorError2;
      }
    }
  }

  visitedPaths.add(file);

  return {
    flattenedContent: contentLines.join('\n'),
    visitedPaths: visitedPaths
  };
}