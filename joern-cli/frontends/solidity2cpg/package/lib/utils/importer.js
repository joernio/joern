"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});

var _typeof = typeof Symbol === "function" && typeof Symbol.iterator === "symbol" ? function (obj) { return typeof obj; } : function (obj) { return obj && typeof Symbol === "function" && obj.constructor === Symbol && obj !== Symbol.prototype ? "symbol" : typeof obj; };

exports.importProfiler = importProfiler;
exports.resolveImportPath = resolveImportPath;
var fs = require('fs');
var path = require('path');
var parser = require('@solidity-parser/parser');

/**
 * Given a list of solidity files, returns a list of imports to those files, and all files imported
 * by those files.  For security, this function throws if a path is resolved to a higher level than 
 * projectDir, and it not a file name ending in .sol 
 *
 * @param      {Array}   files          files to parse for imports
 * @param      {string}  projectDir     the highest level directory accessible
 * @param      {Set}     importedFiles  files already parsed 
 * @return     {Array}   importPaths    A list of importPaths 
 */
function importProfiler(files) {
  var projectDir = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : process.cwd();
  var importedFiles = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : new Set();

  var _loop = function _loop(_file) {
    // Checks for a valid solidity file
    _file = path.resolve(projectDir, _file);
    if (_file.indexOf(projectDir) != 0) {
      throw new Error('\nImports must be found in sub dirs of the project directory.\n      project dir: ' + projectDir + '\n      path: ' + _file + '\n');
    }
    var content = void 0;
    try {
      content = fs.readFileSync(_file).toString('utf-8');
    } catch (e) {
      if (e.code === 'EISDIR') {
        console.error('Skipping directory ' + _file);
        return {
          v: importedFiles
        }; // empty Set
      } else {
        throw e;
      }
    }
    // Having verified that it indeed is a solidity file, add it to set of importedFiles
    importedFiles.add(_file);
    var ast = function () {
      try {
        return parser.parse(content, { tolerant: true });
      } catch (err) {
        console.error('\nError found while parsing the following file: ' + _file + '\n');
        throw err;
      }
    }();

    // create an array to hold the imported files
    var newFiles = [];
    parser.visit(ast, {
      ImportDirective: function ImportDirective(node) {
        var newFile = resolveImportPath(_file, node.path, projectDir);
        if (!importedFiles.has(newFile)) newFiles.push(newFile);
      }
    });
    // Run through the array of files found in this file
    module.exports.importProfiler(newFiles, projectDir, importedFiles);
    file = _file;
  };

  var _iteratorNormalCompletion = true;
  var _didIteratorError = false;
  var _iteratorError = undefined;

  try {
    for (var _iterator = files[Symbol.iterator](), _step; !(_iteratorNormalCompletion = (_step = _iterator.next()).done); _iteratorNormalCompletion = true) {
      var file = _step.value;

      var _ret = _loop(file);

      if ((typeof _ret === 'undefined' ? 'undefined' : _typeof(_ret)) === "object") return _ret.v;
    }
    // Convert the set to an array for easy consumption
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

  var importedFilesArray = Array.from(importedFiles);
  return importedFilesArray;
}

/// Takes a filepath, and an import path found within it, and finds the corresponding source code
/// file. Throws an error if the resolved path is not a file.
///
/// @param      {string}  baseFilePath      The base file path
/// @param      {string}  importedFilePath  The imported file path
/// @param      {string}  projectDir        The top-most directory we will search in
///
function resolveImportPath(baseFilePath, importedFilePath) {
  var projectDir = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : process.cwd();

  var topmostDirArray = projectDir.split(path.sep);
  var resolvedPath = void 0;
  var baseDirPath = path.dirname(baseFilePath);
  // if it's a relative or absolute path:
  if (importedFilePath.slice(0, 1) === '.' || importedFilePath.slice(0, 1) === '/') {
    resolvedPath = path.resolve(baseDirPath, importedFilePath);
    // else it's most likely a special case using a remapping to node_modules dir in Truffle
  } else {
    // we use a string and not the array alone because of different windows and UNIX path roots
    var currentDir = path.resolve(baseDirPath, '..');
    var currentDirArray = baseDirPath.split(path.sep);
    var currentDirName = currentDirArray.pop();
    var nodeModulesDir = '';

    // while (currentDirName != 'contracts') {
    while (!fs.readdirSync(currentDir).includes('node_modules') && !nodeModulesDir) {
      // since we already know the current file is inside the project dir we can check if the
      // folder array length for the current dir is smaller than the top-most one, i.e. we are 
      // still inside the project dir. If not, throw
      if (topmostDirArray.length >= currentDirArray.length) {
        throw new Error('Import statement seems to be a Truffle "\'node_modules\' remapping" but no \'contracts\' truffle dir could be found in the project\'s child dirs. Have you ran \'npm install\', already?\n        project dir: ' + projectDir + '\n        path: ' + currentDir);
      }
      // if we still aren't in a folder containing 'node_modules' go up one level
      currentDirName = currentDirArray.pop();
      currentDir = path.resolve(currentDir, '..');
    }
    // if we've reached this point, then we have found the dir containing node_modules
    nodeModulesDir = path.join(currentDir, 'node_modules');

    // join it all to get the file path
    resolvedPath = path.join(nodeModulesDir, importedFilePath);
  }

  // verify that the resolved path is actually a file
  if (!fs.existsSync(resolvedPath) || !fs.statSync(resolvedPath).isFile()) {
    throw new Error('Import path (' + resolvedPath + ') not resolved to a file');
  }
  return resolvedPath;
}