"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.dependencies = dependencies;
exports.dependenciesPrint = dependenciesPrint;

function _toConsumableArray(arr) { if (Array.isArray(arr)) { for (var i = 0, arr2 = Array(arr.length); i < arr.length; i++) { arr2[i] = arr[i]; } return arr2; } else { return Array.from(arr); } }

var fs = require('fs');
var parser = require('@solidity-parser/parser');

var _require = require('c3-linearization'),
    linearize = _require.linearize;

var importer = require('../lib/utils/importer');

/**
 * @param  {array} files A list of files required to resolve dependency graph
 * @param  {string} childContract The name of the contract to derive
 * @returns {array} A c3-linearized list of the of the dependency graph
 */
function dependencies(files, childContract) {
  var options = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : {};

  if (files.length === 0) {
    throw new Error('\nNo files were specified for analysis in the arguments. Bailing...\n');
  }

  if (!childContract) {
    throw new Error('\nNo target contract specified in the arguments. Bailing...\n');
  }

  // initialize vars that persist over file parsing loops
  var dependencies = {};

  // make the files array unique by typecasting them to a Set and back
  // this is not needed in case the importer flag is on, because the 
  // importer module already filters the array internally
  if (!options.contentsInFilePath && options.importer) {
    files = importer.importProfiler(files);
  } else {
    files = [].concat(_toConsumableArray(new Set(files)));
  }

  var _loop = function _loop(file) {

    var content = void 0;
    if (!options.contentsInFilePath) {
      try {
        content = fs.readFileSync(file).toString('utf-8');
      } catch (e) {
        if (e.code === 'EISDIR') {
          console.error('Skipping directory ' + file);
          return 'continue';
        } else {
          throw e;
        }
      }
    } else {
      content = file;
    }

    var ast = function () {
      try {
        return parser.parse(content);
      } catch (err) {
        if (!options.contentsInFilePath) {
          console.error('\nError found while parsing the following file: ' + file + '\n');
        } else {
          console.error('\nError found while parsing one of the provided files\n');
        }
        throw err;
      }
    }();

    var contractName = null;

    parser.visit(ast, {
      ContractDefinition: function ContractDefinition(node) {
        contractName = node.name;

        dependencies[contractName] = node.baseContracts.map(function (spec) {
          return spec.baseName.namePath;
        });
      }
    });
  };

  var _iteratorNormalCompletion = true;
  var _didIteratorError = false;
  var _iteratorError = undefined;

  try {
    for (var _iterator = files[Symbol.iterator](), _step; !(_iteratorNormalCompletion = (_step = _iterator.next()).done); _iteratorNormalCompletion = true) {
      var file = _step.value;

      var _ret = _loop(file);

      if (_ret === 'continue') continue;
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

  if (!dependencies[childContract]) {
    throw new Error('\nSpecified child contract not found. Bailing...\n');
  }

  dependencies = linearize(dependencies, { reverse: true });

  return dependencies[childContract];
}

/**
 * A function designed to return a nicely formatted string to be printed
 * @param  {array} files A list of files required to resolve dependency graph
 * @param  {string} childContract The name of the contract to derive
 * @returns {array} A c3-linearized list of the of the dependency graph
 */
function dependenciesPrint(files, childContract) {
  var noColorOutput = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : false;

  var outputString = '';

  var derivedLinearization = dependencies(files, childContract);

  if (derivedLinearization) {
    outputString += noColorOutput ? derivedLinearization[0] : derivedLinearization[0].yellow;

    if (derivedLinearization.length < 2) {
      outputString += '\nNo Dependencies Found';
      return outputString;
    }
    derivedLinearization.shift();

    var reducer = function reducer(accumulator, currentValue) {
      return accumulator + '\n  \u2196 ' + currentValue;
    };
    outputString += '\n  \u2196 ' + derivedLinearization.reduce(reducer);
  }

  return outputString;
}