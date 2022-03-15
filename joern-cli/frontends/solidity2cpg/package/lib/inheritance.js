"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.inheritance = inheritance;

function _toConsumableArray(arr) { if (Array.isArray(arr)) { for (var i = 0, arr2 = Array(arr.length); i < arr.length; i++) { arr2[i] = arr[i]; } return arr2; } else { return Array.from(arr); } }

var fs = require('fs');
var parser = require('@solidity-parser/parser');
var graphviz = require('graphviz');

var _require = require('c3-linearization'),
    linearize = _require.linearize;

var importer = require('../lib/utils/importer');

function inheritance(files) {
  var options = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : {};

  if (files.length === 0) {
    throw new Error('\nNo files were specified for analysis in the arguments. Bailing...\n');
  }

  var digraph = graphviz.digraph('G');
  digraph.set('ratio', 'auto');
  digraph.set('page', '40');

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
    var dependencies = {};

    parser.visit(ast, {
      ContractDefinition: function ContractDefinition(node) {
        contractName = node.name;

        if (!digraph.getNode(contractName)) {

          digraph.addNode(contractName);
        }

        dependencies[contractName] = node.baseContracts.map(function (spec) {
          return spec.baseName.namePath;
        });

        var _iteratorNormalCompletion2 = true;
        var _didIteratorError2 = false;
        var _iteratorError2 = undefined;

        try {
          for (var _iterator2 = dependencies[contractName][Symbol.iterator](), _step2; !(_iteratorNormalCompletion2 = (_step2 = _iterator2.next()).done); _iteratorNormalCompletion2 = true) {
            var dep = _step2.value;

            if (!digraph.getNode(dep)) {

              digraph.addNode(dep);
            }

            digraph.addEdge(contractName, dep);
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
      }
    });

    dependencies = linearize(dependencies, { reverse: true });
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

  return digraph.to_dot();
}