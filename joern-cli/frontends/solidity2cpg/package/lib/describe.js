"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.describe = describe;

function _toConsumableArray(arr) { if (Array.isArray(arr)) { for (var i = 0, arr2 = Array(arr.length); i < arr.length; i++) { arr2[i] = arr[i]; } return arr2; } else { return Array.from(arr); } }

var fs = require('fs');
var parser = require('@solidity-parser/parser');
var importer = require('../lib/utils/importer');

function describe(files) {
  var options = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : {};
  var noColorOutput = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : false;

  if (files.length === 0) {
    throw new Error('\nNo files were specified for analysis in the arguments. Bailing...\n');
  }

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

    parser.visit(ast, {
      ContractDefinition: function ContractDefinition(node) {

        var name = node.name;
        var bases = node.baseContracts.map(function (spec) {
          return spec.baseName.namePath;
        }).join(', ');

        bases = bases.length ? noColorOutput ? '(' + bases + ')' : ('(' + bases + ')').gray : '';

        var specs = '';
        if (node.kind === 'library') {
          specs += noColorOutput ? '[Lib]' : '[Lib]'.yellow;
        } else if (node.kind === 'interface') {
          specs += noColorOutput ? '[Int]' : '[Int]'.blue;
        }

        console.log(' + ' + specs + ' ' + name + ' ' + bases);
      },


      'ContractDefinition:exit': function ContractDefinitionExit(node) {
        console.log('');
      },

      FunctionDefinition: function FunctionDefinition(node) {
        var name = void 0;

        if (node.isConstructor) {
          name = noColorOutput ? '<Constructor>' : '<Constructor>'.gray;
        } else if (node.isFallback) {
          name = noColorOutput ? '<Fallback>' : '<Fallback>'.gray;
        } else if (node.isReceiveEther) {
          name = noColorOutput ? '<Receive Ether>' : '<Receive Ether>'.gray;
        } else {
          name = node.name;
        }

        var spec = '';
        if (node.visibility === 'public' || node.visibility === 'default') {
          spec += noColorOutput ? '[Pub]' : '[Pub]'.green;
        } else if (node.visibility === 'external') {
          spec += noColorOutput ? '[Ext]' : '[Ext]'.blue;
        } else if (node.visibility === 'private') {
          spec += noColorOutput ? '[Prv]' : '[Prv]'.red;
        } else if (node.visibility === 'internal') {
          spec += noColorOutput ? '[Int]' : '[Int]'.gray;
        }

        var payable = '';
        if (node.stateMutability === 'payable') {
          payable = noColorOutput ? ' ($)' : ' ($)'.yellow;
        }

        var mutating = '';
        if (!node.stateMutability) {
          mutating = noColorOutput ? ' #' : ' #'.red;
        }

        var modifiers = '';
        var _iteratorNormalCompletion2 = true;
        var _didIteratorError2 = false;
        var _iteratorError2 = undefined;

        try {
          for (var _iterator2 = node.modifiers[Symbol.iterator](), _step2; !(_iteratorNormalCompletion2 = (_step2 = _iterator2.next()).done); _iteratorNormalCompletion2 = true) {
            var m = _step2.value;

            if (!!modifiers) modifiers += ',';
            modifiers += m.name;
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

        console.log('    - ' + spec + ' ' + name + payable + mutating);
        if (!!modifiers) {
          console.log('       - modifiers: ' + modifiers);
        }
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

    // Print a legend for symbols being used
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

  var mutationSymbol = noColorOutput ? ' #' : ' #'.red;
  var payableSymbol = noColorOutput ? ' ($)' : ' ($)'.yellow;

  console.log('\n' + payableSymbol + ' = payable function\n' + mutationSymbol + ' = non-constant function\n  ');
}