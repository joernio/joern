"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.mdreport = mdreport;

function _toConsumableArray(arr) { if (Array.isArray(arr)) { for (var i = 0, arr2 = Array(arr.length); i < arr.length; i++) { arr2[i] = arr[i]; } return arr2; } else { return Array.from(arr); } }

var fs = require('fs');
var parser = require('@solidity-parser/parser');
var sha1File = require('sha1-file');
var importer = require('../lib/utils/importer');

function mdreport(infiles) {
  var options = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : {};

  var content = '';

  if (infiles.length === 0) {
    throw new Error('\nNo files were specified for analysis in the arguments. Bailing...\n');
  }

  var filesTable = '\n|  File Name  |  SHA-1 Hash  |\n|-------------|--------------|\n';

  var contractsTable = '\n|  Contract  |         Type        |       Bases      |                  |                 |\n|:----------:|:-------------------:|:----------------:|:----------------:|:---------------:|\n|     \u2514      |  **Function Name**  |  **Visibility**  |  **Mutability**  |  **Modifiers**  |\n';

  // make the files array unique by typecasting them to a Set and back
  // this is not needed in case the importer flag is on, because the 
  // importer module already filters the array internally
  if (!options.contentsInFilePath && options.importer) {
    infiles = importer.importProfiler(infiles);
  } else {
    infiles = [].concat(_toConsumableArray(new Set(infiles)));
  }

  var _loop = function _loop(file) {
    filesTable += '| ' + file + ' | ' + sha1File(file) + ' |\n';

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

    isPublic = false;
    doesModifierExist = false;
    isConstructor = false;


    parser.visit(ast, {
      ContractDefinition: function ContractDefinition(node) {

        var name = node.name;
        var bases = node.baseContracts.map(function (spec) {
          return spec.baseName.namePath;
        }).join(', ');

        var specs = '';
        if (node.kind === 'library') {
          specs += 'Library';
        } else if (node.kind === 'interface') {
          specs += 'Interface';
        } else {
          specs += 'Implementation';
        }

        contractsTable += '||||||\n| **' + name + '** | ' + specs + ' | ' + bases + ' |||\n';
      },
      FunctionDefinition: function FunctionDefinition(node) {
        var name = void 0;
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

        var spec = '';
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

        var payable = '';
        if (node.stateMutability === 'payable') {
          payable = '💵';
        }

        var mutating = '';
        if (!node.stateMutability) {
          mutating = '🛑';
        }

        contractsTable += '| \u2514 | ' + name + ' | ' + spec + ' | ' + mutating + ' ' + payable + ' |';
      },


      'FunctionDefinition:exit': function FunctionDefinitionExit(node) {
        if (!isConstructor && isPublic && !doesModifierExist) {
          contractsTable += 'NO❗️';
        }
        contractsTable += ' |\n';
      },

      ModifierInvocation: function ModifierInvocation(node) {
        doesModifierExist = true;
        contractsTable += ' ' + node.name;
      }
    });
  };

  var _iteratorNormalCompletion = true;
  var _didIteratorError = false;
  var _iteratorError = undefined;

  try {
    for (var _iterator = infiles[Symbol.iterator](), _step; !(_iteratorNormalCompletion = (_step = _iterator.next()).done); _iteratorNormalCompletion = true) {
      var file = _step.value;
      var isPublic;
      var doesModifierExist;
      var isConstructor;

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

  var reportContents = '#'.repeat(options.deepness) + ' S\u016Brya\'s Description Report\n\n' + '#'.repeat(options.deepness + 1) + ' Files Description Table\n\n' + filesTable + '\n\n' + '#'.repeat(options.deepness + 1) + ' Contracts Description Table\n\n' + contractsTable + '\n\n' + '#'.repeat(options.deepness + 1) + ' Legend\n\n|  Symbol  |  Meaning  |\n|:--------:|-----------|\n|    \uD83D\uDED1    | Function can modify state |\n|    \uD83D\uDCB5    | Function is payable |\n';

  return reportContents;
}