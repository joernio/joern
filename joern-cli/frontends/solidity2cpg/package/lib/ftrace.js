"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});

var _slicedToArray = function () { function sliceIterator(arr, i) { var _arr = []; var _n = true; var _d = false; var _e = undefined; try { for (var _i = arr[Symbol.iterator](), _s; !(_n = (_s = _i.next()).done); _n = true) { _arr.push(_s.value); if (i && _arr.length === i) break; } } catch (err) { _d = true; _e = err; } finally { try { if (!_n && _i["return"]) _i["return"](); } finally { if (_d) throw _e; } } return _arr; } return function (arr, i) { if (Array.isArray(arr)) { return arr; } else if (Symbol.iterator in Object(arr)) { return sliceIterator(arr, i); } else { throw new TypeError("Invalid attempt to destructure non-iterable instance"); } }; }();

exports.ftrace = ftrace;

function _toConsumableArray(arr) { if (Array.isArray(arr)) { for (var i = 0, arr2 = Array(arr.length); i < arr.length; i++) { arr2[i] = arr[i]; } return arr2; } else { return Array.from(arr); } }

var parserHelpers = require('./utils/parserHelpers');
var fs = require('fs');
var parser = require('@solidity-parser/parser');

var _require = require('c3-linearization'),
    linearize = _require.linearize;

var treeify = require('treeify');
var importer = require('../lib/utils/importer');

function ftrace(functionId, accepted_visibility, files) {
  var options = arguments.length > 3 && arguments[3] !== undefined ? arguments[3] : {};
  var noColorOutput = arguments.length > 4 && arguments[4] !== undefined ? arguments[4] : false;

  if (files.length === 0) {
    throw new Error('\nNo files were specified for analysis in the arguments. Bailing...\n');
  }

  var _functionId$split = functionId.split('::', 2),
      _functionId$split2 = _slicedToArray(_functionId$split, 2),
      contractToTraverse = _functionId$split2[0],
      functionToTraverse = _functionId$split2[1];

  if (contractToTraverse === undefined || functionToTraverse === undefined) {
    throw new Error('\nYou did not provide the function identifier in the right format "CONTRACT::FUNCTION"\n');
  }

  if (accepted_visibility !== 'all' && accepted_visibility !== 'internal' && accepted_visibility !== 'external') {
    throw new Error('The "' + accepted_visibility + '" type of call to traverse is not known [all|internal|external]');
  }

  if (options.jsonOutput) {
    noColorOutput = true;
  }

  var functionCallsTree = {};

  // initialize vars that persist over file parsing loops
  var userDefinedStateVars = {};
  var stateVars = {};
  var dependencies = {};

  var functionsPerContract = {};
  var eventsPerContract = {};
  var structsPerContract = {};
  var contractUsingFor = {};
  var contractNames = [];

  var modifiers = {};
  var functionDecorators = {};

  var fileASTs = [];
  var contractASTIndex = {};

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

    fileASTs.push(ast);

    var contractName = null;

    parser.visit(ast, {
      ContractDefinition: function ContractDefinition(node) {
        contractName = node.name;
        contractNames.push(contractName);

        userDefinedStateVars[contractName] = {};
        stateVars[contractName] = {};
        functionsPerContract[contractName] = [];
        eventsPerContract[contractName] = [];
        structsPerContract[contractName] = [];
        contractUsingFor[contractName] = {};

        contractASTIndex[contractName] = fileASTs.length - 1;

        dependencies[contractName] = node.baseContracts.map(function (spec) {
          return spec.baseName.namePath;
        });
      },
      StateVariableDeclaration: function StateVariableDeclaration(node) {
        var _iteratorNormalCompletion9 = true;
        var _didIteratorError9 = false;
        var _iteratorError9 = undefined;

        try {
          for (var _iterator9 = node.variables[Symbol.iterator](), _step9; !(_iteratorNormalCompletion9 = (_step9 = _iterator9.next()).done); _iteratorNormalCompletion9 = true) {
            var variable = _step9.value;

            if (parserHelpers.isUserDefinedDeclaration(variable)) {
              userDefinedStateVars[contractName][variable.name] = variable.typeName.namePath;
            } else if (parserHelpers.isElementaryTypeDeclaration(variable)) {
              stateVars[contractName][variable.name] = variable.typeName.name;
            } else if (parserHelpers.isArrayDeclaration(variable)) {
              stateVars[contractName][variable.name] = variable.typeName.baseTypeName.namePath;
            } else if (parserHelpers.isMappingDeclaration(variable)) {
              stateVars[contractName][variable.name] = variable.typeName.valueType.name;
            }
          }
        } catch (err) {
          _didIteratorError9 = true;
          _iteratorError9 = err;
        } finally {
          try {
            if (!_iteratorNormalCompletion9 && _iterator9.return) {
              _iterator9.return();
            }
          } finally {
            if (_didIteratorError9) {
              throw _iteratorError9;
            }
          }
        }
      },
      FunctionDefinition: function FunctionDefinition(node) {
        functionsPerContract[contractName].push(node.name);
      },
      EventDefinition: function EventDefinition(node) {
        eventsPerContract[contractName].push(node.name);
      },
      StructDefinition: function StructDefinition(node) {
        structsPerContract[contractName].push(node.name);
      },
      UsingForDeclaration: function UsingForDeclaration(node) {
        // Check if the using for declaration is targeting a specific type or all types with "*"
        var typeNameName = node.typeName != null ? node.typeName.name : '*';

        if (!contractUsingFor[contractName][typeNameName]) {
          contractUsingFor[contractName][typeNameName] = new Set([]);
        }
        contractUsingFor[contractName][typeNameName].add(node.libraryName);
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

  dependencies = linearize(dependencies, { reverse: true });

  var _iteratorNormalCompletion2 = true;
  var _didIteratorError2 = false;
  var _iteratorError2 = undefined;

  try {
    for (var _iterator2 = fileASTs[Symbol.iterator](), _step2; !(_iteratorNormalCompletion2 = (_step2 = _iterator2.next()).done); _iteratorNormalCompletion2 = true) {
      var ast = _step2.value;

      constructPerFileFunctionCallTree(ast);
    }
    // END of file traversing
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

  var touched = {};
  var callTree = {};

  if (!functionCallsTree.hasOwnProperty(contractToTraverse)) {
    return 'The ' + contractToTraverse + ' contract is not present in the codebase.';
  } else if (!functionCallsTree[contractToTraverse].hasOwnProperty(functionToTraverse)) {
    return 'The ' + functionToTraverse + ' function is not present in ' + contractToTraverse + '.';
  }

  var seedKeyString = contractToTraverse + '::' + functionToTraverse;
  touched[seedKeyString] = true;
  callTree[seedKeyString] = {};

  // Call with seed
  constructCallTree(contractToTraverse, functionToTraverse, callTree[seedKeyString]);

  if (options.jsonOutput) {
    return callTree;
  } else {
    return treeify.asTree(callTree, true);
  }

  /****************************
   * 
   * INTERNAL FUNCTIONS BLOCK
   * 
   ****************************/

  function modifierCalls(modifierName, contractName) {
    if (dependencies.hasOwnProperty(contractName)) {
      var _iteratorNormalCompletion3 = true;
      var _didIteratorError3 = false;
      var _iteratorError3 = undefined;

      try {
        for (var _iterator3 = dependencies[contractName][Symbol.iterator](), _step3; !(_iteratorNormalCompletion3 = (_step3 = _iterator3.next()).done); _iteratorNormalCompletion3 = true) {
          var dep = _step3.value;

          if (!functionCallsTree.hasOwnProperty(dep)) {
            constructPerFileFunctionCallTree(fileASTs[contractASTIndex[dep]]);
          }

          if (!functionCallsTree.hasOwnProperty(dep)) {
            throw new Error('\nA referenced contract was not available in the provided list of contracts. This usually means that some imported file was left out of the files argument.\nYou can try to solve this automatically by using the \'-i\' flag or by including all the imported files manually.\n');
          }

          if (functionCallsTree[dep].hasOwnProperty(modifierName)) {
            return functionCallsTree[dep][modifierName];
          }
        }
      } catch (err) {
        _didIteratorError3 = true;
        _iteratorError3 = err;
      } finally {
        try {
          if (!_iteratorNormalCompletion3 && _iterator3.return) {
            _iterator3.return();
          }
        } finally {
          if (_didIteratorError3) {
            throw _iteratorError3;
          }
        }
      }
    }

    return functionCallsTree[contractName].hasOwnProperty(modifierName) ? functionCallsTree[contractName][modifierName] : {};
  }

  function constructPerFileFunctionCallTree(ast) {
    var contractName = null;
    var functionName = null;

    var userDefinedLocalVars = {};
    var localVars = {};
    var tempUserDefinedStateVars = {};
    var tempStateVars = {};

    parser.visit(ast, {
      ContractDefinition: function ContractDefinition(node) {
        contractName = node.name;

        functionCallsTree[contractName] = {};
        modifiers[contractName] = {};

        var _iteratorNormalCompletion4 = true;
        var _didIteratorError4 = false;
        var _iteratorError4 = undefined;

        try {
          for (var _iterator4 = dependencies[contractName][Symbol.iterator](), _step4; !(_iteratorNormalCompletion4 = (_step4 = _iterator4.next()).done); _iteratorNormalCompletion4 = true) {
            var dep = _step4.value;

            Object.assign(tempUserDefinedStateVars, userDefinedStateVars[dep]);
            Object.assign(tempStateVars, stateVars[dep]);
          }
        } catch (err) {
          _didIteratorError4 = true;
          _iteratorError4 = err;
        } finally {
          try {
            if (!_iteratorNormalCompletion4 && _iterator4.return) {
              _iterator4.return();
            }
          } finally {
            if (_didIteratorError4) {
              throw _iteratorError4;
            }
          }
        }

        Object.assign(tempUserDefinedStateVars, userDefinedStateVars[contractName]);
        Object.assign(tempStateVars, stateVars[contractName]);
      },


      'ContractDefinition:exit': function ContractDefinitionExit(node) {
        contractName = null;
        tempUserDefinedStateVars = {};
        tempStateVars = {};
      },

      FunctionDefinition: function FunctionDefinition(node) {
        if (node.isConstructor) {
          functionName = '<Constructor>';
        } else if (node.isFallback) {
          functionName = '<Fallback>';
        } else if (node.isReceiveEther) {
          functionName = '<Receive Ether>';
        } else {
          functionName = node.name;
        }

        var spec = '';
        if (node.visibility === 'public' || node.visibility === 'default') {
          spec += '[Pub] ❗️';
        } else if (node.visibility === 'external') {
          spec += '[Ext] ❗️';
        } else if (node.visibility === 'private') {
          spec += '[Priv] 🔐';
        } else if (node.visibility === 'internal') {
          spec += '[Int] 🔒';
        }

        var payable = '';
        if (node.stateMutability === 'payable') {
          payable = '💵';
        }

        var mutating = '';
        if (!node.stateMutability) {
          mutating = '🛑';
        }

        functionDecorators[functionName] = ' | ' + spec + '  ' + mutating + ' ' + payable;

        functionCallsTree[contractName][functionName] = {};
        modifiers[contractName][functionName] = [];
      },


      'FunctionDefinition:exit': function FunctionDefinitionExit(node) {
        functionName = null;
        userDefinedLocalVars = {};
        localVars = {};
      },

      ModifierDefinition: function ModifierDefinition(node) {
        functionName = node.name;
        functionCallsTree[contractName][functionName] = {};
      },


      'ModifierDefinition:exit': function ModifierDefinitionExit(node) {
        functionName = null;
      },

      ModifierInvocation: function ModifierInvocation(node) {
        modifiers[contractName][functionName].push(node.name);
      },
      ParameterList: function ParameterList(node) {
        var _iteratorNormalCompletion5 = true;
        var _didIteratorError5 = false;
        var _iteratorError5 = undefined;

        try {
          for (var _iterator5 = node.parameters[Symbol.iterator](), _step5; !(_iteratorNormalCompletion5 = (_step5 = _iterator5.next()).done); _iteratorNormalCompletion5 = true) {
            var parameter = _step5.value;

            if (parameter.name === null) {
              return;
            } else if (parserHelpers.isUserDefinedDeclaration(parameter)) {
              userDefinedLocalVars[parameter.name] = parameter.typeName.namePath;
            } else if (functionName) {
              localVars[parameter.name] = parameter.typeName.name;
            }
          }
        } catch (err) {
          _didIteratorError5 = true;
          _iteratorError5 = err;
        } finally {
          try {
            if (!_iteratorNormalCompletion5 && _iterator5.return) {
              _iterator5.return();
            }
          } finally {
            if (_didIteratorError5) {
              throw _iteratorError5;
            }
          }
        }
      },
      VariableDeclaration: function VariableDeclaration(node) {
        if (functionName && node.name === null) {
          return;
        } else if (functionName && parserHelpers.isUserDefinedDeclaration(node)) {
          userDefinedLocalVars[node.name] = node.typeName.namePath;
        } else if (functionName && parserHelpers.isElementaryTypeDeclaration(node)) {
          localVars[node.name] = node.typeName.name;
        } else if (functionName && parserHelpers.isArrayDeclaration(node)) {
          localVars[node.name] = node.typeName.baseTypeName.namePath;
        } else if (functionName && parserHelpers.isMappingDeclaration(node)) {
          localVars[node.name] = node.typeName.valueType.name;
        }
      },
      FunctionCall: function FunctionCall(node) {
        if (!functionName) {
          // this is a function call outside of functions and modifiers, ignore if exists
          return;
        }

        var expr = node.expression;

        var name = void 0;
        var localContractName = void 0;
        var visibility = void 0;

        // Construct an array with the event and struct names in the whole dependencies tree of the current contract
        var eventsOfDependencies = [];
        var structsOfDependencies = [];
        if (dependencies.hasOwnProperty(contractName)) {
          var _iteratorNormalCompletion6 = true;
          var _didIteratorError6 = false;
          var _iteratorError6 = undefined;

          try {
            for (var _iterator6 = dependencies[contractName][Symbol.iterator](), _step6; !(_iteratorNormalCompletion6 = (_step6 = _iterator6.next()).done); _iteratorNormalCompletion6 = true) {
              var dep = _step6.value;

              eventsOfDependencies = eventsOfDependencies.concat(eventsPerContract[dep]);
              structsOfDependencies = structsOfDependencies.concat(structsPerContract[dep]);
            }
          } catch (err) {
            _didIteratorError6 = true;
            _iteratorError6 = err;
          } finally {
            try {
              if (!_iteratorNormalCompletion6 && _iterator6.return) {
                _iterator6.return();
              }
            } finally {
              if (_didIteratorError6) {
                throw _iteratorError6;
              }
            }
          }
        }
        // The following block is a nested switch statement for creation of the call tree
        // START BLOCK
        if (parserHelpers.isRegularFunctionCall(node, contractNames, eventsOfDependencies, structsOfDependencies)) {
          name = expr.name;

          localContractName = contractName;

          // check if function is implemented in this contract or in any of its dependencies
          if (dependencies.hasOwnProperty(contractName)) {
            var _iteratorNormalCompletion7 = true;
            var _didIteratorError7 = false;
            var _iteratorError7 = undefined;

            try {
              for (var _iterator7 = dependencies[contractName][Symbol.iterator](), _step7; !(_iteratorNormalCompletion7 = (_step7 = _iterator7.next()).done); _iteratorNormalCompletion7 = true) {
                var _dep = _step7.value;

                if (!functionCallsTree.hasOwnProperty(_dep)) {
                  constructPerFileFunctionCallTree(fileASTs[contractASTIndex[_dep]]);
                }

                if (!functionCallsTree.hasOwnProperty(_dep)) {
                  throw new Error('\nA referenced contract was not available in the provided list of contracts. This usually means that some imported file was left out of the files argument.\nYou can try to solve this automatically by using the \'-i\' flag or by including all the imported files manually.\n');
                }

                if (functionCallsTree[_dep].hasOwnProperty(name)) {
                  localContractName = _dep;
                }
              }
            } catch (err) {
              _didIteratorError7 = true;
              _iteratorError7 = err;
            } finally {
              try {
                if (!_iteratorNormalCompletion7 && _iterator7.return) {
                  _iterator7.return();
                }
              } finally {
                if (_didIteratorError7) {
                  throw _iteratorError7;
                }
              }
            }
          }

          visibility = 'internal';
        } else if (parserHelpers.isMemberAccess(node)) {
          var object = null;
          var variableType = null;

          visibility = 'external';

          name = expr.memberName;

          // checking if the member expression is a simple identifier
          if (expr.expression.hasOwnProperty('name')) {
            object = expr.expression.name;

            // checking if it is a member of `address` and pass along it's contents
          } else if (parserHelpers.isMemberAccessOfAddress(node)) {
            if (expr.expression.arguments[0].hasOwnProperty('name')) {
              object = expr.expression.arguments[0].name;
            } else if (expr.expression.arguments[0].type === 'NumberLiteral') {
              object = 'address(' + expr.expression.arguments[0].number + ')';
            } else {
              object = JSON.stringify(expr.expression.arguments).replace(/"/g, "");
            }

            // checking if it is a typecasting to a user-defined contract type
          } else if (parserHelpers.isAContractTypecast(node, contractNames)) {
            object = expr.expression.expression.name;
          }

          // check if member expression is a special var and get its canonical type
          if (parserHelpers.isSpecialVariable(expr.expression)) {
            variableType = parserHelpers.getSpecialVariableType(expr.expression);

            // check if member expression is a typecast for a canonical type
          } else if (parserHelpers.isElementaryTypecast(expr.expression)) {
            variableType = expr.expression.expression.typeName.name;

            // else check for vars in defined the contract
          } else {
            // check if member access is a function of a "using for" declaration
            // START
            if (localVars.hasOwnProperty(object)) {
              variableType = localVars[object];
            } else if (userDefinedLocalVars.hasOwnProperty(object)) {
              variableType = userDefinedLocalVars[object];
            } else if (tempUserDefinedStateVars.hasOwnProperty(object)) {
              variableType = tempUserDefinedStateVars[object];
            } else if (tempStateVars.hasOwnProperty(object)) {
              variableType = tempStateVars[object];
            }
          }

          // convert to canonical elementary type: uint -> uint256
          variableType = variableType === 'uint' ? 'uint256' : variableType;

          // if variable type is not null let's replace "object" for the actual library name
          if (variableType !== null) {
            // Incase there is a "using for" declaration for this specific variable type we get its definition
            if (contractUsingFor[contractName].hasOwnProperty(variableType) && functionsPerContract.hasOwnProperty(contractUsingFor[contractName][variableType])) {

              // If there were any library declarations done to all the types with "*"
              // we will add them to the list of matching contracts
              var contractUsingForDefinitions = new (Function.prototype.bind.apply(Set, [null].concat(_toConsumableArray(contractUsingFor[contractName][variableType]))))();
              if (contractUsingFor[contractName].hasOwnProperty('*') && functionsPerContract.hasOwnProperty(contractUsingFor[contractName]['*'])) {
                contractUsingForDefinitions = new (Function.prototype.bind.apply(Set, [null].concat(_toConsumableArray(contractUsingFor[contractName][variableType]), _toConsumableArray(contractUsingFor[contractName]['*']))))();
              }

              // check which usingFor contract the method resolves to (best effort first match)
              var matchingContracts = [].concat(_toConsumableArray(contractUsingForDefinitions)).filter(function (contract) {
                return functionsPerContract[contract].includes(name);
              });

              if (matchingContracts.length > 0) {
                // we found at least one matching contract. use the first. don't know what to do if multiple are matching :/
                if (!options.libraries) {
                  object = matchingContracts[0];
                } else {
                  return;
                }
              }
            }
            // In case there is not, we can just shortcircuit the search to only the "*" variable type, incase it exists
          } else if (contractUsingFor[contractName].hasOwnProperty('*') && functionsPerContract.hasOwnProperty(contractUsingFor[contractName]['*'])) {
            // check which usingFor contract the method resolves to (best effort first match)
            var _matchingContracts = [].concat(_toConsumableArray(contractUsingFor[contractName]['*'])).filter(function (contract) {
              return functionsPerContract[contract].includes(name);
            });

            if (_matchingContracts.length > 0) {
              // we found at least one matching contract. use the first. don't know what to do if multiple are matching :/
              if (!options.libraries) {
                object = _matchingContracts[0];
              } else {
                return;
              }
            }
          }
          // END

          // if we have found nothing so far then create no node
          if (object === null) {
            return;
          } else if (object === 'super') {
            // "super" in this context is gonna be the 2nd element of the dependencies array
            // since the first is the contract itself
            localContractName = dependencies[contractName][1];
          } else if (tempUserDefinedStateVars[object] !== undefined) {
            localContractName = tempUserDefinedStateVars[object];
          } else if (userDefinedLocalVars[object] !== undefined) {
            localContractName = userDefinedLocalVars[object];
          } else {
            localContractName = object;
          }
        } else {
          return;
        }

        if (!functionCallsTree[contractName][functionName].hasOwnProperty(name)) {
          functionCallsTree[contractName][functionName][name] = {
            contract: localContractName,
            numberOfCalls: 1,
            visibility: visibility
          };
        } else {
          functionCallsTree[contractName][functionName][name].numberOfCalls += 1;
        }
      }
    });
  }

  // Function to recursively generate the tree to show in the console
  function constructCallTree(reduceJobContractName, reduceJobFunctionName, parentObject) {
    var tempIterable = void 0;

    if (functionCallsTree[reduceJobContractName] === undefined) {
      //unknown method. do not resolve further
      return;
    }

    if (functionCallsTree[reduceJobContractName][reduceJobFunctionName] === undefined) {
      return;
    }

    tempIterable = functionCallsTree[reduceJobContractName][reduceJobFunctionName];

    var _iteratorNormalCompletion8 = true;
    var _didIteratorError8 = false;
    var _iteratorError8 = undefined;

    try {
      for (var _iterator8 = modifiers[reduceJobContractName][reduceJobFunctionName][Symbol.iterator](), _step8; !(_iteratorNormalCompletion8 = (_step8 = _iterator8.next()).done); _iteratorNormalCompletion8 = true) {
        var modifier = _step8.value;

        Object.assign(tempIterable, modifierCalls(modifier, reduceJobContractName));
      }
    } catch (err) {
      _didIteratorError8 = true;
      _iteratorError8 = err;
    } finally {
      try {
        if (!_iteratorNormalCompletion8 && _iterator8.return) {
          _iterator8.return();
        }
      } finally {
        if (_didIteratorError8) {
          throw _iteratorError8;
        }
      }
    }

    Object.entries(tempIterable).forEach(function (_ref) {
      var _ref2 = _slicedToArray(_ref, 2),
          functionCallName = _ref2[0],
          functionCallObject = _ref2[1];

      if (functionCallName !== 'undefined' && (accepted_visibility == 'all' || functionCallObject.visibility == accepted_visibility)) {
        var keyString = functionCallObject.contract + '::' + functionCallName;

        keyString += functionDecorators[functionCallName] === undefined ? '' : functionDecorators[functionCallName];

        if (!noColorOutput && functionCallObject.visibility === 'external' && accepted_visibility !== 'external') {
          keyString = keyString.yellow;
        }

        if (touched[keyString] === undefined) {
          parentObject[keyString] = {};
          touched[keyString] = true;

          // Test if the call is really to a contract or rather an address variable member access
          // If it is not a contract we should stop here
          if (functionCallObject.contract.substring(0, 8) !== '#address') {
            constructCallTree(functionCallObject.contract, functionCallName, parentObject[keyString]);
          }
        } else {
          if (functionCallsTree[functionCallObject.contract] === undefined) {
            parentObject[keyString] = {};
          } else {
            parentObject[keyString] = Object.keys(functionCallsTree[functionCallObject.contract][functionCallName]).length === 0 ? {} : noColorOutput ? '..[Repeated Ref]..' : '..[Repeated Ref]..'.red;
          }
        }
      }
    });
  }
}