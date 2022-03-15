"use strict";

const BUILTINS = [
  'gasleft', 'require', 'assert', 'revert', 'addmod', 'mulmod', 'keccak256',
  'sha256', 'sha3', 'ripemd160', 'ecrecover',
];

function isLowerCase(str) {
  return str === str.toLowerCase();
}

const parserHelpers = {
  isRegularFunctionCall: (node, contractNames, eventNames, structNames) => {
    const expr = node.expression;
    return expr && expr.type === 'Identifier'
        && !contractNames.includes(expr.name)
        && !eventNames.includes(expr.name)
        && !structNames.includes(expr.name)
        && !BUILTINS.includes(expr.name);
  },

  isMemberAccess: node => {
    const expr = node.expression;
    return expr.type === 'MemberAccess' && !['push', 'pop', 'encode', 'encodePacked', 'encodeWithSelector', 'encodeWithSignature', 'decode'].includes(expr.memberName);
  },

  isIndexAccess: node => {
    const expr = node.expression;
    return expr.type === 'IndexAccess';
  },

  isMemberAccessOfAddress: node => {
    const expr = node.expression.expression;
    return expr.type === 'FunctionCall'
        && expr.expression.hasOwnProperty('typeName')
        && expr.expression.typeName.name === 'address';
  },

  isAContractTypecast: (node, contractNames) => {
    const expr = node.expression.expression;
    // @TODO: replace lowercase for better filtering
    return expr.type === 'FunctionCall'
        && expr.expression.hasOwnProperty('name')
        && contractNames.includes(expr.expression.name[0]);
  },

  isUserDefinedDeclaration: node => {
    return node.hasOwnProperty('typeName')&& node.typeName.type === 'UserDefinedTypeName';
  },

  isElementaryTypeDeclaration: node => {
    return node.hasOwnProperty('typeName')&& node.typeName.type === 'ElementaryTypeName';
  },

  isArrayDeclaration: node => {
    return node.hasOwnProperty('typeName')&& node.typeName.type === 'ArrayTypeName';
  },

  isMappingDeclaration: node => {
    return node.hasOwnProperty('typeName')&& node.typeName.type === 'Mapping';
  },

  isAddressDeclaration: node => {
    return node.hasOwnProperty('typeName')
        && node.typeName.type === 'ElementaryTypeName'
        && node.typeName.name === 'address';
  },

  isElementaryTypecast: node => {
    return node.hasOwnProperty('type')
        && node.type === 'FunctionCall'
        && node.hasOwnProperty('expression')
        && node.expression.type === 'TypeNameExpression'
        && node.expression.typeName.type === 'ElementaryTypeName';
  },

  isSpecialVariable: node => {
    // now (same as block.timestamp)
    if(
      node.hasOwnProperty('type')
      && node.type === 'Identifier'
      && node.name === 'now'
    ) {
      return true;
    // any block.<x> special variable
    } else if(
      node.hasOwnProperty('type')
      && node.type === 'MemberAccess'
      && node.hasOwnProperty('expression')
      && node.expression.type === 'Identifier'
      && node.expression.name === 'block'
    ) {
      return true;
    // any msg.<x> special variable
    } else if(
      node.hasOwnProperty('type')
      && node.type === 'MemberAccess'
      && node.hasOwnProperty('expression')
      && node.expression.type === 'Identifier'
      && node.expression.name === 'msg'
    ) {
      return true;
    // any tx.<x> special variable
    } else if(
      node.hasOwnProperty('type')
      && node.type === 'MemberAccess'
      && node.hasOwnProperty('expression')
      && node.expression.type === 'Identifier'
      && node.expression.name === 'tx'
    ) {
      return true;
    // if not then... return false
    } else {
      return false;
    }
  },

  getSpecialVariableType: node => {
    // now (same as block.timestamp)
    if(
      node.hasOwnProperty('type')
      && node.type === 'Identifier'
      && node.name === 'now'
    ) {
      return 'uint256';

    } else if(
    node.hasOwnProperty('type')
    && node.type === 'MemberAccess'
    && node.hasOwnProperty('expression')
    && node.expression.hasOwnProperty('type')
    && node.expression.type === 'Identifier'
    ) {
      // in case it is block.<x> special variable
      if(node.expression.name === 'block') {
        if(node.memberName === 'coinbase') {
          return 'address';
        } else {
          return 'uint256';
        }
      }
      
      // or msg.<x> special variable
      else if(node.expression.name === 'msg') {
        if(node.memberName === 'data') {
          return 'bytes';
        } else if(node.memberName === 'sender') {
          return 'address';
        } else if(node.memberName === 'sig') {
          return 'bytes4';
        } else if(node.memberName === 'value') {
          return 'uint256';
        }
      }

      // or tx.<x> special variable
      else if(node.expression.name === 'tx') {
        if(node.memberName === 'origin') {
          return 'address';
        } else if(node.memberName === 'gasprice') {
          return 'uint256';
        }
      }
    } else {
      // if not a special variable, return false
      return null;
    }
  },
};

module.exports = parserHelpers;