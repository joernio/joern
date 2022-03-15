"use strict";

var utils = {
  insertBeforeLastOccurrence: function insertBeforeLastOccurrence(strToSearch, strToFind, strToInsert) {
    var n = strToSearch.lastIndexOf(strToFind);
    if (n < 0) {
      return strToSearch;
    }
    return strToSearch.substring(0, n) + strToInsert + strToSearch.substring(n);
  }
};

module.exports = utils;