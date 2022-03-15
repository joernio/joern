"use strict";

const fs = require('fs');
const path = require('path');
const parser = require('@solidity-parser/parser');


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
export function importProfiler(files, projectDir = process.cwd(), importedFiles = new Set()) {
  for (let file of files){
    // Checks for a valid solidity file
    file = path.resolve(projectDir, file);
    if (file.indexOf(projectDir) != 0) {
      throw new Error(`\nImports must be found in sub dirs of the project directory.
      project dir: ${projectDir}
      path: ${file}\n`);
    }
    let content;
    try {
      content = fs.readFileSync(file).toString('utf-8');
    } catch (e) {
      if (e.code === 'EISDIR') {
        console.error(`Skipping directory ${file}`);
        return importedFiles; // empty Set
      } else {
        throw e;
      }
    }
    // Having verified that it indeed is a solidity file, add it to set of importedFiles
    importedFiles.add(file);
    const ast = (() => {
      try {
        return parser.parse(content, {tolerant: true});
      } catch (err) {
        console.error(`\nError found while parsing the following file: ${file}\n`);
        throw err;
      }
    })();
    
    // create an array to hold the imported files
    const newFiles = [];
    parser.visit(ast, {
      ImportDirective(node) {
        let newFile = resolveImportPath(file, node.path, projectDir);
        if (!importedFiles.has(newFile)) newFiles.push(newFile);
      }
    });
    // Run through the array of files found in this file
    module.exports.importProfiler(newFiles, projectDir, importedFiles);
  }
    // Convert the set to an array for easy consumption
  const importedFilesArray = Array.from(importedFiles);
  return importedFilesArray;
}

/// Takes a filepath, and an import path found within it, and finds the corresponding source code
/// file. Throws an error if the resolved path is not a file.
///
/// @param      {string}  baseFilePath      The base file path
/// @param      {string}  importedFilePath  The imported file path
/// @param      {string}  projectDir        The top-most directory we will search in
///
export function resolveImportPath(baseFilePath, importedFilePath, projectDir = process.cwd()) {
  const topmostDirArray = projectDir.split(path.sep);
  let resolvedPath;
  let baseDirPath = path.dirname(baseFilePath);
  // if it's a relative or absolute path:
  if (
    importedFilePath.slice(0,1) === '.'
    || importedFilePath.slice(0,1) === '/'
  ) {
    resolvedPath = path.resolve(baseDirPath, importedFilePath);
  // else it's most likely a special case using a remapping to node_modules dir in Truffle
  } else {
    // we use a string and not the array alone because of different windows and UNIX path roots
    let currentDir = path.resolve(baseDirPath, '..');
    let currentDirArray = baseDirPath.split(path.sep);
    let currentDirName = currentDirArray.pop();
    let nodeModulesDir = '';

    // while (currentDirName != 'contracts') {
    while (!fs.readdirSync(currentDir).includes('node_modules') && !nodeModulesDir) {
      // since we already know the current file is inside the project dir we can check if the
      // folder array length for the current dir is smaller than the top-most one, i.e. we are 
      // still inside the project dir. If not, throw
      if (topmostDirArray.length >= currentDirArray.length) {
        throw new Error(`Import statement seems to be a Truffle "'node_modules' remapping" but no 'contracts' truffle dir could be found in the project's child dirs. Have you ran 'npm install', already?
        project dir: ${projectDir}
        path: ${currentDir}`);
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
  if (
    !fs.existsSync(resolvedPath) 
    || !fs.statSync(resolvedPath).isFile()
  ) {
    throw new Error(`Import path (${resolvedPath}) not resolved to a file`);
  }
  return resolvedPath;
}