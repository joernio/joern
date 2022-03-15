<img align="right" height="200" src="https://user-images.githubusercontent.com/4008213/39437435-bed48266-4c98-11e8-834d-1de152667231.jpg">

<p align="left">

  [<img width="200" alt="get in touch with Consensys Diligence" src="https://user-images.githubusercontent.com/2865694/56826101-91dcf380-685b-11e9-937c-af49c2510aa0.png">](https://diligence.consensys.net)<br/>
  <sup>
  [[  🌐  ](https://diligence.consensys.net?utm_source=github_npm&utm_medium=banner&utm_campaign=surya)  [  📩  ](mailto:diligence@consensys.net)  [  🔥  ](https://consensys.github.io/diligence/)]
  </sup>
</p>
<br/><br/>

[![npm](https://img.shields.io/npm/v/surya.svg)](https://www.npmjs.com/package/surya)
[![Build Status](https://travis-ci.com/ConsenSys/surya.svg?branch=master)](https://travis-ci.com/ConsenSys/surya)

# Sūrya, The Sun God: A Solidity Inspector 

#### 

Surya is an utility tool for smart contract systems. It provides a number of visual outputs and information about the contracts' structure. Also supports querying the function call graph in multiple ways to aid in the manual inspection of contracts.

Currently only supports Solidity but we hope to extend the tool to encompass other languages.

The name stems from the sun deity [Surya](https://en.wikipedia.org/wiki/Surya)

Why the sun, you ask? Because "sun" in latin and portuguese is [*Sol*](https://en.wikipedia.org/wiki/Solar_deity).


## Getting Started

Install it via NPM:

```shell
npm install -g surya
```

**NOTE:** In order to view the graph output, you need to have `graphviz` installed, so that you can run the `dot` command.

Currently, however, the easiest way to use Surya in your project might be through [VSCode's Solidity Auditor extension](https://github.com/tintinweb/vscode-solidity-auditor) created by [@tintinweb](https://github.com/tintinweb)

![vscode-solidity-auditor-interactive-graph](https://user-images.githubusercontent.com/2865694/57710279-e27e8a00-766c-11e9-9ca9-8cde50aa31fc.gif)

## Command List

Surya takes in a `--no-color` flag with any command that disables the colors in the output making it effectively plain text.

All the commands that take in an array of files also take in a flag (`-i`/`--import`) that resolves file imports automatically.
Please be aware that in the case you use Truffle's "node_modules" remapping import statements, Surya searches up the project directory recursively until it finds a `contracts` directory in the Truffle project *up until the directory you ran the command in*.
This is so that we try to prevent any kind of path traversal vulnerabilities that could come from exposing Surya as a service.

All the commands that take in an array of files also take in a flag (`-c`/`--content`) that allows you to pass the actual source code contents as an argument instead of a file path (mostly useful when Surya is being used as another package's dependency).

The `-i` and `-c` flags are mutually exclusive.

### graph

The `graph` command outputs a DOT-formatted graph of the control flow.

```shell
surya graph contracts/**/*.sol | dot -Tpng > MyContract.png
```

<img src="https://user-images.githubusercontent.com/4008213/39415345-fbac4e3a-4c39-11e8-8260-0d9670c352d6.png" height="236">

There is new flag (`-s`/`--simple`) that amkes the command chart only the *contract* call graph, instead of the function call graph. It's super useful for higher-level analyses!

**Accepted flags**

* `-i`/`--import` - Resolve all imports automatically by fetching the right files.
* `-c`/`--content` - Allow passing in file contents as arguments instead of file paths.
* `-s`/`--simple` -  Only show calls between contracts, without specifying the functions.
* `-m`/`--modifiers` - **Enable** printing edges from functions to modifiers (when the latter are invoked in the function definitions).
* `-l`/`--libraries` - **Disable** printing edges from functions to libraries when the "Using ... for" syntax is in use (to prevent libraries like SafeMath from polluting the call graph).

### ftrace

The `ftrace` command outputs a _treefied_ function call trace stemming from the defined "CONTRACT::FUNCTION" and traversing "all|internal|external" types of calls.
External calls are marked in `orange` and internal calls are `uncolored`.

```shell
surya ftrace APMRegistry::_newRepo all MyContract.sol
```

<img src="https://user-images.githubusercontent.com/4008213/42409007-61473d12-81f1-11e8-8fee-1867cfd66822.png" height="236">

**Accepted flags**

* `-i`/`--import` - Resolve all imports automatically by fetching the right files.
* `-c`/`--content` - Allow passing in file contents as arguments instead of file paths.
* `-j`/`--json` - Return a JSON object instead of a _treefied_ function call trace (mostly useful when Surya is being used as another package's dependency).

### flatten

The `flatten` command outputs a flattened version of the source code, with all import statements replaced by the corresponding source code. Import statements that reference a file that has already been imported, will simply be commented out. 

```shell
surya flatten MyContract.sol
```

### describe

The `describe` command shows a summary of the contracts and methods in the files provided.

```shell
surya describe *.sol
```

<img src="https://user-images.githubusercontent.com/4008213/48572168-97bfc780-e900-11e8-9e86-d265498de936.png" width="336" height="236">

Functions will be listed as:

* `[Pub]` public
* `[Ext]` external
* `[Prv]` private
* `[Int]` internal

A yellow `($)`denotes a function is `payable`.

A red `#` indicates that it's able to modify state.

**Accepted flags**

* `-i`/`--import` - Resolve all imports automatically by fetching the right files.
* `-c`/`--content` - Allow passing in file contents as arguments instead of file paths.

### inheritance

The `inheritance` command outputs a DOT-formatted graph of the inheritance tree. For Windows machines, the `>` should be replaced with `-o`.

```shell
surya inheritance MyContract.sol | dot -Tpng > MyContract.png
```

<img src="https://user-images.githubusercontent.com/23033765/39249140-f50d2828-486b-11e8-81b8-8c4ffb7b1b54.png" height="236">

**Accepted flags**

* `-i`/`--import` - Resolve all imports automatically by fetching the right files.
* `-c`/`--content` - Allow passing in file contents as arguments instead of file paths.

### dependencies

The `dependencies` command outputs the [c3-linearization](https://en.wikipedia.org/wiki/C3_linearization) of a given contract's inheritance graph. Contracts will be listed starting with most-derived, ie. if the same function is defined in more than one contract, the solidity compiler will use the definition in whichever contract is listed first. 

```shell
surya dependencies Exchange Exchange.sol
```

**Accepted flags**

* `-i`/`--import` - Resolve all imports automatically by fetching the right files.
* `-c`/`--content` - Allow passing in file contents as arguments instead of file paths.

### parse

The `parse` command outputs a _treefied_ AST object coming from the parser.

**Accepted flags**

* `-j`/`--json` - Return a JSON object instead of a _treefied_ object.

```shell
surya parse MyContract.sol
```

<img src="https://user-images.githubusercontent.com/4008213/39415303-87df40de-4c39-11e8-8e03-ead72e88f1e3.png" height="236">


### mdreport

The `mdreport` command creates a Markdown description report with tables comprising information about the system's files, contracts and their functions. Much like describe but outputting to a nicely formatted Markdown file.

```shell
surya mdreport report_outfile.md MyContract.sol
```



## License

GPL-3.0

## Kudos

Created by @federicobond extended by @GNSPS
