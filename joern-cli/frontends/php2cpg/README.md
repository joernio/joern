# php2cpg

A PHP to CPG converter based on `php-parse`.

## Requirements

* PHP (>=7.0)

## Installation

```
sudo apt install php
```

# How it works

1. `php-parse --json-dump --with-recovery $file` is invoked on each
source file to obtain a corresponding abstract syntax tree in JSON
format.

2. JSON ASTs are parsed and converted into code property graphs.

