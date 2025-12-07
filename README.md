# ymlint

A minimal YAML validator written in pure Haskell.

## Features

- Validates a subset of YAML syntax
- Reports errors with line numbers
- No external dependencies (only base libraries)

### Supported Syntax

- Key-value pairs (`key: value`)
- Lists (`- item`)
- Comments (`# comment`)
- Multi-line text (`|` and `>`)
- Nested structures via indentation

## Build
```bash
cabal build
```

## Usage
```bash
cabal run ymlint -- <filepath>
```

### Examples
```bash
$ cabal run ymlint -- examples/test.yaml
This is a valid YAML file.

$ cabal run ymlint -- examples/bad.yaml
Line 2: Invalid syntax: bad line here
```

## Install
```bash
cabal install
ymlint <filepath>
```

## License

BSD-3-Clause

