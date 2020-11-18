# Reading Group for the PureScript by Example book

latest version of the book, community edition: https://book.purescript.org/index.html

## How to Use With Nix

1. Install nix with `curl -L https://nixos.org/nix/install | sh`
2. Run `nix-shell` in the root directory.
3. Wait until it has built all dependencies. See the `shell.nix` file where all the dependencies are listed.
4. That's it. You are now in a new isolated shell and should be able to run `spago test  --main Test.Main` and see all tests passing.

## How to Add the Purescript Language Server in VSCode

1. Install the [purescript-ide plugin](https://marketplace.visualstudio.com/items?itemName=nwolverson.ide-purescript) for language server support
2. Add the following settings to your settings.json:
```json
{
  "purescript.addSpagoSources": true,
  "purescript.addNpmPath": true,  
  "purescript.buildCommand": "spago build --purs-args --json-errors"
}
```
3. You now have syntax highlighting and auto completion for purescript. If you open a `.purs` file the language server will automatically build it using spago and report back any errors.


## Build Instructions

Built with purs version 0.13.6

[Spago docs](https://github.com/purescript/spago/blob/master/README.md)

Notable commands (`spago <cmd> --help`)

        spago repl  --path src/path/to/file
        spago build --watch --clear-screen
        spago run   --path src/path/to/file
        spago test  --main Test.Main
        spago bundle-app --to index.js --path src/entry/point
