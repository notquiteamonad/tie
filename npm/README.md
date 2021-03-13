# TIE - TypeScript Interoperator for Elm

TIE is a tool for generating TypeScript definitions for elm projects such that
complex data structures can be passed through flags and ports safely.

**Note on Support:** Currently, TIE only supports Linux. If you're able to help add
support for Windows or Mac, please check out #25 and #27.

## Motivation

I had been using dillonkearns/elm-typescript-interop for this job before, but
since this has been deprecated in favour of its successor [elm-ts-interop](https://elm-ts-interop.com)
and since I'm doing this for hobby projects I don't have the money to spend on it.

If you want a more full-featured, premium product, I wholeheartedly recommend
checking out [elm-ts-interop](https://elm-ts-interop.com) and supporting that project.

Rather than taking on the original elm-typescript-interop, I wrote TIE from scratch.

## Features

Generates a TypeScript definition file for your Elm types, which includes:

* The flags specified in your `main` type definition as a parameter to `init`
* The ports specified in your application along with their `send` or `subscribe` functions

## Installation

### Method 1: Download the binary

A static binary for TIE is available on the [releases page](https://github.com/notquiteamonad/tie/releases).

### Method 2: Install via npm

This is convenient for using TIE as a project dependency, and essentially just runs
the static binary. You can install it via the following command:

```sh
npm i --save-dev elm-tie
# or
yarn add -D elm-tie
```

And run it using `npx -p elm-tie tie` or `yarn tie`.

**Note:** If using Yarn 2, you'll currently need to unplug the elm-tie dependency
via `yarn unplug elm-tie`.

## Usage

Make sure your elm's definition of `main` has an explicit type definition.

To run TIE, simply invoke it with the path to your elm source directory.
This should probably match the directory listed in your elm.json.

```sh
tie ~/projects/my-elm-project/src
```

You can pass the `--watch` or `-w` flag to make TIE watch for changes to elm
files within that directory and regenerate on each change.

Then, use it in your TypeScript code like so:

```typescript
// This should be the path to the directory in which the generated
// type definitions sit:
import { Elm } from "../elm/Main";

// Use the Elm namespace as normal, for example:

const app = Elm.Main.init({
  node: document.getElementById("elmApp"),
  flags: "data of whatever type my app requests as flags"
});

app.ports.receiveMessage.subscribe((data: number) => {
  // do something
});
```

## Support for Types

The table below outlines TIE's support for different Elm types:

|       Elm Type      | Generated TypeScript Type |                                    Notes                                   |
|:-------------------:|:-------------------------:|:--------------------------------------------------------------------------:|
| `Bool`              | `boolean`                 |                                                                            |
| `Int`               | `number`                  | **Passing a float to this will typecheck in TS but cause an error in Elm** |
| `Float`             | `number`                  |                                                                            |
| `String`            | `string`                  |                                                                            |
| `Maybe`             | `x \| void \| null`       | where `x` is the type which may be present                                 |
| `List`              | `x[]`                     | where `x` is the type of which there may be many                           |
| `Array`             | `x[]`                     | where `x` is the type of which there may be many                           |
| tuples              | not currently supported   | Support is planned for a future release (issue #17)                        |
| records             | `interface x {...}`       | where `x` is the name of the type and `...` are its constituent types      |
| `Json.Decode.Value` | `unknown`                 |                                                                            |
| `Json.Encode.Value` | `unknown`                 |                                                                            |
| `Value`             | `unknown`                 | Assumed to be imported from Json.*                                         |

## Limitations

* TIE does not support tuple types (yet) and behaviour for them may be unexpected (#17)
* TypeScript makes no distinction between integers and floats, which could lead to decoding
  errors in Elm for floats passed when integers were expected.

## Contributing

Please feel free to contribute to this project.

A pre-commit hook can be enabled to make the same checks as CI before pushing.
To enable it, run `./scripts/install_hooks.sh`.

More information is available in [CONTRIBUTING.md](https://github.com/notquiteamonad/tie/blob/main/CONTRIBUTING.md).
