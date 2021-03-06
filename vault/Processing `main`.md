# Processing `main`

We need to process `main` to get the program's flags. This can be done from its type definition, which should be of type `Program flags model msg`, so we need to extract `flags`.

1. [[Parsing Elm type definitions|Parse the type definition of `main`]]
2. Get the equivalent TypeScript Type
  a. If the `flags` type is a primitive type: [[Converting Elm Primitive Types to TypeScript Types|Convert it from a primitive]]
  b. If it's a user-defined type: [[Converting User-Defined Types to TypeScript Types|Convert it from a user-defined type]]
3. Produce an output similar to the following based on this type:

```typescript
export function init(options: {
      node?: HTMLElement | null;
      flags: null;
}): Elm.Main.App;
```

This will probably rely on some [[TypeScript Function Type]] with a formatter. We'll also need a [[TypeScript Type Type]] and probably something like a `newtype` for [[TypeScript Identifier Type|TypeScript Identifiers]]
