# Processing Ports

Ports have two forms: input and output.

Conversion of thr types may require [[Converting Elm Primitive Types to TypeScript Types]] or [[Converting User-Defined Types to TypeScript Types]].

## Input

```elm
port messageReceiver : (a -> msg) -> Sub msg
```

In this case, `a` is the interesting type - the following TypeScript should be produced:

```ts
messageReceiver: { send(data: a /* or TS's equivalent */): void }
```

## Output

```elm
port sendMessage : a -> Cmd msg
```

Again, `a` is the interesting type:

```ts
sendMessage: {
          subscribe(callback: (data: a /* or TS's equivalent */) => void): void
}
```
