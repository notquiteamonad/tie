# Converting Elm Primitive Types to TypeScript Types

| Elm                 | [[TypeScript Types|TypeScript Type]] |
|---------------------|--------------------------------------|
| `String`            | `string`                             |
| `Int | Float`       | `number`                             |
| `Bool`              | `boolean`                            |
| `Json.Decode.Value` | `unknown`                            |
| `Json.Encode.Value` | `unknown`                            |
| `()`                | `null`                               |
| `Maybe a`           | `a | null`                           |
