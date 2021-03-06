# Converting User-Defined Types to TypeScript Types

## Records

Given the record:

```elm
type alias UserInfo =
  { name : String
  , age : Int
  }
```

A typescript interface could be

```ts
interface UserInfo {
  name: string; // Use the conversions from primitives or in this file
  age: number;
}
```

## Union Types

For now, don't support union types (fatal).
