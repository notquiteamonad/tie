# Configuring TIE

TIE can be configured by inserting a file called `.TIE.toml` into the root folder of your project.

It should conform to the [TOML 0.5.0 spec](https://toml.io/en/v0.5.0).

## Overrides

You can configure TIE to use custom types instead of what it would generate by default by using override tables.

To skip straight to how to do this, go to the [Configuring Overrides section](#configuring-overrides).

### Motivation for Overrides

Say you have the following Elm code:

```elm
type alias StatusMessage =
  { message : String
  , variant : SMVariant
  }

type SMVariant = Success | Warning | Error

type alias StatusMessagePort =
  { message: String
  , variant: String
  }

statusMessagePortToStatusMessage : StatusMessagePort -> StatusMessage
statusMessagePortToStatusMessage {message, variant} = StatusMessage message <|
  case port of
    "success" -> Success
    "warning" -> Warning
    _ -> Error
-- of course, you could go further and add a Result to this      

port statusMessage (StatusMessagePort -> msg) -> Sub msg

type Msg =
  ...
  SetStatusMessage StatusMessage

subscriptions = [statusMessage (statusMessagePortToStatusMessage >> SetStatusMessage)]
```

From this, TIE would generate:

```typescript
// inside Elm.Main
interface StatusMessagePort {
  message: string;
  variant: string;
}
```

But if you override the variant property, TIE could instead give you the following:

```typescript
interface StatusMessagePort {
  message: string;
  variant: "success" | "warning" | "error";
}
```

Then you maintain the safety of using an Elm decoder as long as the TS definition is correct

### Configuring Overrides

Overrides can be configured using override tables in your configuration file, which look like this:

```toml
# You can override the properties of record types like this:
[[override]]
property = "StatusMessagePort.variant"
type = '"success" | "warning" | "error"'

# You can override type aliases like this
[[override]]
property = "MySmallNumber"
type = "1 | 2"
```

#### Rules for Override Blocks

* You can add as many override blocks as you need
* Every override block **must** have both a `property` value and a `type` value
* The `property` value **must** be either the name of a present type alias or the name of a record type and its property, separated by a dot (.)
* The `type` value is a TypeScript type value which will be inserted directly into the generated .d.ts file in place of the automatically-generated type
