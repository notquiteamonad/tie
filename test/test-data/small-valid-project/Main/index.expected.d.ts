// This file was generated automatically by TIE (TypeScript Interoperator for Elm).

export namespace Elm {
  namespace Main {
    export interface App {
      ports: {
        setSignedInUser: {
          send(data: Elm.Main.User | null): void;
        };
      };
    }
    export function init(options: { node?: HTMLElement | null; flags: Elm.Main.User | null }): Elm.Main.App;
    export type S = string;
    export interface User {
      userName: Elm.Main.S;
      emailAddress: Elm.Main.S | null;
    }
  }
}
