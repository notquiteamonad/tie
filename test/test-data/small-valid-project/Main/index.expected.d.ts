export namespace Elm {
  namespace Main {
    export interface App {
      ports: {
        setSignedInUser: {
          send(data: Elm.Main.User | void | null): void;
        };
      };
    }
    export function init(options: { node?: HTMLElement | null; flags: Elm.Main.User | void | null }): Elm.Main.App;
    export type S = string;
    export interface User {
      userName: Elm.Main.S;
      emailAddress: Elm.Main.S | void | null;
    }
  }
}
