port module Main exposing (main)

import Browser
import Html exposing (Html, text)

type alias S = String

type alias User =
  { userName : S
  , emailAddress : Maybe S
  }

type alias Model = 
  { signedInUser : Maybe User
  }

type Msg = SetSignedInUser (Maybe User)

-- Strictly speaking this wouldn't work as we can't
-- subscribe to it, but TIE isn't (and doesn't need to be)
-- aware of this.
port setSignedInUser : (Maybe User -> msg) -> Sub msg

main : Program (Maybe User) Model Msg
main = Browser.element
          { init = \user -> {signedInUser = user}
          , view = \_ -> text "This is a test. 愛"
          , update = \_ _ -> ()
          }
