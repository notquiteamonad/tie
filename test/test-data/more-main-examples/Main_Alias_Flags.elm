module Main_Alias_Flags exposing (main)

import Browser
import Html exposing (Html, text)

type alias S = String

type alias SOverTwoLines =
  String

main : Program S () ()
main = Browser.element
          { init = \_ -> ()
          , view = \_ -> text "This is a test."
          , update = \_ _ -> ()
          }
