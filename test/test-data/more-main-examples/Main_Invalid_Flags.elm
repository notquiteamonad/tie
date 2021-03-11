module Main_Invalid_Flags exposing (main)

import Browser
import Html exposing (Html, text)

main : Program ( () ()
main = Browser.element
          { init = \_ -> ()
          , view = \_ -> text "This is a test."
          , update = \_ _ -> ()
          }
