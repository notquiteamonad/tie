module Main_No_Type_Definition exposing (main)

import Browser
import Html exposing (Html, text)

main = Browser.element
          { init = \_ -> ()
          , view = \_ -> text "This is a test."
          , update = \_ _ -> ()
          }
