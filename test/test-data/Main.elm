module Main exposing (main)

import Browser
import Html exposing (Html, text)

main : Program String () ()
main = Browser.element
          { init = \_ -> ()
          , view = \_ -> text "This is a test. 愛"
          , update = \_ _ -> ()
          }
