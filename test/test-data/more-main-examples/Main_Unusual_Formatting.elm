module Main_Unusual_Formatting exposing (main)

import Browser
import Html exposing (Html, text)

main : Program
  (Maybe
Int)
 () ()
main = Browser.element
          { init = \_ -> ()
          , view = \_ -> text "This is a test. 愛"
          , update = \_ _ -> ()
          }
