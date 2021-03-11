port module BadPort1 exposing ()

port aGoodPort : (String -> msg) -> Sub msg

port badPort1 : (( -> msg) -> Sub msg
