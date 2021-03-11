port module BadPort3 exposing ()

port aGoodPort : (String -> msg) -> Sub msg

port