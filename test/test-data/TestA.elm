port module TestA exposing (Foo)

type alias Foo =
  { username : String
  , email : String
  }

port port1 : (String -> msg) -> Sub msg

port port2 : (Maybe (List String) -> a) -> Sub a

port port3 : 
  (Foo 
  -> a
  ) 
  -> Sub a

port port4 : String -> Cmd msg

port port5 : Maybe (List String) -> Sub a

port port6 : 
  Foo
  -> Cmd
 a