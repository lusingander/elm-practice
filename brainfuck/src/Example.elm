module Example exposing (examples, getExampleString)

import String


examples : List ( String, String )
examples =
    [ ( "", "-" )
    , ( "simple", "Simple Hello World" )
    ]


getExampleString : String -> String
getExampleString value =
    case value of
        "simple" ->
            simpleHelloWorld

        _ ->
            ""



-- https://en.wikipedia.org/wiki/Brainfuck#Hello_World!


simpleHelloWorld : String
simpleHelloWorld =
    """
++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.
""" |> String.trim
