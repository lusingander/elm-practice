module Example exposing (simpleHelloWorld)

import String



-- https://en.wikipedia.org/wiki/Brainfuck#Hello_World!


simpleHelloWorld : String
simpleHelloWorld =
    """
++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.
""" |> String.trim
