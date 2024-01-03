module ExampleTest exposing (..)

import Expect
import Lens exposing (Lens)
import Test exposing (Test)


aLens : Lens { r | a : x } x
aLens =
    { get = .a
    , set = \x r -> { r | a = x }
    }


bLens : Lens { r | b : x } x
bLens =
    { get = .b
    , set = \x r -> { r | b = x }
    }


type Boxed a
    = Boxed a


boxedLens : Lens (Boxed a) a
boxedLens =
    { get = \(Boxed x) -> x
    , set = \x (Boxed _) -> Boxed x
    }


type alias NestedObject =
    Boxed { a : { b : Int } }


nestedObjectLens : Lens NestedObject Int
nestedObjectLens =
    boxedLens
        |> Lens.andCompose aLens
        |> Lens.andCompose bLens



-- Here's the magic


suite : Test
suite =
    Test.test "lens should be composable" <|
        \() ->
            Boxed { a = { b = 100 } }
                |> Lens.update nestedObjectLens (\x -> x + 1)
                |> Expect.equal (Boxed { a = { b = 101 } })
