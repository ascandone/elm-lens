## `elm-lenses`

Lenses are an abstractions used in functional programming to compose getters and setters.

#### Example
```elm
-- Define lens specifying getter and setter

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

-- Compose lens
nestedObjectLens : Lens NestedObject Int
nestedObjectLens =
  boxedLens
    |> Lens.andCompose aLens
    |> Lens.andCompose bLens


-- Update (or set) nested fields using lens

suite : Test
suite =
  Test.test "lens should be composable" <|
    \() ->
      Boxed { a = { b = 100 }}
      |> Lens.update nestedObjectLens (\x -> x + 1)
      |> Expect.equal (Boxed { a = { b = 101 }})
```
