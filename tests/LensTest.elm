module LensTest exposing (suite)

import Expect
import Fuzz
import Lens as Lens exposing (Lens)
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Lens"
        [ Test.fuzz Fuzz.int "get" <|
            \n ->
                { a = n }
                    |> aLens.get
                    |> Expect.equal n
        , Test.fuzz2 Fuzz.int Fuzz.int "set" <|
            \from to ->
                { a = from }
                    |> aLens.set to
                    |> Expect.equal { a = to }
        , Test.fuzz Fuzz.int "update" <|
            \n ->
                { a = n }
                    |> Lens.update aLens ((+) 100)
                    |> Expect.equal { a = n + 100 }
        , Test.describe "Composition"
            [ let
                abLens =
                    aLens |> Lens.andCompose bLens
              in
              Test.describe "Nested records"
                [ Test.fuzz Fuzz.int "get" <|
                    \n ->
                        { a = { b = n } }
                            |> abLens.get
                            |> Expect.equal n
                , Test.fuzz2 Fuzz.int Fuzz.int "set" <|
                    \from to ->
                        { a = { b = from } }
                            |> abLens.set to
                            |> Expect.equal { a = { b = to } }
                , Test.fuzz Fuzz.int "update" <|
                    \n ->
                        { a = { b = n } }
                            |> Lens.update abLens ((+) 100)
                            |> Expect.equal { a = { b = n + 100 } }
                ]
            , let
                boxedALens =
                    boxedLens |> Lens.andCompose aLens
              in
              Test.describe "Boxed record"
                [ Test.fuzz Fuzz.int "get" <|
                    \n ->
                        Boxed { a = n }
                            |> boxedALens.get
                            |> Expect.equal n
                , Test.fuzz2 Fuzz.int Fuzz.int "set" <|
                    \from to ->
                        Boxed { a = from }
                            |> boxedALens.set to
                            |> Expect.equal (Boxed { a = to })
                , Test.fuzz Fuzz.int "update" <|
                    \n ->
                        Boxed { a = n }
                            |> Lens.update boxedALens ((+) 100)
                            |> Expect.equal (Boxed { a = n + 100 })
                ]
            ]
        , Test.describe "Computed state"
            [ Test.fuzz fuzzTodoApp "get" <|
                \app ->
                    app
                        |> allCompletedLens.get
                        |> Expect.equal (List.all .completed app.todos)
            , Test.fuzz2 fuzzTodoApp Fuzz.bool "set" <|
                \app b ->
                    app
                        |> allCompletedLens.set b
                        |> .todos
                        |> List.all (\todo -> todo.completed == b)
                        |> Expect.equal True
            ]
        ]


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


type Boxed
    = Boxed { a : Int }


boxedLens : Lens Boxed { a : Int }
boxedLens =
    { get = \(Boxed x) -> x
    , set = \x (Boxed _) -> Boxed x
    }



-- Todolist example


type alias TodoItem =
    { text : String
    , completed : Bool
    }


type alias TodoApp =
    { todos : List TodoItem
    }


todosLens : Lens { a | todos : b } b
todosLens =
    Lens .todos (\x r -> { r | todos = x })


allCompletedLens : Lens TodoApp Bool
allCompletedLens =
    todosLens
        |> Lens.andCompose
            { get = List.all .completed
            , set = \newValue -> List.map (\todo -> { todo | completed = newValue })
            }


fuzzTodoItem : Fuzz.Fuzzer TodoItem
fuzzTodoItem =
    Fuzz.map2 TodoItem
        Fuzz.string
        Fuzz.bool


fuzzTodoApp : Fuzz.Fuzzer TodoApp
fuzzTodoApp =
    Fuzz.map TodoApp
        (Fuzz.list fuzzTodoItem)
