module Lens exposing
    ( Lens
    , andCompose
    , update
    )


type alias Lens a b =
    { get : a -> b
    , set : b -> a -> a
    }


update : Lens a b -> (b -> b) -> a -> a
update { get, set } mapper r =
    set (mapper (get r)) r


andCompose : Lens b c -> Lens a b -> Lens a c
andCompose nestedLens lens =
    { get = lens.get >> nestedLens.get
    , set = nestedLens.set >> update lens
    }
