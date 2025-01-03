-- Source: https://raw.githubusercontent.com/dhall-lang/Prelude/35deff0d41f2bf86c42089c6ca16665537f54d75/List/map
{-
Transform a list by applying a function to each element

Examples:

```
./map Natural Bool Natural/even [ 2, 3, 5 ]
= [ True, False, False ]

./map Natural Bool Natural/even ([] : List Natural)
= [] : List Bool
```
-}
    let map
        : ∀(a : Type) → ∀(b : Type) → (a → b) → List a → List b
        =   λ(a : Type)
          → λ(b : Type)
          → λ(f : a → b)
          → λ(xs : List a)
          → List/build
            b
            (   λ(list : Type)
              → λ(cons : b → list → list)
              → List/fold a xs list (λ(x : a) → cons (f x))
            )

    let Map
        : Type → Type → Type
        = λ(k : Type) → λ(v : Type) → List { mapKey : k, mapValue : v }

    let Entry
        : Type → Type → Type
        = λ(k : Type) → λ(v : Type) → { mapKey : k, mapValue : v }

in  { Map, Entry, map }
