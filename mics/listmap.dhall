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
in map
