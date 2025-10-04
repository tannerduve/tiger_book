

import Std.Data.HashMap
-- import Cslib.Foundations.Control.Monad.Free.Effects

open Std

structure Symbol where
  name : String
  id : Int
deriving Repr, BEq

instance : Ord Symbol where
  compare s₁ s₂ := compare s₁.2 s₂.2

instance : Hashable Symbol where
  hash s := hash s.id
