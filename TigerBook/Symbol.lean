import Std.Data.HashMap
import Std.Data.TreeMap
import Batteries.Data.RBMap

open Std TreeMap

structure Symbol where
  name : String
  id : Int
deriving Repr, BEq, Hashable

instance : Ord Symbol where
  compare s₁ s₂ := compare s₁.2 s₂.2

structure InternState where
  next : Nat := 0
  dict : HashMap String Nat := {}

abbrev InternM := StateM InternState

def symbol (s : String) : InternM Symbol := do
  let st ← get
  match st.dict.get? s with
  | some v =>
      pure {name := s, id := v}
  | none =>
      let i := st.next
      let dict := HashMap.insert st.dict s i
      set {st with next := i + 1, dict}
      pure {name := s, id := i}

def name (s : Symbol) := s.1

abbrev Env (α : Type) := TreeMap Symbol α Ord.compare

namespace Env

def empty {α} : Env α := ∅
def add {α} (s : Symbol) (a : α) (t : Env α) : Env α := TreeMap.insert t s a
def look {α} (s : Symbol) (t : Env α) : Option α := TreeMap.get? t s

end Env
