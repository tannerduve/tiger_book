import TigerBook.Symbol

open Symbol

structure Unique where
  id : Nat
deriving BEq, Hashable, Ord, Repr

abbrev Fresh := StateM Nat

def fresh : Fresh Unique := do
  let n ← get
  set (n + 1)
  pure ⟨n⟩

inductive ty : Type where
| Record : (List (Symbol × ty)) → Unique → ty
| Nil : ty
| Int : ty
| String : ty
| Array : ty → Unique → ty

def mkArray (elt : ty) : Fresh ty := do
  let u ← fresh
  pure (ty.Array elt u)

def mkRecord (fields : List (Symbol × ty)) : Fresh ty := do
  let u ← fresh
  pure (ty.Record fields u)
