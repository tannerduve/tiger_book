abbrev id' := String

inductive binop where
| Plus : binop
| Minus : binop
| Times : binop
| Div : binop
deriving Repr
open binop

def binopToString : binop → String
| Plus => " + "
| Minus => " - "
| Times => " × "
| binop.Div => " / "

instance : ToString binop where
toString := binopToString

mutual
inductive stm where
| AssignStm : id' → exp → stm
| PrintStm : List exp → stm
| CompoundStm : stm → stm → stm
deriving Repr

inductive exp where
| IdExp : id' → exp
| NumExp : Int → exp
| OpExp : exp → binop → exp → exp
| EseqExp : stm → exp → exp
deriving Repr
end

open stm exp binop

def prog : stm :=
  CompoundStm
    (AssignStm "a" (OpExp (NumExp 5) Plus (NumExp 3)))
    (CompoundStm
      (AssignStm "b"
        (EseqExp
          (PrintStm [
            IdExp "a",
            OpExp (IdExp "a") Minus (NumExp 1)
          ])
          (OpExp (NumExp 10) Times (IdExp "a"))
        )
      )
      (PrintStm [IdExp "b"]))

/--
Chapter 1 Programming Exercise 1. Write a function that
tells the maximum number of arguments of any print statement
within any subexpression of a given statement. eg. maxargs(prog) is 2.
-/
def maxargs (s : stm) : Int :=
  let rec maxarg_exp (e : exp) :=
    match e with
    | EseqExp s e₁ => max (maxargs s) (maxarg_exp e₁)
    | OpExp e₁ _ e₂ => max (maxarg_exp e₁) (maxarg_exp e₂)
    | _ => 0
  match s with
  | PrintStm l => l.length
  | AssignStm _ e => maxarg_exp e
  | CompoundStm s₁ s₂ => max (maxargs s₁) (maxargs s₂)

#eval maxargs prog

mutual
  partial def expToString : exp → String
    | exp.IdExp x => s!"IdExp {x}"
    | exp.NumExp n => s!"NumExp {n}"
    | exp.OpExp e1 op e2 => s!"OpExp ({expToString e1}) {op} ({expToString e2})"
    | exp.EseqExp s e => s!"EseqExp ({stmToString s}) ({expToString e})"

  partial def stmToString : stm → String
    | stm.AssignStm id e => s!"AssignStm {id} := {expToString e}"
    | stm.PrintStm l => s!"PrintStm [{String.intercalate ", " (l.map expToString)}]"
    | stm.CompoundStm s1 s2 => s!"CompoundStm ({stmToString s1}) ; ({stmToString s2})"
end
/--
Chapter 1 Programming Exercise 2. Write a function interp : stm → unit
that interprets a program. To write in FP style, maintain a list of variable-integer pairs
and produce new versions of this list at each AssignStm
-/

instance : ToString exp where
toString := expToString

instance : ToString stm where
toString := stmToString

abbrev table := List (id' × Int)

def update (t : table) (x : id') (z : Int) : table :=
match t with
| [] => [(x, z)]
| hd :: tl => (x, z) :: hd :: tl

def lookup (t : table) (x : id') : Int :=
match t with
| [] => panic! "s!{x} not found in table"
| hd :: tl => if (hd.1 = x) then hd.2 else lookup tl x

mutual

  def interpStm (s : stm) (t : table) : IO table := do
  match s with
  | CompoundStm s₁ s₂ => do
    let t₂ ← interpStm s₁ t
    interpStm s₂ t₂
  | AssignStm i e =>
    let (n, t') ← interpExp e t
    pure (update t' i n)
  | PrintStm l => do
    let t' ← l.foldlM (λ acc e => do
      let (v, acc') ← interpExp e acc
      IO.println v
      pure acc'
      ) t
    pure t'

  def interpExp (e : exp) (t : table) : IO (Int × table) := do
  match e with
  | IdExp i => pure (lookup t i, t)
  | NumExp n => pure (n, t)
  | OpExp e₁ op e₂ =>
    (match op with
    | Plus => do
      let (n, t') ← interpExp e₁ t
      let (n', t'') ← interpExp e₂ t'
      pure (n + n', t'')
    | Minus => do
      let (n, t') ← interpExp e₁ t
      let (n', t'') ← interpExp e₂ t'
      pure (n - n', t'')
    | Times => do
      let (n, t') ← interpExp e₁ t
      let (n', t'') ← interpExp e₂ t'
      pure (n * n', t'')
    | binop.Div => do
      let (n, t') ← interpExp e₁ t
      let (n', t'') ← interpExp e₂ t'
      pure (n / n', t''))
  | EseqExp s₁ e₁ => do
    let t₂ ← interpStm s₁ t
    interpExp e₁ t₂
end

#eval interpStm prog []

/-
Exercises

1.1. This program implements persistent functional BSTs, such that if tree₂ = insert x tree₁,
then tree₁ is available for lookups while tree₂ can be used
-/

abbrev key := String

inductive tree where
| Leaf : tree
| Tree (t₁ : tree) (k : key) (t₂ : tree) : tree
deriving Repr, Inhabited, DecidableEq, BEq

open tree

abbrev empty := Leaf

def insert (k : key) (t : tree) : tree :=
match t with
| Leaf => Tree Leaf k Leaf
| Tree l k' r =>
  if k' < k then Tree (insert k' l) k r
  else if k' > k then Tree l k (insert k' r)
  else Tree l k' r

-- 1.1a. Implement a `member` function which returns `true` if an item is found, else `false`
def member (k : key) (t : tree) : Bool :=
match t with
| Leaf => false
| Tree l k' r =>
  if k' < k then member k r
  else if k' > k then member k l
  else true

-- 1.1b. Extend the program to include not just membership, but the mapping of keys to bindings
inductive ptree (α : Type) where
| pLeaf : ptree α
| pTree (l : ptree α) (kv : key × α) (r : ptree α) : ptree α

namespace ptree

def insert (kv : key × α) (t : ptree α) : ptree α :=
match t with
| pLeaf => pTree pLeaf kv pLeaf
| pTree l kv' r =>
  if kv'.1 < kv.1 then pTree (insert kv' l) kv r
  else if kv'.1 > kv.1 then pTree l kv (insert kv' r)
  else pTree l kv' r

def member {α} [Inhabited α] (k : key) (t : ptree α) : α :=
match t with
| pLeaf => panic! "s!{k} not found in tree"
| pTree l kv r =>
  if kv.1 < k then member k r
  else if kv.1 > k then member k l
  else kv.2
