import TigerBook.Symbol

open Symbol

abbrev unique := IO.Ref Unit

inductive ty : Type where
| Record : (List (Symbol × ty)) → Unique → ty
| Nil : ty
| Int : ty
| String : ty
| Array : ty → Unique → ty
